submodule (fpm_dependency) node


contains

  !> Update dependency from project manifest
  module subroutine register(self, package, root, fetch, revision, error)
    !> Instance of the dependency node
    class(dependency_node_t), intent(inout) :: self
    !> Package configuration data
    type(package_config_t), intent(in) :: package
    !> Project has been fetched
    logical, intent(in) :: fetch
    !> Root directory of the project
    character(len=*), intent(in) :: root
    !> Git revision of the project
    character(len=*), intent(in), optional :: revision
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    logical :: update

    update = .false.
    if (self%name /= package%name) then
      call fatal_error(error, "Dependency name '"//package%name// &
        & "' found, but expected '"//self%name//"' instead")
    end if

    self%version = package%version
    self%proj_dir = root

    if (allocated(self%git) .and. present(revision)) then
      self%revision = revision
      if (.not. fetch) then
        ! Change in revision ID was checked already. Only update if ALL git information is missing
        update = .not. allocated(self%git%url)
      end if
    end if

    if (update) self%update = update
    self%done = .true.

  end subroutine register


  !> Get a dependency from the registry. Whether the dependency is fetched
  !> from a local, a custom remote or the official registry is determined
  !> by the global configuration settings.
  module subroutine get_from_registry(self, target_dir, global_settings, error, downloader_)

    !> Instance of the dependency configuration.
    class(dependency_node_t), intent(in) :: self

    !> The target directory of the dependency.
    character(:), allocatable, intent(out) :: target_dir

    !> Global configuration settings.
    type(fpm_global_settings), intent(in) :: global_settings

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    !> Downloader instance.
    class(downloader_t), optional, intent(in) :: downloader_

    character(:), allocatable :: cache_path, target_url, tmp_file
    type(version_t) :: version
    integer :: stat, unit
    type(json_object) :: json
    class(downloader_t), allocatable :: downloader

    if (present(downloader_)) then
      downloader = downloader_
    else
      allocate (downloader)
    end if

    ! Use local registry if it was specified in the global config file.
    if (allocated(global_settings%registry_settings%path)) then
      call self%get_from_local_registry(target_dir, global_settings%registry_settings%path, error); return
    end if

    ! Include namespace and package name in the cache path.
    cache_path = join_path(global_settings%registry_settings%cache_path, self%namespace, self%name)

    ! Check cache before downloading from the remote registry if a specific version was requested. When no specific
    ! version was requested, do network request first to check which is the newest version.
    if (allocated(self%requested_version)) then
      if (exists(join_path(cache_path, self%requested_version%s(), 'fpm.toml'))) then
        print *, "Using cached version of '", join_path(self%namespace, self%name, self%requested_version%s()), "'."
        target_dir = join_path(cache_path, self%requested_version%s()); return
      end if
    end if

    tmp_file = get_temp_filename()
    open (newunit=unit, file=tmp_file, action='readwrite', iostat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Error creating temporary file for downloading package '"//self%name//"'."); return
    end if

    ! Include namespace and package name in the target url and download package data.
    target_url = global_settings%registry_settings%url//'packages/'//self%namespace//'/'//self%name
    call downloader%get_pkg_data(target_url, self%requested_version, tmp_file, json, error)
    close (unit, status='delete')
    if (allocated(error)) return

    ! Verify package data and read relevant information.
    call check_and_read_pkg_data(json, self, target_url, version, error)
    if (allocated(error)) return

    ! Open new tmp file for downloading the actual package.
    open (newunit=unit, file=tmp_file, action='readwrite', iostat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Error creating temporary file for downloading package '"//self%name//"'."); return
    end if

    ! Include version number in the cache path. If no cached version exists, download it.
    cache_path = join_path(cache_path, version%s())
    if (.not. exists(join_path(cache_path, 'fpm.toml'))) then
      if (is_dir(cache_path)) call os_delete_dir(os_is_unix(), cache_path)
      call mkdir(cache_path)

      call downloader%get_file(target_url, tmp_file, error)
      if (allocated(error)) then
        close (unit, status='delete'); return
      end if

      ! Unpack the downloaded package to the final location.
      call downloader%unpack(tmp_file, cache_path, error)
      close (unit, status='delete')
      if (allocated(error)) return
    end if

    target_dir = cache_path

  end subroutine get_from_registry



  !> Get the dependency from a local registry.
  module subroutine get_from_local_registry(self, target_dir, registry_path, error)

    !> Instance of the dependency configuration.
    class(dependency_node_t), intent(in) :: self

    !> The target directory to download the dependency to.
    character(:), allocatable, intent(out) :: target_dir

    !> The path to the local registry.
    character(*), intent(in) :: registry_path

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: path_to_name
    type(string_t), allocatable :: files(:)
    type(version_t), allocatable :: versions(:)
    type(version_t) :: version
    integer :: i

    path_to_name = join_path(registry_path, self%namespace, self%name)

    if (.not. exists(path_to_name)) then
      call fatal_error(error, "Dependency resolution of '"//self%name// &
      & "': Directory '"//path_to_name//"' doesn't exist."); return
    end if

    call list_files(path_to_name, files)
    if (size(files) == 0) then
      call fatal_error(error, "No versions of '"//self%name//"' found in '"//path_to_name//"'."); return
    end if

    ! Version requested, find it in the cache.
    if (allocated(self%requested_version)) then
      do i = 1, size(files)
        ! Identify directory that matches the version number.
        if (files(i)%s == join_path(path_to_name, self%requested_version%s()) .and. is_dir(files(i)%s)) then
          if (.not. exists(join_path(files(i)%s, 'fpm.toml'))) then
            call fatal_error(error, "'"//files(i)%s//"' is missing an 'fpm.toml' file."); return
          end if
          target_dir = files(i)%s; return
        end if
      end do
      call fatal_error(error, "Version '"//self%requested_version%s()//"' not found in '"//path_to_name//"'")
      return
    end if

    ! No specific version requested, therefore collect available versions.
    allocate (versions(0))
    do i = 1, size(files)
      if (is_dir(files(i)%s)) then
        call new_version(version, basename(files(i)%s), error)
        if (allocated(error)) return
        versions = [versions, version]
      end if
    end do

    if (size(versions) == 0) then
      call fatal_error(error, "No versions found in '"//path_to_name//"'"); return
    end if

    ! Find the latest version.
    version = versions(1)
    do i = 1, size(versions)
      if (versions(i) > version) version = versions(i)
    end do

    path_to_name = join_path(path_to_name, version%s())

    if (.not. exists(join_path(path_to_name, 'fpm.toml'))) then
      call fatal_error(error, "'"//path_to_name//"' is missing an 'fpm.toml' file."); return
    end if

    target_dir = path_to_name
  end subroutine get_from_local_registry


  !> Write information on instance
  module subroutine node_info(self, unit, verbosity)

    !> Instance of the dependency configuration
    class(dependency_node_t), intent(in) :: self

    !> Unit for IO
    integer, intent(in) :: unit

    !> Verbosity of the printout
    integer, intent(in), optional :: verbosity

    integer :: pr
    character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

    if (present(verbosity)) then
      pr = verbosity
    else
      pr = 1
    end if

    !> Call base object info
    call self%dependency_config_t%info(unit, pr)

    if (allocated(self%version)) then
      write (unit, fmt) "- version", self%version%s()
    end if

    if (allocated(self%proj_dir)) then
      write (unit, fmt) "- dir", self%proj_dir
    end if

    if (allocated(self%revision)) then
      write (unit, fmt) "- revision", self%revision
    end if

    write (unit, fmt) "- done", merge('YES', 'NO ', self%done)
    write (unit, fmt) "- update", merge('YES', 'NO ', self%update)

  end subroutine node_info

  !> Check that two dependency nodes are equal
  module function node_is_same(this,that)
      class(dependency_node_t), intent(in) :: this
      class(serializable_t), intent(in) :: that
      logical :: node_is_same

      node_is_same = .false.

      select type (other=>that)
         type is (dependency_node_t)

            ! Base class must match
            if (.not.(this%dependency_config_t==other%dependency_config_t)) return

            ! Extension must match
            if (.not.(this%done  .eqv.other%done)) return
            if (.not.(this%update.eqv.other%update)) return
            if (.not.(this%cached.eqv.other%cached)) return
            if (.not.(this%proj_dir==other%proj_dir)) return
            if (.not.(this%revision==other%revision)) return

            if (.not.(allocated(this%version).eqv.allocated(other%version))) return
               if (allocated(this%version)) then
              if (.not.(this%version==other%version)) return
            endif

         class default
            ! Not the same type
            return
      end select

      !> All checks passed!
      node_is_same = .true.

  end function node_is_same


    !> Dump dependency to toml table
    module subroutine node_dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_node_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: ierr

        ! Dump parent class
        call self%dependency_config_t%dump_to_toml(table, error)
        if (allocated(error)) return

        if (allocated(self%version)) then
            call set_string(table, "version", self%version%s(), error,'dependency_node_t')
            if (allocated(error)) return
        endif
        call set_string(table, "proj-dir", self%proj_dir, error, 'dependency_node_t')
        if (allocated(error)) return
        call set_string(table, "revision", self%revision, error, 'dependency_node_t')
        if (allocated(error)) return
        call set_value(table, "done", self%done, error, 'dependency_node_t')
        if (allocated(error)) return
        call set_value(table, "update", self%update, error, 'dependency_node_t')
        if (allocated(error)) return
        call set_value(table, "cached", self%cached, error, 'dependency_node_t')
        if (allocated(error)) return

    end subroutine node_dump_to_toml

    !> Read dependency from toml table (no checks made at this stage)
    module subroutine node_load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_node_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        character(len=:), allocatable :: version
        integer :: ierr

        call destroy_dependency_node(self)

        ! Load parent class
        call self%dependency_config_t%load_from_toml(table, error)
        if (allocated(error)) return

        call get_value(table, "done", self%done, error, 'dependency_node_t')
        if (allocated(error)) return
        call get_value(table, "update", self%update, error, 'dependency_node_t')
        if (allocated(error)) return
        call get_value(table, "cached", self%cached, error, 'dependency_node_t')
        if (allocated(error)) return

        call get_value(table, "proj-dir", self%proj_dir)
        call get_value(table, "revision", self%revision)

        call get_value(table, "version", version)
        if (allocated(version)) then
            allocate(self%version)
            call new_version(self%version, version, error)
            if (allocated(error)) then
                error%message = 'dependency_node_t: version error from TOML table - '//error%message
                return
            endif
        end if

    end subroutine node_load_from_toml

end submodule node