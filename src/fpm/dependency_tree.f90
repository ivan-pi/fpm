submodule (fpm_dependency) tree

  !> Common output format for writing to the command line
  character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

contains

  !> Add project dependencies, each depth level after each other.
  !>
  !> We implement this algorithm in an interative rather than a recursive fashion
  !> as a choice of design.
  module subroutine add_project(self, package, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Project configuration to add
    type(package_config_t), intent(in) :: package
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(dependency_config_t) :: dependency
    type(dependency_tree_t) :: cached
    character(len=*), parameter :: root = '.'
    integer :: id

    if (.not. exists(self%dep_dir)) then
      call mkdir(self%dep_dir)
    end if

    ! Create this project as the first dependency node (depth 0)
    dependency%name = package%name
    dependency%path = root
    call self%add(dependency, error)
    if (allocated(error)) return

    ! Resolve the root project
    call self%resolve(root, error)
    if (allocated(error)) return

    ! Add the root project dependencies (depth 1)
    call self%add(package, root, .true., error)
    if (allocated(error)) return

    ! After resolving all dependencies, check if we have cached ones to avoid updates
    if (allocated(self%cache)) then
      call new_dependency_tree(cached, verbosity=self%verbosity,cache=self%cache)
      call cached%load_cache(self%cache, error)
      if (allocated(error)) return

      ! Skip root node
      do id = 2, cached%ndep
        cached%dep(id)%cached = .true.
        call self%add(cached%dep(id), error)
        if (allocated(error)) return
      end do
    end if

    ! Now decent into the dependency tree, level for level
    do while (.not. self%finished())
      call self%resolve(root, error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

    if (allocated(self%cache)) then
      call self%dump_cache(self%cache, error)
      if (allocated(error)) return
    end if

  end subroutine add_project

  !> Add a project and its dependencies to the dependency tree
  recursive module subroutine add_project_dependencies(self, package, root, main, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Project configuration to add
    type(package_config_t), intent(in) :: package
    !> Current project root directory
    character(len=*), intent(in) :: root
    !> Is the main project
    logical, intent(in) :: main
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii

    if (allocated(package%dependency)) then
      call self%add(package%dependency, error)
      if (allocated(error)) return
    end if

    if (main) then
      if (allocated(package%dev_dependency)) then
        call self%add(package%dev_dependency, error)
        if (allocated(error)) return
      end if

      if (allocated(package%executable)) then
        do ii = 1, size(package%executable)
          if (allocated(package%executable(ii)%dependency)) then
            call self%add(package%executable(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if

      if (allocated(package%example)) then
        do ii = 1, size(package%example)
          if (allocated(package%example(ii)%dependency)) then
            call self%add(package%example(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if

      if (allocated(package%test)) then
        do ii = 1, size(package%test)
          if (allocated(package%test(ii)%dependency)) then
            call self%add(package%test(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if
    end if

    !> Ensure allocation fits
    call resize(self%dep,self%ndep)

  end subroutine add_project_dependencies

  !> Add a list of dependencies to the dependency tree
  module subroutine add_dependencies(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_config_t), intent(in) :: dependency(:)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii, ndep

    ndep = size(self%dep)
    if (ndep < size(dependency) + self%ndep) then
      call resize(self%dep, ndep + ndep/2 + size(dependency))
    end if

    do ii = 1, size(dependency)
      call self%add(dependency(ii), error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

    !> Ensure allocation fits ndep
    call resize(self%dep,self%ndep)

  end subroutine add_dependencies

  !> Add a single dependency node to the dependency tree
  !> Dependency nodes contain additional information (version, git, revision)
  module subroutine add_dependency_node(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_node_t), intent(in) :: dependency
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: id

    if (self%has_dependency(dependency)) then
      ! A dependency with this same name is already in the dependency tree.
      ! Check if it needs to be updated
      id = self%find(dependency%name)

      ! If this dependency was in the cache, and we're now requesting a different version
      ! in the manifest, ensure it is marked for update. Otherwise, if we're just querying
      ! the same dependency from a lower branch of the dependency tree, the existing one from
      ! the manifest has priority
      if (dependency%cached) then
        if (dependency_has_changed(dependency, self%dep(id), self%verbosity, self%unit)) then
          if (self%verbosity > 0) write (self%unit, out_fmt) "Dependency change detected:", dependency%name
          self%dep(id)%update = .true.
        else
          ! Store the cached one
          self%dep(id) = dependency
          self%dep(id)%update = .false.
        end if
      end if
    else

      !> Safety: reallocate if necessary
      if (size(self%dep)==self%ndep) call resize(self%dep,self%ndep+1)

      ! New dependency: add from scratch
      self%ndep = self%ndep + 1
      self%dep(self%ndep) = dependency
      self%dep(self%ndep)%update = .false.
    end if

  end subroutine add_dependency_node

  !> Add a single dependency to the dependency tree
  module subroutine add_dependency(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_config_t), intent(in) :: dependency
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(dependency_node_t) :: node

    call new_dependency_node(node, dependency)
    call add_dependency_node(self, node, error)

  end subroutine add_dependency


  !> Resolve all dependencies in the tree
  module subroutine resolve_dependencies(self, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(fpm_global_settings) :: global_settings
    integer :: ii

    call get_global_settings(global_settings, error)
    if (allocated(error)) return

    do ii = 1, self%ndep
      call self%resolve(self%dep(ii), global_settings, root, error)
      if (allocated(error)) exit
    end do

    if (allocated(error)) return

  end subroutine resolve_dependencies

  !> Resolve a single dependency node
  module subroutine resolve_dependency(self, dependency, global_settings, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_node_t), intent(inout) :: dependency
    !> Global configuration settings.
    type(fpm_global_settings), intent(in) :: global_settings
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(package_config_t) :: package
    character(len=:), allocatable :: manifest, proj_dir, revision
    logical :: fetch

    if (dependency%done) return

    fetch = .false.
    if (allocated(dependency%proj_dir)) then
      proj_dir = dependency%proj_dir
    else if (allocated(dependency%path)) then
      proj_dir = join_path(root, dependency%path)
    else if (allocated(dependency%git)) then
      proj_dir = join_path(self%dep_dir, dependency%name)
      fetch = .not. exists(proj_dir)
      if (fetch) then
        call dependency%git%checkout(proj_dir, error)
        if (allocated(error)) return
      end if
    else
      call dependency%get_from_registry(proj_dir, global_settings, error)
      if (allocated(error)) return
    end if

    if (allocated(dependency%git)) then
      call git_revision(proj_dir, revision, error)
      if (allocated(error)) return
    end if

    manifest = join_path(proj_dir, "fpm.toml")
    call get_package_data(package, manifest, error)
    if (allocated(error)) return

    call dependency%register(package, proj_dir, fetch, revision, error)
    if (allocated(error)) return

    if (self%verbosity > 1) then
      write (self%unit, out_fmt) &
        "Dep:", dependency%name, "version", dependency%version%s(), &
        "at", dependency%proj_dir
    end if

    call self%add(package, proj_dir, .false., error)
    if (allocated(error)) return

  end subroutine resolve_dependency


  !> True if dependency is part of the tree
  pure module function has_dependency(self, dependency)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> Dependency configuration to check
    class(dependency_node_t), intent(in) :: dependency
    logical :: has_dependency

    has_dependency = find_name(self,dependency%name) /= 0

  end function has_dependency

  !> Find a dependency in the dependency tree
  pure module function find_name(self, name) result(pos)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> Dependency configuration to add
    character(len=*), intent(in) :: name
    !> Index of the dependency
    integer :: pos

    integer :: ii

    pos = 0
    do ii = 1, self%ndep
      if (name == self%dep(ii)%name) then
        pos = ii
        exit
      end if
    end do

  end function find_name

  !> Check if we are done with the dependency resolution
  pure module function finished(self)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> All dependencies are updated
    logical :: finished

    finished = all(self%dep(:self%ndep)%done)

  end function finished


  !> Read dependency tree from file
  module subroutine load_cache_from_file(self, file, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: unit
    logical :: exist

    inquire (file=file, exist=exist)
    if (.not. exist) return

    open (file=file, newunit=unit)
    call self%load_cache(unit, error)
    close (unit)
  end subroutine load_cache_from_file

  !> Read dependency tree from file
  module subroutine load_cache_from_unit(self, unit, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    integer, intent(in) :: unit
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_error), allocatable :: parse_error
    type(toml_table), allocatable :: table

    call toml_load(table, unit, error=parse_error)

    if (allocated(parse_error)) then
      allocate (error)
      call move_alloc(parse_error%message, error%message)
      return
    end if

    call self%load_cache(table, error)
    if (allocated(error)) return

  end subroutine load_cache_from_unit

  !> Read dependency tree from TOML data structure
  module subroutine load_cache_from_toml(self, table, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ndep, ii
    logical :: is_unix
    character(len=:), allocatable :: version, url, obj, rev, proj_dir
    type(toml_key), allocatable :: list(:)
    type(toml_table), pointer :: ptr

    call table%get_keys(list)

    ndep = size(self%dep)
    if (ndep < size(list) + self%ndep) then
      call resize(self%dep, ndep + ndep/2 + size(list))
    end if

    is_unix = get_os_type() /= OS_WINDOWS

    do ii = 1, size(list)
      call get_value(table, list(ii)%key, ptr)
      call get_value(ptr, "version", version)
      call get_value(ptr, "proj-dir", proj_dir)
      call get_value(ptr, "git", url)
      call get_value(ptr, "obj", obj)
      call get_value(ptr, "rev", rev)
      if (.not. allocated(proj_dir)) cycle
      self%ndep = self%ndep + 1
      associate (dep => self%dep(self%ndep))
        dep%name = list(ii)%key
        if (is_unix) then
          dep%proj_dir = proj_dir
        else
          dep%proj_dir = windows_path(proj_dir)
        end if
        dep%done = .false.
        if (allocated(version)) then
          if (.not. allocated(dep%version)) allocate (dep%version)
          call new_version(dep%version, version, error)
          if (allocated(error)) exit
        end if
        if (allocated(url)) then
          if (allocated(obj)) then
            dep%git = git_target_revision(url, obj)
          else
            dep%git = git_target_default(url)
          end if
          if (allocated(rev)) then
            dep%revision = rev
          end if
        else
          dep%path = proj_dir
        end if
      end associate
    end do
    if (allocated(error)) return

    self%ndep = size(list)
  end subroutine load_cache_from_toml


  !> Write dependency tree to file
  module subroutine dump_cache_to_file(self, file, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: unit

    open (file=file, newunit=unit)
    call self%dump_cache(unit, error)
    close (unit)
    if (allocated(error)) return

  end subroutine dump_cache_to_file

  !> Write dependency tree to file
  module subroutine dump_cache_to_unit(self, unit, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Formatted unit
    integer, intent(in) :: unit
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table

    table = toml_table()
    call self%dump_cache(table, error)

    write (unit, '(a)') toml_serialize(table)

  end subroutine dump_cache_to_unit

  !> Write dependency tree to TOML datastructure
  module subroutine dump_cache_to_toml(self, table, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii
    type(toml_table), pointer :: ptr
    character(len=:), allocatable :: proj_dir

    do ii = 1, self%ndep
      associate (dep => self%dep(ii))
        call add_table(table, dep%name, ptr)
        if (.not. associated(ptr)) then
          call fatal_error(error, "Cannot create entry for "//dep%name)
          exit
        end if
        if (allocated(dep%version)) then
          call set_value(ptr, "version", dep%version%s())
        end if
        proj_dir = canon_path(dep%proj_dir)
        call set_value(ptr, "proj-dir", proj_dir)
        if (allocated(dep%git)) then
          call set_value(ptr, "git", dep%git%url)
          if (allocated(dep%git%object)) then
            call set_value(ptr, "obj", dep%git%object)
          end if
          if (allocated(dep%revision)) then
            call set_value(ptr, "rev", dep%revision)
          end if
        end if
      end associate
    end do
    if (allocated(error)) return

  end subroutine dump_cache_to_toml


  !> Check if a dependency node has changed
  logical function dependency_has_changed(cached, manifest, verbosity, iunit) result(has_changed)
    !> Two instances of the same dependency to be compared
    type(dependency_node_t), intent(in) :: cached, manifest

    !> Log verbosity
    integer, intent(in) :: verbosity, iunit

    integer :: ip

    has_changed = .true.

    !> All the following entities must be equal for the dependency to not have changed
    if (manifest_has_changed(cached=cached, manifest=manifest, verbosity=verbosity, iunit=iunit)) return

    !> For now, only perform the following checks if both are available. A dependency in cache.toml
    !> will always have this metadata; a dependency from fpm.toml which has not been fetched yet
    !> may not have it
    if (allocated(cached%version) .and. allocated(manifest%version)) then
      if (cached%version /= manifest%version) then
        if (verbosity > 1) write (iunit, out_fmt) "VERSION has changed: "//cached%version%s()//" vs. "//manifest%version%s()
        return
      end if
    else
      if (verbosity > 1) write (iunit, out_fmt) "VERSION has changed presence "
    end if
    if (allocated(cached%revision) .and. allocated(manifest%revision)) then
      if (cached%revision /= manifest%revision) then
        if (verbosity > 1) write (iunit, out_fmt) "REVISION has changed: "//cached%revision//" vs. "//manifest%revision
        return
      end if
    else
      if (verbosity > 1) write (iunit, out_fmt) "REVISION has changed presence "
    end if
    if (allocated(cached%proj_dir) .and. allocated(manifest%proj_dir)) then
      if (cached%proj_dir /= manifest%proj_dir) then
        if (verbosity > 1) write (iunit, out_fmt) "PROJECT DIR has changed: "//cached%proj_dir//" vs. "//manifest%proj_dir
        return
      end if
    else
      if (verbosity > 1) write (iunit, out_fmt) "PROJECT DIR has changed presence "
    end if
    if (allocated(cached%preprocess) .eqv. allocated(manifest%preprocess)) then
      if (allocated(cached%preprocess)) then
          if (size(cached%preprocess) /= size(manifest%preprocess)) then
            if (verbosity > 1) write (iunit, out_fmt) "PREPROCESS has changed size"
            return
          end if
          do ip=1,size(cached%preprocess)
             if (.not.(cached%preprocess(ip) == manifest%preprocess(ip))) then
                if (verbosity > 1) write (iunit, out_fmt) "PREPROCESS config has changed"
                return
             end if
          end do
      endif
    else
      if (verbosity > 1) write (iunit, out_fmt) "PREPROCESS has changed presence "
      return
    end if

    !> All checks passed: the two dependencies have no differences
    has_changed = .false.

  end function dependency_has_changed



  !> Check that two dependency trees are equal
  module function dependency_tree_is_same(this,that)
    class(dependency_tree_t), intent(in) :: this
    class(serializable_t), intent(in) :: that
    logical :: dependency_tree_is_same

    integer :: ii

    dependency_tree_is_same = .false.

    select type (other=>that)
       type is (dependency_tree_t)

          if (.not.(this%unit==other%unit)) return
          if (.not.(this%verbosity==other%verbosity)) return
          if (.not.(this%dep_dir==other%dep_dir)) return
          if (.not.(this%ndep==other%ndep)) return
          if (.not.(allocated(this%dep).eqv.allocated(other%dep))) return
          if (allocated(this%dep)) then
             if (.not.(size(this%dep)==size(other%dep))) return
             do ii = 1, size(this%dep)
                if (.not.(this%dep(ii)==other%dep(ii))) return
             end do
          endif
          if (.not.(this%cache==other%cache)) return

       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    dependency_tree_is_same = .true.

   end function dependency_tree_is_same

   !> Dump dependency to toml table
   module subroutine tree_dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_tree_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: ierr, ii
        type(toml_table), pointer :: ptr_deps,ptr
        character(27) :: unnamed

        call set_value(table, "unit", self%unit, error, 'dependency_tree_t')
        if (allocated(error)) return
        call set_value(table, "verbosity", self%verbosity, error, 'dependency_tree_t')
        if (allocated(error)) return
        call set_string(table, "dep-dir", self%dep_dir, error, 'dependency_tree_t')
        if (allocated(error)) return
        call set_string(table, "cache", self%cache, error, 'dependency_tree_t')
        if (allocated(error)) return
        call set_value(table, "ndep", self%ndep, error, 'dependency_tree_t')
        if (allocated(error)) return

        if (allocated(self%dep)) then

           ! Create dependency table
           call add_table(table, "dependencies", ptr_deps)
           if (.not. associated(ptr_deps)) then
              call fatal_error(error, "dependency_tree_t cannot create dependency table ")
              return
           end if

           do ii = 1, size(self%dep)
              associate (dep => self%dep(ii))

                 !> Because dependencies are named, fallback if this has no name
                 !> So, serialization will work regardless of size(self%dep) == self%ndep
                 if (len_trim(dep%name)==0) then
                    write(unnamed,1) ii
                    call add_table(ptr_deps, trim(unnamed), ptr)
                 else
                    call add_table(ptr_deps, dep%name, ptr)
                 end if
                 if (.not. associated(ptr)) then
                    call fatal_error(error, "dependency_tree_t cannot create entry for dependency "//dep%name)
                    return
                 end if
                 call dep%dump_to_toml(ptr, error)
                 if (allocated(error)) return
              end associate
           end do

        endif

        1 format('UNNAMED_DEPENDENCY_',i0)

    end subroutine tree_dump_to_toml

    !> Read dependency from toml table (no checks made at this stage)
    module subroutine tree_load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_tree_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        type(toml_key), allocatable :: keys(:),dep_keys(:)
        type(toml_table), pointer :: ptr_deps,ptr
        integer :: ii, jj, ierr

        call table%get_keys(keys)

        call get_value(table, "unit", self%unit, error, 'dependency_tree_t')
        if (allocated(error)) return
        call get_value(table, "verbosity", self%verbosity, error, 'dependency_tree_t')
        if (allocated(error)) return
        call get_value(table, "ndep", self%ndep, error, 'dependency_tree_t')
        if (allocated(error)) return
        call get_value(table, "dep-dir", self%dep_dir)
        call get_value(table, "cache", self%cache)

        find_deps_table: do ii = 1, size(keys)
            if (keys(ii)%key=="dependencies") then

               call get_value(table, keys(ii), ptr_deps)
               if (.not.associated(ptr_deps)) then
                  call fatal_error(error,'dependency_tree_t: error retrieving dependency table from TOML table')
                  return
               end if

               !> Read all dependencies
               call ptr_deps%get_keys(dep_keys)
               call resize(self%dep, size(dep_keys))

               do jj = 1, size(dep_keys)

                   call get_value(ptr_deps, dep_keys(jj), ptr)
                   call self%dep(jj)%load_from_toml(ptr, error)
                   if (allocated(error)) return

               end do

               exit find_deps_table

            endif
        end do find_deps_table

    end subroutine tree_load_from_toml

end submodule