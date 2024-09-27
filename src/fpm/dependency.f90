!> # Dependency management
!>
!> ## Fetching dependencies and creating a dependency tree
!>
!> Dependencies on the top-level can be specified from:
!>
!> - `package%dependencies`
!> - `package%dev_dependencies`
!> - `package%executable(:)%dependencies`
!> - `package%test(:)%dependencies`
!>
!> Each dependency is fetched in some way and provides a path to its package
!> manifest.
!> The `package%dependencies` of the dependencies are resolved recursively.
!>
!> To initialize the dependency tree all dependencies are recursively fetched
!> and stored in a flat data structure to avoid retrieving a package twice.
!> The data structure used to store this information should describe the current
!> status of the dependency tree. Important information are:
!>
!> - name of the package
!> - version of the package
!> - path to the package root
!>
!> Additionally, for version controlled dependencies the following should be
!> stored along with the package:
!>
!> - the upstream url
!> - the current checked out revision
!>
!> Fetching a remote (version controlled) dependency turns it for our purpose
!> into a local path dependency which is handled by the same means.
!>
!> ## Updating dependencies
!>
!> For a given dependency tree all top-level dependencies can be updated.
!> We have two cases to consider, a remote dependency and a local dependency,
!> again, remote dependencies turn into local dependencies by fetching.
!> Therefore we will update remote dependencies by simply refetching them.
!>
!> For remote dependencies we have to refetch if the revision in the manifest
!> changes or the upstream HEAD has changed (for branches _and_ tags).
!>
!> @Note For our purpose a tag is just a fancy branch name. Tags can be delete and
!>       modified afterwards, therefore they do not differ too much from branches
!>       from our perspective.
!>
!> For the latter case we only know if we actually fetch from the upstream URL.
!>
!> In case of local (and fetched remote) dependencies we have to read the package
!> manifest and compare its dependencies against our dependency tree, any change
!> requires updating the respective dependencies as well.
!>
!> ## Handling dependency compatibilties
!>
!> Currenly ignored. First come, first serve.
module fpm_dependency
  use, intrinsic :: iso_fortran_env, only: output_unit
  use fpm_environment, only: get_os_type, OS_WINDOWS, os_is_unix
  use fpm_error, only: error_t, fatal_error
  use fpm_filesystem, only: exists, join_path, mkdir, canon_path, windows_path, list_files, is_dir, basename, &
                            os_delete_dir, get_temp_filename
  use fpm_git, only: git_target_revision, git_target_default, git_revision, serializable_t
  
  use fpm_manifest, only: package_config_t, dependency_config_t, get_package_data, &
    manifest_has_changed

!  use fpm_manifest_dependency, only: manifest_has_changed, dependency_destroy
!  use fpm_manifest_preprocess, only: operator(==)

  use fpm_strings, only: string_t, operator(.in.)
  use fpm_toml, only: toml_table, toml_key, toml_error, toml_serialize, &
                      get_value, set_value, add_table, toml_load, toml_stat, set_string
  use fpm_versioning, only: version_t, new_version
  use fpm_settings, only: fpm_global_settings, get_global_settings, official_registry_base_url
  use fpm_downloader, only: downloader_t
  use jonquil, only: json_object
  use fpm_strings, only: str

  implicit none
  private

  public :: dependency_tree_t, new_dependency_tree, dependency_node_t, new_dependency_node, resize, &
            & check_and_read_pkg_data, destroy_dependency_node

  public :: dependency_destroy

  !> Overloaded reallocation interface
  interface resize
    module procedure :: resize_dependency_node
  end interface resize

  !> Dependency node in the projects dependency tree
  type, extends(dependency_config_t) :: dependency_node_t
    !> Actual version of this dependency
    type(version_t), allocatable :: version
    !> Installation prefix of this dependencies
    character(len=:), allocatable :: proj_dir
    !> Checked out revision of the version control system
    character(len=:), allocatable :: revision
    !> Dependency is handled
    logical :: done = .false.
    !> Dependency should be updated
    logical :: update = .false.
    !> Dependency was loaded from a cache
    logical :: cached = .false.
  contains
    !> Update dependency from project manifest.
    procedure :: register
    
    !> Get dependency from the registry.
    procedure :: get_from_registry
    procedure, private :: get_from_local_registry
    
    !> Print information on this instance
    procedure :: info => node_info

    !> Serialization interface
    procedure :: serializable_is_same => node_is_same
    procedure :: dump_to_toml => node_dump_to_toml
    procedure :: load_from_toml => node_load_from_toml

  end type dependency_node_t


  interface
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
    end subroutine
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
    end subroutine
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
    end subroutine
    !> Write information on instance
    module subroutine node_info(self, unit, verbosity)
      !> Instance of the dependency configuration
      class(dependency_node_t), intent(in) :: self
      !> Unit for IO
      integer, intent(in) :: unit
      !> Verbosity of the printout
      integer, intent(in), optional :: verbosity
    end subroutine
    !> Check that two dependency nodes are equal
    module logical function node_is_same(this,that)
      class(dependency_node_t), intent(in) :: this
      class(serializable_t), intent(in) :: that
    end function
    !> Dump dependency to toml table
    module subroutine node_dump_to_toml(self, table, error)
      !> Instance of the serializable object
      class(dependency_node_t), intent(inout) :: self
      !> Data structure
      type(toml_table), intent(inout) :: table
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Read dependency from toml table (no checks made at this stage)
    module subroutine node_load_from_toml(self, table, error)
      !> Instance of the serializable object
      class(dependency_node_t), intent(inout) :: self
      !> Data structure
      type(toml_table), intent(inout) :: table
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
  end interface


  !> Representation of a projects dependencies
  !>
  !> The dependencies are stored in a simple array for now, this can be replaced
  !> with a binary-search tree or a hash table in the future.
  type, extends(serializable_t) :: dependency_tree_t
    !> Unit for IO
    integer :: unit = output_unit
    !> Verbosity of printout
    integer :: verbosity = 1
    !> Installation prefix for dependencies
    character(len=:), allocatable :: dep_dir
    !> Number of currently registered dependencies
    integer :: ndep = 0
    !> Flattened list of all dependencies
    type(dependency_node_t), allocatable :: dep(:)
    !> Cache file
    character(len=:), allocatable :: cache

  contains

    !> Overload procedure to add new dependencies to the tree
    generic :: add => add_project, add_project_dependencies, add_dependencies, &
      add_dependency, add_dependency_node
    !> Main entry point to add a project
    procedure, private :: add_project
    !> Add a project and its dependencies to the dependency tree
    procedure, private :: add_project_dependencies
    !> Add a list of dependencies to the dependency tree
    procedure, private :: add_dependencies
    !> Add a single dependency to the dependency tree
    procedure, private :: add_dependency
    !> Add a single dependency node to the dependency tree
    procedure, private :: add_dependency_node
    !> Resolve dependencies
    generic :: resolve => resolve_dependencies, resolve_dependency
    !> Resolve dependencies
    procedure, private :: resolve_dependencies
    !> Resolve dependency
    procedure, private :: resolve_dependency
    !> True if entity can be found
    generic :: has => has_dependency
    !> True if dependency is part of the tree
    procedure, private :: has_dependency
    !> Find a dependency in the tree
    generic :: find => find_name
    !> Find a dependency by its name
    procedure, private :: find_name
    !> Depedendncy resolution finished
    procedure :: finished
    !> Reading of dependency tree
    generic :: load_cache => load_cache_from_file, load_cache_from_unit, load_cache_from_toml
    !> Read dependency tree from file
    procedure, private :: load_cache_from_file
    !> Read dependency tree from formatted unit
    procedure, private :: load_cache_from_unit
    !> Read dependency tree from TOML data structure
    procedure, private :: load_cache_from_toml
    !> Writing of dependency tree
    generic :: dump_cache => dump_cache_to_file, dump_cache_to_unit, dump_cache_to_toml
    !> Write dependency tree to file
    procedure, private :: dump_cache_to_file
    !> Write dependency tree to formatted unit
    procedure, private :: dump_cache_to_unit
    !> Write dependency tree to TOML data structure
    procedure, private :: dump_cache_to_toml
    !> Update dependency tree
    generic :: update => update_dependency, update_tree
    !> Update a list of dependencies
    procedure, private :: update_dependency
    !> Update all dependencies in the tree
    procedure, private :: update_tree

    !> Serialization interface
    procedure :: serializable_is_same => dependency_tree_is_same
    procedure :: dump_to_toml   => tree_dump_to_toml
    procedure :: load_from_toml => tree_load_from_toml

  end type dependency_tree_t


  interface
    module subroutine add_project(self, package, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Project configuration to add
      type(package_config_t), intent(in) :: package
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
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
    end subroutine
    !> Add a list of dependencies to the dependency tree
    module subroutine add_dependencies(self, dependency, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Dependency configuration to add
      type(dependency_config_t), intent(in) :: dependency(:)
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Add a single dependency node to the dependency tree
    !> Dependency nodes contain additional information (version, git, revision)
    module subroutine add_dependency_node(self, dependency, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Dependency configuration to add
      type(dependency_node_t), intent(in) :: dependency
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Add a single dependency to the dependency tree
    module subroutine add_dependency(self, dependency, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Dependency configuration to add
      type(dependency_config_t), intent(in) :: dependency
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
  end interface


  interface
    !> Resolve all dependencies in the tree
    module subroutine resolve_dependencies(self, root, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Current installation prefix
      character(len=*), intent(in) :: root
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
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
    end subroutine

    !> True if dependency is part of the tree
    pure module function has_dependency(self, dependency)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(in) :: self
      !> Dependency configuration to check
      class(dependency_node_t), intent(in) :: dependency
      logical :: has_dependency
    end function

    !> Find a dependency in the dependency tree
    pure module function find_name(self, name) result(pos)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(in) :: self
      !> Dependency configuration to add
      character(len=*), intent(in) :: name
      !> Index of the dependency
      integer :: pos
    end function

    !> Check if we are done with the dependency resolution
    pure module function finished(self)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(in) :: self
      !> All dependencies are updated
      logical :: finished
    end function
  end interface


  interface
    !> Read dependency tree from file
    module subroutine load_cache_from_file(self, file, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> File name
      character(len=*), intent(in) :: file
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Read dependency tree from file
    module subroutine load_cache_from_unit(self, unit, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> File name
      integer, intent(in) :: unit
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Read dependency tree from TOML data structure
    module subroutine load_cache_from_toml(self, table, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Data structure
      type(toml_table), intent(inout) :: table
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine

    !> Write dependency tree to file
    module subroutine dump_cache_to_file(self, file, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> File name
      character(len=*), intent(in) :: file
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Write dependency tree to file
    module subroutine dump_cache_to_unit(self, unit, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Formatted unit
      integer, intent(in) :: unit
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Write dependency tree to TOML datastructure
    module subroutine dump_cache_to_toml(self, table, error)
      !> Instance of the dependency tree
      class(dependency_tree_t), intent(inout) :: self
      !> Data structure
      type(toml_table), intent(inout) :: table
      !> Error handling
      type(error_t), allocatable, intent(out) :: error
    end subroutine
  end interface



  interface
    !> Check that two dependency trees are equal
    module function dependency_tree_is_same(this,that)
        class(dependency_tree_t), intent(in) :: this
        class(serializable_t), intent(in) :: that
        logical :: dependency_tree_is_same
    end function
    !> Dump dependency to toml table
    module subroutine tree_dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_tree_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Read dependency from toml table (no checks made at this stage)
    module subroutine tree_load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(dependency_tree_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
  end interface


  !> Common output format for writing to the command line
  character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

contains

    !> Clean memory
    elemental subroutine dependency_destroy(self)
        class(dependency_config_t), intent(inout) :: self

        if (allocated(self%name)) deallocate(self%name)
        if (allocated(self%path)) deallocate(self%path)
        if (allocated(self%namespace)) deallocate(self%namespace)
        if (allocated(self%requested_version)) deallocate(self%requested_version)
        if (allocated(self%git)) deallocate(self%git)

    end subroutine dependency_destroy


  !> Create a new dependency tree
  subroutine new_dependency_tree(self, verbosity, cache)
    !> Instance of the dependency tree
    type(dependency_tree_t), intent(out) :: self
    !> Verbosity of printout
    integer, intent(in), optional :: verbosity
    !> Name of the cache file
    character(len=*), intent(in), optional :: cache

    call resize(self%dep)
    self%dep_dir = join_path("build", "dependencies")

    if (present(verbosity)) self%verbosity = verbosity

    if (present(cache)) self%cache = cache

  end subroutine new_dependency_tree

  !> Create a new dependency node from a configuration
  subroutine new_dependency_node(self, dependency, version, proj_dir, update)
    !> Instance of the dependency node
    type(dependency_node_t), intent(out) :: self
    !> Dependency configuration data
    type(dependency_config_t), intent(in) :: dependency
    !> Version of the dependency
    type(version_t), intent(in), optional :: version
    !> Installation prefix of the dependency
    character(len=*), intent(in), optional :: proj_dir
    !> Dependency should be updated
    logical, intent(in), optional :: update

    self%dependency_config_t = dependency

    if (present(version)) then
      self%version = version
    end if

    if (present(proj_dir)) then
      self%proj_dir = proj_dir
    end if

    if (present(update)) then
      self%update = update
    end if

  end subroutine new_dependency_node



  !> Update dependency tree
  subroutine update_dependency(self, name, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Name of the dependency to update
    character(len=*), intent(in) :: name
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: id
    character(len=:), allocatable :: proj_dir, root

    id = self%find(name)
    root = "."

    if (id <= 0) then
      call fatal_error(error, "Cannot update dependency '"//name//"'")
      return
    end if

    associate (dep => self%dep(id))
      if (allocated(dep%git) .and. dep%update) then
        if (self%verbosity > 0) write (self%unit, out_fmt) "Update:", dep%name
        proj_dir = join_path(self%dep_dir, dep%name)
        call dep%git%checkout(proj_dir, error)
        if (allocated(error)) return

        ! Unset dependency and remove updatable attribute
        dep%done = .false.
        dep%update = .false.

        ! Now decent into the dependency tree, level for level
        do while (.not. self%finished())
          call self%resolve(root, error)
          if (allocated(error)) exit
        end do
        if (allocated(error)) return
      end if
    end associate

  end subroutine update_dependency

  !> Update whole dependency tree
  subroutine update_tree(self, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: i

    ! Update dependencies where needed
    do i = 1, self%ndep
      call self%update(self%dep(i)%name, error)
      if (allocated(error)) return
    end do

  end subroutine update_tree





  subroutine check_and_read_pkg_data(json, node, download_url, version, error)
    type(json_object), intent(inout) :: json
    class(dependency_node_t), intent(in) :: node
    character(:), allocatable, intent(out) :: download_url
    type(version_t), intent(out) :: version
    type(error_t), allocatable, intent(out) :: error

    integer :: code, stat
    type(json_object), pointer :: p, q
    character(:), allocatable :: version_key, version_str, error_message, namespace, name

    namespace = ""
    name = "UNNAMED_NODE"
    if (allocated(node%namespace)) namespace = node%namespace
    if (allocated(node%name)) name = node%name

    if (.not. json%has_key('code')) then
      call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': No status code."); return
    end if

    call get_value(json, 'code', code, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': "// &
      & "Failed to read status code."); return
    end if

    if (code /= 200) then
      if (.not. json%has_key('message')) then
        call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': No error message."); return
      end if

      call get_value(json, 'message', error_message, stat=stat)
      if (stat /= 0) then
        call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': "// &
        & "Failed to read error message."); return
      end if

      call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"'. Status code: '"// &
      & str(code)//"'. Error message: '"//error_message//"'."); return
    end if

    if (.not. json%has_key('data')) then
      call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': No data."); return
    end if

    call get_value(json, 'data', p, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to read package data for '"//join_path(namespace, name)//"'."); return
    end if

    if (allocated(node%requested_version)) then
      version_key = 'version_data'
    else
      version_key = 'latest_version_data'
    end if

    if (.not. p%has_key(version_key)) then
      call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': No version data."); return
    end if

    call get_value(p, version_key, q, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to retrieve version data for '"//join_path(namespace, name)//"'."); return
    end if

    if (.not. q%has_key('download_url')) then
      call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': No download url."); return
    end if

    call get_value(q, 'download_url', download_url, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to read download url for '"//join_path(namespace, name)//"'."); return
    end if

    download_url = official_registry_base_url//download_url

    if (.not. q%has_key('version')) then
      call fatal_error(error, "Failed to download '"//join_path(namespace, name)//"': No version found."); return
    end if

    call get_value(q, 'version', version_str, stat=stat)
    if (stat /= 0) then
      call fatal_error(error, "Failed to read version data for '"//join_path(namespace, name)//"'."); return
    end if

    call new_version(version, version_str, error)
    if (allocated(error)) then
      call fatal_error(error, "'"//version_str//"' is not a valid version for '"// &
      & join_path(namespace, name)//"'."); return
    end if
  end subroutine



  !> Reallocate a list of dependencies
  pure subroutine resize_dependency_node(var, n)
    !> Instance of the array to be resized
    type(dependency_node_t), allocatable, intent(inout) :: var(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(dependency_node_t), allocatable :: tmp(:)
    integer :: this_size, new_size
    integer, parameter :: initial_size = 16

    if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
    else
      this_size = initial_size
    end if

    if (present(n)) then
      new_size = n
    else
      new_size = this_size + this_size/2 + 1
    end if

    allocate (var(new_size))

    if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate (tmp)
    end if

  end subroutine resize_dependency_node


    !> Destructor
    elemental subroutine destroy_dependency_node(self)

        class(dependency_node_t), intent(inout) :: self

        integer :: ierr

        ! Destroy the parent type
        call dependency_destroy(self%dependency_config_t)

        deallocate(self%version,stat=ierr)
        deallocate(self%proj_dir,stat=ierr)
        deallocate(self%revision,stat=ierr)
        self%done = .false.
        self%update = .false.
        self%cached = .false.

    end subroutine destroy_dependency_node


end module fpm_dependency
