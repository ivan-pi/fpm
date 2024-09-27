!> Implementation for interacting with git repositories.
module fpm_git

    use fpm_error, only: error_t
    use fpm_toml, only: serializable_t, toml_table

    implicit none

    public :: git_target_t, git_target_default, git_target_branch, git_target_tag, git_target_revision, git_revision, &
            & git_archive, git_matches_manifest, compressed_package_name

    !> Name of the compressed package that is generated temporarily.
    character(len=*), parameter :: compressed_package_name = 'compressed_package'


    !> Possible git target
    type :: enum_descriptor

        !> Default target
        integer :: default = 200

        !> Branch in git repository
        integer :: branch = 201

        !> Tag in git repository
        integer :: tag = 202

        !> Commit hash
        integer :: revision = 203

        !> Invalid descriptor
        integer :: error = -999

    end type enum_descriptor

    !> Actual enumerator for descriptors
    type(enum_descriptor), parameter :: git_descriptor = enum_descriptor()


    !> Description of an git target
    type, extends(serializable_t) :: git_target_t

        !> Kind of the git target
        integer :: descriptor = git_descriptor%default

        !> Target URL of the git repository
        character(len=:), allocatable :: url

        !> Additional descriptor of the git object
        character(len=:), allocatable :: object

    contains

        !> Fetch and checkout in local directory
        procedure :: checkout => git_checkout

        !> Show information on instance
        procedure :: info => git_info

        !> Serialization interface
        procedure :: serializable_is_same => git_is_same
        procedure :: dump_to_toml => git_dump_to_toml
        procedure :: load_from_toml => git_load_from_toml

    end type git_target_t

    interface
        module subroutine git_checkout(self, local_path, error)

            !> Instance of the git target
            class(git_target_t), intent(in) :: self

            !> Local path to checkout in
            character(*), intent(in) :: local_path

            !> Error
            type(error_t), allocatable, intent(out) :: error
        end subroutine git_checkout
        !> Show information on git target
        module subroutine git_info(self, unit, verbosity)
            !> Instance of the git target
            class(git_target_t), intent(in) :: self
            !> Unit for IO
            integer, intent(in) :: unit
            !> Verbosity of the printout
            integer, intent(in), optional :: verbosity
        end subroutine git_info
        module logical function git_is_same(this,that)
            class(git_target_t), intent(in) :: this
            class(serializable_t), intent(in) :: that
        end function git_is_same
        !> Dump dependency to toml table
        module subroutine git_dump_to_toml(self, table, error)

            !> Instance of the serializable object
            class(git_target_t), intent(inout) :: self

            !> Data structure
            type(toml_table), intent(inout) :: table

            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine git_dump_to_toml
        !> Read dependency from toml table (no checks made at this stage)
        module subroutine git_load_from_toml(self, table, error)

            !> Instance of the serializable object
            class(git_target_t), intent(inout) :: self

            !> Data structure
            type(toml_table), intent(inout) :: table

            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine git_load_from_toml
    end interface


    interface
        module subroutine git_archive(source, destination, ref, additional_files, verbose, error)
            !> Directory to archive.
            character(*), intent(in) :: source
            !> Destination of the archive.
            character(*), intent(in) :: destination
            !> (Symbolic) Reference to be archived.
            character(*), intent(in) :: ref
            !> (Optional) list of additional untracked files to be added to the archive.
            character(*), optional, intent(in) :: additional_files(:)
            !> Print additional information if true.
            logical, intent(in) :: verbose
            !> Error handling.
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Check that a cached dependency matches a manifest request
        module logical function git_matches_manifest(cached,manifest,verbosity,iunit)

            !> Two input git targets
            type(git_target_t), intent(in) :: cached,manifest

            integer, intent(in) :: verbosity,iunit
        end function
        module subroutine git_revision(local_path, object, error)

            !> Local path to checkout in
            character(*), intent(in) :: local_path

            !> Git object reference
            character(len=:), allocatable, intent(out) :: object

            !> Error
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

contains

!> Default target
    function git_target_default(url) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%default
        self%url = url

    end function git_target_default


    !> Target a branch in the git repository
    function git_target_branch(url, branch) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Name of the branch of interest
        character(len=*), intent(in) :: branch

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%branch
        self%url = url
        self%object = branch

    end function git_target_branch


    !> Target a specific git revision
    function git_target_revision(url, sha1) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Commit hash of interest
        character(len=*), intent(in) :: sha1

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%revision
        self%url = url
        self%object = sha1

    end function git_target_revision


    !> Target a git tag
    function git_target_tag(url, tag) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Tag name of interest
        character(len=*), intent(in) :: tag

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%tag
        self%url = url
        self%object = tag

    end function git_target_tag

end module fpm_git
