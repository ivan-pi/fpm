!># Discovery of sources
!>
!> This module implements subroutines for building a list of
!> `[[srcfile_t]]` objects by looking for source files in the filesystem.
!>
module fpm_sources

use fpm_strings, only: string_t
use fpm_error, only: error_t
use fpm_model, only: srcfile_t
use fpm_manifest, only: executable_config_t

implicit none
private

public :: add_sources_from_dir
public :: add_executable_sources
public :: get_exe_name_with_suffix

interface
    
    !> Add to `sources` by looking for source files in `directory`
    module subroutine add_sources_from_dir(sources,directory,scope,with_executables,with_f_ext,recurse,error)
        !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
        type(srcfile_t), allocatable, intent(inout), target :: sources(:)
        !> Directory in which to search for source files
        character(*), intent(in) :: directory
        !> Scope to apply to the discovered sources, see [[fpm_model]] for enumeration
        integer, intent(in) :: scope
        !> Executable sources (fortran `program`s) are ignored unless `with_executables=.true.`
        logical, intent(in), optional :: with_executables
        !> Additional user-defined (preprocessor) extensions that should be treated as Fortran sources
        type(string_t), intent(in), optional :: with_f_ext(:)
        !> Whether to recursively search subdirectories, default is `.true.`
        logical, intent(in), optional :: recurse
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
    
    !> Add to `sources` using the executable and test entries in the manifest and
    !> applies any executable-specific overrides such as `executable%name`.
    !> Adds all sources (including modules) from each `executable%source_dir`
    module subroutine add_executable_sources(sources,executables,scope,auto_discover,with_f_ext,error)
        !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
        type(srcfile_t), allocatable, intent(inout), target :: sources(:)
        !> List of `[[executable_config_t]]` entries from manifest
        class(executable_config_t), intent(in) :: executables(:)
        !> Scope to apply to the discovered sources: either `FPM_SCOPE_APP` or `FPM_SCOPE_TEST`, see [[fpm_model]]
        integer, intent(in) :: scope
        !> If `.false.` only executables and tests specified in the manifest are added to `sources`
        logical, intent(in) :: auto_discover
        !> Additional user-defined (preprocessor) extensions that should be treated as Fortran sources
        type(string_t), intent(in), optional :: with_f_ext(:)
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine

    !> Build an executable name with suffix. Safe routine that always returns an allocated string
    module function get_exe_name_with_suffix(source) result(suffixed)
        type(srcfile_t), intent(in) :: source
        character(len=:), allocatable :: suffixed
    end function
end interface



end module fpm_sources
