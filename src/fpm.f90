module fpm

use fpm_error, only : error_t
use fpm_manifest, only: package_config_t
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
    fpm_clean_settings, fpm_run_settings
use fpm_model, only: fpm_model_t

implicit none
private

public :: cmd_build, cmd_run, cmd_clean
public :: build_model, check_modules_for_duplicates

interface
    module subroutine cmd_build(settings)
        type(fpm_build_settings), intent(inout) :: settings
    end subroutine
    module subroutine cmd_run(settings,test)
        class(fpm_run_settings), intent(inout) :: settings
        logical, intent(in) :: test
    end subroutine
    module subroutine cmd_clean(settings)
        !> Settings for the clean command.
        class(fpm_clean_settings), intent(in) :: settings
    end subroutine
end interface

interface
    !> Constructs a valid fpm model from command line settings and the toml manifest.
    module subroutine build_model(model, settings, package, error)
        type(fpm_model_t), intent(out) :: model
        class(fpm_build_settings), intent(inout) :: settings
        type(package_config_t), intent(inout) :: package
        type(error_t), allocatable, intent(out) :: error
    end subroutine
    ! Check for duplicate modules
    module subroutine check_modules_for_duplicates(model, duplicates_found)
        type(fpm_model_t), intent(in) :: model
        logical :: duplicates_found
    end subroutine
end interface

end module fpm
