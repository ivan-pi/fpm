!> Implementation of the metapackage configuration data.
!>
!> A metapackage table can currently have the following fields
!>
!>```toml
!>[metapackages]
!>fpm = "0.1.0"
!>openmp = bool
!>stdlib = bool
!>```
submodule (fpm_manifest) fpm_manifest_metapackage

    use fpm_error, only: error_t, fatal_error, syntax_error
    use fpm_toml, only : toml_key, toml_stat, get_value

    !use fpm_environment

    use fpm_meta_request, only: new_meta_request

contains


    !> Construct a new build configuration from a TOML data structure
    subroutine new_meta_config(self, table, meta_allowed, error)

        !> Instance of the build configuration
        type(metapackage_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> List of keys allowed to be metapackages
        logical, intent(in) :: meta_allowed(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> The toml table is not checked here because it already passed
        !> the "new_dependencies" check
        call new_meta_request(self%openmp, "openmp", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%stdlib, "stdlib", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%minpack, "minpack", table, meta_allowed, error)
        if (allocated(error)) return

        call new_meta_request(self%mpi, "mpi", table, meta_allowed, error)
        if (allocated(error)) return
        
        call new_meta_request(self%hdf5, "hdf5", table, meta_allowed, error)
        if (allocated(error)) return

    end subroutine new_meta_config

end submodule fpm_manifest_metapackage
