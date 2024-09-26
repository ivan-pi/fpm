!> Define the package data containing the meta data from the configuration file.
!>
!> The package data defines a Fortran type corresponding to the respective
!> TOML document, after creating it from a package file no more interaction
!> with the TOML document is required.
!>
!> Every configuration type provides it custom constructor (prefixed with `new_`)
!> and knows how to deserialize itself from a TOML document.
!> To ensure we find no untracked content in the package file all keywords are
!> checked and possible entries have to be explicitly allowed in the `check`
!> function.
!> If entries are mutally exclusive or interdependent inside the current table
!> the `check` function is required to enforce this schema on the data structure.
!>
!> The package file root allows the following keywords
!>
!>```toml
!>name = "string"
!>version = "string"
!>license = "string"
!>author = "string"
!>maintainer = "string"
!>copyright = "string"
!>[library]
!>[dependencies]
!>[dev-dependencies]
!>[profiles]
!>[build]
!>[install]
!>[fortran]
!>[[ executable ]]
!>[[ example ]]
!>[[ test ]]
!>[extra]
!>```
module fpm_manifest_package

    use fpm_error, only: error_t
    use fpm_toml, only: toml_table, serializable_t
    use fpm_versioning, only : version_t

    use fpm_manifest_build, only: build_config_t
    use fpm_manifest_metapackages, only: metapackage_config_t
    use fpm_manifest_install, only: install_config_t
    use fpm_manifest_fortran, only : fortran_config_t
    use fpm_manifest_dependency, only : dependency_config_t
    use fpm_manifest_example, only : example_config_t
    use fpm_manifest_executable, only : executable_config_t
    use fpm_manifest_library, only : library_config_t
    use fpm_manifest_preprocess, only : preprocess_config_t
    use fpm_manifest_test, only : test_config_t
    use fpm_manifest_profile, only : profile_config_t

    implicit none (type,external)
    private

    public :: package_config_t, new_package


    !> Package meta data
    type, extends(serializable_t) :: package_config_t

        !> Name of the package
        character(len=:), allocatable :: name

        !> Package version
        type(version_t) :: version

        !> Build configuration data
        type(build_config_t) :: build

        !> Metapackage data
        type(metapackage_config_t) :: meta

        !> Installation configuration data
        type(install_config_t) :: install

        !> Fortran meta data
        type(fortran_config_t) :: fortran

        !> License meta data
        character(len=:), allocatable :: license

        !> Author meta data
        character(len=:), allocatable :: author

        !> Maintainer meta data
        character(len=:), allocatable :: maintainer

        !> Copyright meta data
        character(len=:), allocatable :: copyright

        !> Library meta data
        type(library_config_t), allocatable :: library

        !> Executable meta data
        type(executable_config_t), allocatable :: executable(:)

        !> Dependency meta data
        type(dependency_config_t), allocatable :: dependency(:)

        !> Development dependency meta data
        type(dependency_config_t), allocatable :: dev_dependency(:)

        !> Profiles meta data
        type(profile_config_t), allocatable :: profiles(:)

        !> Example meta data
        type(example_config_t), allocatable :: example(:)

        !> Test meta data
        type(test_config_t), allocatable :: test(:)

        !> Preprocess meta data
        type(preprocess_config_t), allocatable :: preprocess(:)

    contains

        !> Print information on this instance
        procedure :: info

        !> Serialization interface
        procedure :: serializable_is_same => manifest_is_same
        procedure :: dump_to_toml => dump_to_toml
        procedure :: load_from_toml => load_from_toml

    end type package_config_t
    character(len=*), parameter :: class_name = 'package_config_t'

    interface
        !> Construct a new package configuration from a TOML data structure
        module subroutine new_package(self, table, root, error)
            !> Instance of the package configuration
            type(package_config_t), intent(out) :: self
            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table
            !> Root directory of the manifest
            character(len=*), intent(in), optional :: root
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine

        module function manifest_is_same(this,that)
          class(package_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
          logical :: manifest_is_same
        end function

        !> Dump manifest to toml table
        module subroutine dump_to_toml(self, table, error)
           !> Instance of the serializable object
           class(package_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine

        !> Read manifest from toml table (no checks made at this stage)
        module subroutine load_from_toml(self, table, error)
            !> Instance of the serializable object
            class(package_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine

        !> Write information on instance
        module subroutine info(self, unit, verbosity)
            !> Instance of the package configuration
            class(package_config_t), intent(in) :: self
            !> Unit for IO
            integer, intent(in) :: unit
            !> Verbosity of the printout
            integer, intent(in), optional :: verbosity
        end subroutine
    end interface

end module fpm_manifest_package
