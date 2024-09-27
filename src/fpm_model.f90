!># The fpm package model
!>
!> Defines the fpm model data types which encapsulate all information
!> required to correctly build a package and its dependencies.
!>
!> The process (see `[[build_model(subroutine)]]`) for generating a valid `[[fpm_model]]` involves
!>  source files discovery ([[fpm_sources]]) and parsing ([[fpm_source_parsing]]).
!>
!> Once a valid `[[fpm_model]]` has been constructed, it may be passed to `[[fpm_targets:targets_from_sources]]` to
!> generate a list of build targets for the backend.
!>
!>### Enumerations
!>
!> __Source type:__ `FPM_UNIT_*`
!> Describes the type of source file — determines build target generation
!>
!> The logical order of precedence for assigning `unit_type` is as follows:
!>
!>```
!> if source-file contains program then
!>   unit_type = FPM_UNIT_PROGRAM
!> else if source-file contains non-module subroutine/function then
!>   unit_type = FPM_UNIT_SUBPROGRAM
!> else if source-file contains submodule then
!>   unit_type = FPM_UNIT_SUBMODULE
!> else if source-file contains module then
!>   unit_type = FPM_UNIT_MODULE
!> end if
!>```
!>
!> @note A source file is only designated `FPM_UNIT_MODULE` if it **only** contains modules - no non-module subprograms.
!> (This allows tree-shaking/pruning of build targets based on unused module dependencies.)
!>
!> __Source scope:__ `FPM_SCOPE_*`
!> Describes the scoping rules for using modules — controls module dependency resolution
!>
module fpm_model
use iso_fortran_env, only: int64
use fpm_compiler, only: compiler_t, archiver_t!, debug
use fpm_dependency, only: dependency_tree_t
use fpm_strings, only: string_t!, str, len_trim, upper, operator(==)
use fpm_toml, only: serializable_t, toml_table!, toml_stat, set_value, set_list, get_value, &
!                     & get_list, add_table, toml_key, add_array, set_string
use fpm_error, only: error_t
use fpm_manifest, only: preprocess_config_t

implicit none
private

public :: fpm_model_t, srcfile_t, fortran_features_t, package_t

public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
          FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST, &
          FPM_UNIT_CPPSOURCE

public :: fpm_scope_name

interface
    module function fpm_scope_name(flag) result(name)
        integer, intent(in) :: flag
        character(len=:), allocatable :: name
    end function
end interface

!> Source type unknown
integer, parameter :: FPM_UNIT_UNKNOWN = -1
!> Source contains a fortran program
integer, parameter :: FPM_UNIT_PROGRAM = 1
!> Source **only** contains one or more fortran modules
integer, parameter :: FPM_UNIT_MODULE = 2
!> Source contains one or more fortran submodules
integer, parameter :: FPM_UNIT_SUBMODULE = 3
!> Source contains one or more fortran subprogram not within modules
integer, parameter :: FPM_UNIT_SUBPROGRAM = 4
!> Source type is c source file
integer, parameter :: FPM_UNIT_CSOURCE = 5
!> Source type is c header file
integer, parameter :: FPM_UNIT_CHEADER = 6
!> Souce type is c++ source file.
integer, parameter :: FPM_UNIT_CPPSOURCE = 7

!> Source has no module-use scope
integer, parameter :: FPM_SCOPE_UNKNOWN = -1
!> Module-use scope is library/dependency modules only
integer, parameter :: FPM_SCOPE_LIB = 1
!> Module-use scope is library/dependency modules only
integer, parameter :: FPM_SCOPE_DEP = 2
!> Module-use scope is library/dependency and app modules
integer, parameter :: FPM_SCOPE_APP = 3
!> Module-use scope is library/dependency and test modules
integer, parameter :: FPM_SCOPE_TEST = 4
integer, parameter :: FPM_SCOPE_EXAMPLE = 5


!> Enabled Fortran language features
type, extends(serializable_t) :: fortran_features_t

    !> Use default implicit typing
    logical :: implicit_typing = .false.

    !> Use implicit external interface
    logical :: implicit_external = .false.

    !> Form to use for all Fortran sources
    character(:), allocatable :: source_form

    contains

        !> Serialization interface
        procedure :: serializable_is_same => fft_is_same
        procedure :: dump_to_toml   => fft_dump_to_toml
        procedure :: load_from_toml => fft_load_from_toml

end type fortran_features_t

interface
    module logical function fft_is_same(this,that)
        class(fortran_features_t), intent(in) :: this
        class(serializable_t), intent(in) :: that
    end function
    !> Dump fortran features to toml table
    module subroutine fft_dump_to_toml(self, table, error)
        !> Instance of the serializable object
        class(fortran_features_t), intent(inout) :: self
        !> Data structure
        type(toml_table), intent(inout) :: table
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Read dependency from toml table (no checks made at this stage)
    module subroutine fft_load_from_toml(self, table, error)
        !> Instance of the serializable object
        class(fortran_features_t), intent(inout) :: self
        !> Data structure
        type(toml_table), intent(inout) :: table
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
end interface


!> Type for describing a source file
type, extends(serializable_t) :: srcfile_t
    !> File path relative to cwd
    character(:), allocatable :: file_name

    !> Name of executable for FPM_UNIT_PROGRAM
    character(:), allocatable :: exe_name

    !> Target module-use scope
    integer :: unit_scope = FPM_SCOPE_UNKNOWN

    !> Modules provided by this source file (lowerstring)
    type(string_t), allocatable :: modules_provided(:)

    !> Type of source unit
    integer :: unit_type = FPM_UNIT_UNKNOWN

    !> Parent modules (submodules only)
    type(string_t), allocatable :: parent_modules(:)

    !>  Modules USEd by this source file (lowerstring)
    type(string_t), allocatable :: modules_used(:)

    !> Files INCLUDEd by this source file
    type(string_t), allocatable :: include_dependencies(:)

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> Current hash
    integer(int64) :: digest

    contains

        !> Serialization interface
        procedure :: serializable_is_same => srcfile_is_same
        procedure :: dump_to_toml   => srcfile_dump_to_toml
        procedure :: load_from_toml => srcfile_load_from_toml

end type srcfile_t

interface
    !> Check that two source files are equal
    module logical function srcfile_is_same(this,that)
        class(srcfile_t), intent(in) :: this
        class(serializable_t), intent(in) :: that
    end function
    !> Dump dependency to toml table
    module subroutine srcfile_dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(srcfile_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
    module subroutine srcfile_load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(srcfile_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
end interface


!> Type for describing a single package
type, extends(serializable_t) :: package_t

    !> Name of package
    character(:), allocatable :: name

    !> Array of sources
    type(srcfile_t), allocatable :: sources(:)

    !> List of macros.
    type(preprocess_config_t) :: preprocess

    !> Package version number.
    character(:), allocatable :: version

    !> Module naming conventions
    logical :: enforce_module_names = .false.

    !> Prefix for all module names
    type(string_t) :: module_prefix

    !> Language features
    type(fortran_features_t) :: features

    contains

        !> Serialization interface
        procedure :: serializable_is_same => package_is_same
        procedure :: dump_to_toml   => package_dump_to_toml
        procedure :: load_from_toml => package_load_from_toml

end type package_t

interface
    module logical function package_is_same(this,that)
        class(package_t), intent(in) :: this
        class(serializable_t), intent(in) :: that
    end function
    !> Dump dependency to toml table
    module subroutine package_dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(package_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
    !> Read dependency from toml table (no checks made at this stage)
    module subroutine package_load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(package_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
end interface


!> Type describing everything required to build
!>  the root package and its dependencies.
type, extends(serializable_t) :: fpm_model_t

    !> Name of root package
    character(:), allocatable :: package_name

    !> Array of packages (including the root package)
    type(package_t), allocatable :: packages(:)

    !> Compiler object
    type(compiler_t) :: compiler

    !> Archiver object
    type(archiver_t) :: archiver

    !> Command line flags passed to fortran for compilation
    character(:), allocatable :: fortran_compile_flags

    !> Command line flags passed to C for compilation
    character(:), allocatable :: c_compile_flags

    !> Command line flags passed to C++ for compilation
    character(:), allocatable :: cxx_compile_flags

    !> Command line flags passed to the linker
    character(:), allocatable :: link_flags

    !> Base directory for build
    character(:), allocatable :: build_prefix

    !> Include directories
    type(string_t), allocatable :: include_dirs(:)

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> External modules used
    type(string_t), allocatable :: external_modules(:)

    !> Project dependencies
    type(dependency_tree_t) :: deps

    !> Whether tests should be added to the build list
    logical :: include_tests = .true.

    !> Whether module names should be prefixed with the package name
    logical :: enforce_module_names = .false.

    !> Prefix for all module names
    type(string_t) :: module_prefix

    contains

        !> Serialization interface
        procedure :: serializable_is_same => model_is_same
        procedure :: dump_to_toml   => model_dump_to_toml
        procedure :: load_from_toml => model_load_from_toml

        !> Output information to stdout
        procedure :: show => show_model
end type fpm_model_t

interface
    module subroutine show_model(model)
        ! Prints a human readable representation of the Model
        class(fpm_model_t), intent(in) :: model
    end subroutine
    !> Check that two model objects are equal
    module logical function model_is_same(this,that)
        class(fpm_model_t), intent(in) :: this
        class(serializable_t), intent(in) :: that
    end function
    !> Dump dependency to toml table
    module subroutine model_dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(fpm_model_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
    module subroutine model_load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(fpm_model_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error
    end subroutine
end interface

end module fpm_model
