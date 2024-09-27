!> Package configuration data.
!>
!> This module provides the necessary procedure to translate a TOML document
!> to the corresponding Fortran type, while verifying it with respect to
!> its schema.
!>
!> Additionally, the required data types for users of this module are reexported
!> to hide the actual implementation details.
module fpm_manifest

    use fpm_strings, only: string_t
    use fpm_error, only: error_t, fatal_error

    use fpm_toml, only : toml_table, serializable_t, read_package_file
    use fpm_versioning, only: version_t

    use fpm_environment, only: OS_ALL, OS_WINDOWS
    use fpm_git, only: git_target_t

    use fpm_meta_request, only: metapackage_request_t
    use fpm_filesystem, only: join_path, exists, dirname, is_dir
    
    
    implicit none
    private

    public :: default_executable
    public :: default_library
    public :: default_test
    public :: default_example
    public :: package_defaults
    public :: get_default_profiles

    public :: get_package_data
    public :: manifest_has_changed
    public :: resize


    type, abstract, extends(serializable_t) :: config_t
    contains
        procedure(print_info), deferred :: info
    end type

    abstract interface
        !> Write information on instance
        subroutine print_info(self, unit, verbosity)
            import config_t
            !> Instance of the dependency configuration
            class(config_t), intent(in) :: self
            !> Unit for IO
            integer, intent(in) :: unit
            !> Verbosity of the printout
            integer, intent(in), optional :: verbosity
        end subroutine
    end interface

    public :: build_config_t
    public :: dependency_config_t
    public :: executable_config_t
    public ::   example_config_t
    public ::   test_config_t
    public :: fortran_config_t
    public :: install_config_t
    public :: library_config_t
    public :: metapackage_config_t
    public :: package_config_t
    public :: preprocess_config_t
    public :: profile_config_t

    public :: new_test
    public :: new_executable
    public :: new_fortran_config
    public :: new_install_config
    public :: new_library
    public :: new_meta_config
    public :: new_package
    public :: new_preprocess_config, new_preprocessors
    public :: new_profile, new_profiles

    !> Configuration data for build
    type, extends(config_t) :: build_config_t

        !> Automatic discovery of executables
        logical :: auto_executables = .true.

        !> Automatic discovery of examples
        logical :: auto_examples = .true.

        !> Automatic discovery of tests
        logical :: auto_tests = .true.

        !> Enforcing of package module names
        logical :: module_naming = .false.
        type(string_t) :: module_prefix

        !> Libraries to link against
        type(string_t), allocatable :: link(:)

        !> External modules to use
        type(string_t), allocatable :: external_modules(:)

    contains

        !> Print information on this instance
        procedure :: info => build_info

        !> Serialization interface
        procedure :: serializable_is_same => build_is_same
        procedure :: dump_to_toml => build_dump
        procedure :: load_from_toml => build_load

    end type build_config_t

    interface
        !> Construct a new build configuration from a TOML data structure
        module subroutine new_build_config(self, table, package_name, error)
            !> Instance of the build configuration
            type(build_config_t), intent(out) :: self
            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table
            !> Package name
            character(len=*), intent(in) :: package_name
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine build_info(self, unit, verbosity)
            class(build_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function build_is_same(this,that)
          class(build_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine build_dump(self, table, error)
           !> Instance of the serializable object
           class(build_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine build_load(self, table, error)
            !> Instance of the serializable object
            class(build_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface


    !> Configuration meta data for an executable
    type, extends(config_t) :: executable_config_t

        !> Name of the resulting executable
        character(len=:), allocatable :: name

        !> Source directory for collecting the executable
        character(len=:), allocatable :: source_dir

        !> Name of the source file declaring the main program
        character(len=:), allocatable :: main

        !> Dependency meta data for this executable
        type(dependency_config_t), allocatable :: dependency(:)

        !> Libraries to link against
        type(string_t), allocatable :: link(:)

    contains

        !> Print information on this instance
        procedure :: info => executable_info

        !> Serialization interface
        procedure :: serializable_is_same => executable_is_same
        procedure :: dump_to_toml => executable_dump
        procedure :: load_from_toml => executable_load

    end type executable_config_t

    interface
        module subroutine new_executable(self, table, error)

            !> Instance of the executable configuration
            type(executable_config_t), intent(out) :: self

            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table

            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine executable_info(self, unit, verbosity)
            class(executable_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function executable_is_same(this,that)
          class(executable_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine executable_dump(self, table, error)
           !> Instance of the serializable object
           class(executable_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine executable_load(self, table, error)
            !> Instance of the serializable object
            class(executable_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

    !> Configuration meta data for an example
    type, extends(executable_config_t) :: example_config_t
    contains
        !> Print information on this instance
        procedure :: info => example_info
    end type example_config_t

    interface
        module subroutine new_example(self, table, error)
            !> Instance of the example configuration
            type(example_config_t), intent(out) :: self
            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine example_info(self, unit, verbosity)
            class(example_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
    end interface

    !> Configuration data for Fortran
    type, extends(config_t) :: fortran_config_t

        !> Enable default implicit typing
        logical :: implicit_typing = .false.

        !> Enable implicit external interfaces
        logical :: implicit_external = .false.

        !> Form to use for all Fortran sources
        character(:), allocatable :: source_form

        contains
            procedure :: info => fortran_info

            !> Serialization interface
            procedure :: serializable_is_same => fortran_is_same
            procedure :: dump_to_toml => fortran_dump
            procedure :: load_from_toml => fortran_load
    end type fortran_config_t

    interface
        !> Construct a new build configuration from a TOML data structure
        module subroutine new_fortran_config(self, table, error)

            !> Instance of the fortran configuration
            type(fortran_config_t), intent(out) :: self

            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table

            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine fortran_info(self, unit, verbosity)
            class(fortran_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function fortran_is_same(this,that)
          class(fortran_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine fortran_dump(self, table, error)
           !> Instance of the serializable object
           class(fortran_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine fortran_load(self, table, error)
            !> Instance of the serializable object
            class(fortran_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

    !> Configuration meta data for a preprocessor
    type, extends(config_t) :: preprocess_config_t

        !> Name of the preprocessor
        character(len=:), allocatable :: name

        !> Suffixes of the files to be preprocessed
        type(string_t), allocatable :: suffixes(:)

        !> Directories to search for files to be preprocessed
        type(string_t), allocatable :: directories(:)

        !> Macros to be defined for the preprocessor
        type(string_t), allocatable :: macros(:)

    contains

        !> Print information on this instance
        procedure :: info => preprocess_info

        !> Serialization interface
        procedure :: serializable_is_same => preprocess_is_same
        procedure :: dump_to_toml => preprocess_dump
        procedure :: load_from_toml => preprocess_load

        !> Operations
        procedure :: destroy => preprocess_destroy
        procedure :: add_config => preprocess_add_config

        !> Properties
        procedure :: is_cpp => preprocess_is_cpp
        procedure :: is_fypp => preprocess_is_fypp

    end type preprocess_config_t

    interface
        !> Construct a new preprocess configuration from TOML data structure
        module subroutine new_preprocess_config(self, table, error)
          !> Instance of the preprocess configuration
          type(preprocess_config_t), intent(out) :: self
          !> Instance of the TOML data structure.
          type(toml_table), intent(inout) :: table
          !> Error handling
          type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Construct new preprocess array from a TOML data structure.
        module subroutine new_preprocessors(preprocessors, table, error)
          !> Instance of the preprocess configuration
          type(preprocess_config_t), allocatable, intent(out) :: preprocessors(:)
          !> Instance of the TOML data structure
          type(toml_table), intent(inout) :: table
          !> Error handling
          type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine preprocess_info(self, unit, verbosity)
            class(preprocess_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function preprocess_is_same(this,that)
          class(preprocess_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine preprocess_dump(self, table, error)
           !> Instance of the serializable object
           class(preprocess_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine preprocess_load(self, table, error)
            !> Instance of the serializable object
            class(preprocess_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine

        !> Add preprocessor settings
        module subroutine preprocess_add_config(this,that)
           class(preprocess_config_t), intent(inout) :: this
            type(preprocess_config_t), intent(in) :: that
        end subroutine
        ! Check cpp
        module logical function preprocess_is_cpp(this)
           class(preprocess_config_t), intent(in) :: this
        end function preprocess_is_cpp
        ! Check is Fypp
        module logical function preprocess_is_fypp(this)
           class(preprocess_config_t), intent(in) :: this
        end function preprocess_is_fypp
    end interface


    !> Configuration data for installation
    type, extends(config_t) :: install_config_t

        !> Install library with this project
        logical :: library = .false.

    contains

        !> Print information on this instance
        procedure :: info => install_info

        !> Serialization interface
        procedure :: serializable_is_same => install_is_same
        procedure :: dump_to_toml => install_dump
        procedure :: load_from_toml => install_load

    end type install_config_t

    interface
        module subroutine new_install_config(self, table, error)

            !> Instance of the install configuration
            type(install_config_t), intent(out) :: self

            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table

            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine install_info(self, unit, verbosity)
            class(install_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function install_is_same(this,that)
          class(install_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine install_dump(self, table, error)
           !> Instance of the serializable object
           class(install_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine install_load(self, table, error)
            !> Instance of the serializable object
            class(install_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

    !> Configuration meta data for a library
    type, extends(config_t) :: library_config_t

        !> Source path prefix
        character(len=:), allocatable :: source_dir

        !> Include path prefix
        type(string_t), allocatable :: include_dir(:)

        !> Alternative build script to be invoked
        character(len=:), allocatable :: build_script

    contains

        !> Print information on this instance
        procedure :: info => library_info

        !> Serialization interface
        procedure :: serializable_is_same => library_is_same
        procedure :: dump_to_toml => library_dump
        procedure :: load_from_toml => library_load

    end type library_config_t

    interface
        !> Construct a new library configuration from a TOML data structure
        module subroutine new_library(self, table, error)
            !> Instance of the library configuration
            type(library_config_t), intent(out) :: self
            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine library_info(self, unit, verbosity)
            class(library_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function library_is_same(this,that)
          class(library_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine library_dump(self, table, error)
           !> Instance of the serializable object
           class(library_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine library_load(self, table, error)
            !> Instance of the serializable object
            class(library_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

    !> Configuration data for metapackages
    type :: metapackage_config_t
        !> Request MPI support
        type(metapackage_request_t) :: mpi
        !> Request OpenMP support
        type(metapackage_request_t) :: openmp
        !> Request stdlib support
        type(metapackage_request_t) :: stdlib
        !> fortran-lang minpack
        type(metapackage_request_t) :: minpack
        !> HDF5
        type(metapackage_request_t) :: hdf5
    end type metapackage_config_t

    interface
        !> Construct a new build configuration from a TOML data structure
        module subroutine new_meta_config(self, table, meta_allowed, error)
            !> Instance of the build configuration
            type(metapackage_config_t), intent(out) :: self
            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table
            !> List of keys allowed to be metapackages
            logical, intent(in) :: meta_allowed(:)
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

    !> Configuration meta data for a dependency
    type, extends(config_t) :: dependency_config_t

        !> Name of the dependency
        character(len=:), allocatable :: name

        !> Local target
        character(len=:), allocatable :: path

        !> Namespace which the dependency belongs to.
        !> Enables multiple dependencies with the same name.
        !> Required for dependencies that are obtained via the official registry.
        character(len=:), allocatable :: namespace

        !> The requested version of the dependency.
        !> The latest version is used if not specified.
        type(version_t), allocatable :: requested_version

        !> Requested macros for the dependency
        type(preprocess_config_t), allocatable :: preprocess(:)

        !> Git descriptor
        type(git_target_t), allocatable :: git

    contains

        !> Print information on this instance
        procedure :: info => dependency_info

        !> Serialization interface
        procedure :: serializable_is_same => dependency_is_same
        procedure :: dump_to_toml => dependency_dump
        procedure :: load_from_toml => dependency_load

    end type dependency_config_t

    interface resize
        !> Reallocate a list of dependencies
        pure module subroutine resize_dependency_config(var, n)
            !> Instance of the array to be resized
            type(dependency_config_t), allocatable, intent(inout) :: var(:)
            !> Dimension of the final array size
            integer, intent(in), optional :: n
        end subroutine
    end interface

    interface
        !> Construct new dependency array from a TOML data structure
        module subroutine new_dependencies(deps, table, root, meta, error)
            !> Instance of the dependency configuration
            type(dependency_config_t), allocatable, intent(out) :: deps(:)
            !> (optional) metapackages
            type(metapackage_config_t), optional, intent(out) :: meta
            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table
            !> Root directory of the manifest
            character(*), intent(in), optional :: root
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        
        !> Check if two dependency configurations are different
        module function manifest_has_changed(cached, manifest, verbosity, iunit)
            !> Two instances of the dependency configuration
            class(dependency_config_t), intent(in) :: cached, manifest
            !> Log verbosity
            integer, intent(in) :: verbosity, iunit
            logical :: manifest_has_changed
        end function manifest_has_changed

        module subroutine dependency_info(self, unit, verbosity)
            class(dependency_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function dependency_is_same(this,that)
          class(dependency_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine dependency_dump(self, table, error)
           !> Instance of the serializable object
           class(dependency_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine dependency_load(self, table, error)
            !> Instance of the serializable object
            class(dependency_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface


        !> Type storing file name - file scope compiler flags pairs
        !type, extends(serializable_t) :: file_scope_flag
        type :: file_scope_flag
          !> Name of the file
          character(len=:), allocatable :: file_name
          !> File scope flags
          character(len=:), allocatable :: flags
        end type file_scope_flag


    !> Configuration meta data for a profile
    type, extends(config_t) :: profile_config_t
      !> Name of the profile
      character(len=:), allocatable :: profile_name

      !> Name of the compiler
      character(len=:), allocatable :: compiler

      !> Value repesenting OS
      integer :: os_type = OS_ALL

      !> Fortran compiler flags
      character(len=:), allocatable :: flags

      !> C compiler flags
      character(len=:), allocatable :: c_flags

      !> C++ compiler flags
      character(len=:), allocatable :: cxx_flags

      !> Link time compiler flags
      character(len=:), allocatable :: link_time_flags


      !> Is this profile one of the built-in ones?
      logical :: is_built_in = .false.

      !> File scope flags
      type(file_scope_flag), allocatable, dimension(:) :: file_scope_flags

    contains

        !> Print information on this instance
        procedure :: info => profile_info

        !> Serialization interface
        procedure :: serializable_is_same => profile_is_same
        procedure :: dump_to_toml => profile_dump
        procedure :: load_from_toml => profile_load

    end type profile_config_t

    interface
        !> Construct new profiles array from a TOML data structure
        module subroutine new_profiles(profiles, table, error)

            !> Instance of the dependency configuration
            type(profile_config_t), allocatable, intent(out) :: profiles(:)

            !> Instance of the TOML data structure
            type(toml_table), target, intent(inout) :: table

            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine profile_info(self, unit, verbosity)
            class(profile_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function profile_is_same(this,that)
          class(profile_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine profile_dump(self, table, error)
           !> Instance of the serializable object
           class(profile_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine profile_load(self, table, error)
            !> Instance of the serializable object
            class(profile_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface


    !> Configuration meta data for an test
    type, extends(executable_config_t) :: test_config_t
    contains
        !> Print information on this instance
        procedure :: info => test_info
    end type test_config_t

    interface
        !> Construct a new test configuration from a TOML data structure
        module subroutine new_test(self, table, error)
            !> Instance of the test configuration
            type(test_config_t), intent(out) :: self
            !> Instance of the TOML data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        module subroutine test_info(self, unit, verbosity)
            class(test_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
    end interface

    !> Package meta data
    type, extends(config_t) :: package_config_t

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
        procedure :: info => package_info

        !> Serialization interface
        procedure :: serializable_is_same => package_is_same
        procedure :: dump_to_toml => package_dump
        procedure :: load_from_toml => package_load

    end type package_config_t

    interface

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

        module subroutine package_info(self, unit, verbosity)
            class(package_config_t), intent(in) :: self
            integer, intent(in) :: unit
            integer, intent(in), optional :: verbosity
        end subroutine
        module logical function package_is_same(this,that)
          class(package_config_t), intent(in) :: this
          class(serializable_t), intent(in) :: that
        end function
        !> Dump manifest to toml table
        module subroutine package_dump(self, table, error)
           !> Instance of the serializable object
           class(package_config_t), intent(inout) :: self
           !> Data structure
           type(toml_table), intent(inout) :: table
           !> Error handling
           type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Read manifest from toml table (no checks made at this stage)
        module subroutine package_load(self, table, error)
            !> Instance of the serializable object
            class(package_config_t), intent(inout) :: self
            !> Data structure
            type(toml_table), intent(inout) :: table
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

contains


    !> Clean preprocessor structure
    elemental subroutine preprocess_destroy(this)
       class(preprocess_config_t), intent(inout) :: this

       if (allocated(this%name))deallocate(this%name)
       if (allocated(this%suffixes))deallocate(this%suffixes)
       if (allocated(this%directories))deallocate(this%directories)
       if (allocated(this%macros))deallocate(this%macros)

    end subroutine preprocess_destroy

    !> Populate library in case we find the default src directory
    subroutine default_library(self)

        !> Instance of the library meta data
        type(library_config_t), intent(out) :: self

        self%source_dir = "src"
        self%include_dir = [string_t("include")]

    end subroutine default_library


    !> Populate executable in case we find the default app directory
    subroutine default_executable(self, name)

        !> Instance of the executable meta data
        type(executable_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name
        self%source_dir = "app"
        self%main = "main.f90"

    end subroutine default_executable

    !> Populate test in case we find the default example/ directory
    subroutine default_example(self, name)

        !> Instance of the executable meta data
        type(example_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name // "-demo"
        self%source_dir = "example"
        self%main = "main.f90"

    end subroutine default_example

    !> Populate test in case we find the default test/ directory
    subroutine default_test(self, name)

        !> Instance of the executable meta data
        type(test_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name // "-test"
        self%source_dir = "test"
        self%main = "main.f90"

    end subroutine default_test


      !> Construct a new profile configuration from a TOML data structure
      function new_profile(profile_name, compiler, os_type, flags, c_flags, cxx_flags, &
                           link_time_flags, file_scope_flags, is_built_in) &
                      & result(profile)

        !> Name of the profile
        character(len=*), intent(in) :: profile_name

        !> Name of the compiler
        character(len=*), intent(in) :: compiler

        !> Type of the OS
        integer, intent(in) :: os_type

        !> Fortran compiler flags
        character(len=*), optional, intent(in) :: flags

        !> C compiler flags
        character(len=*), optional, intent(in) :: c_flags

        !> C++ compiler flags
        character(len=*), optional, intent(in) :: cxx_flags

        !> Link time compiler flags
        character(len=*), optional, intent(in) :: link_time_flags

        !> File scope flags
        type(file_scope_flag), optional, intent(in) :: file_scope_flags(:)

        !> Is this profile one of the built-in ones?
        logical, optional, intent(in) :: is_built_in

        type(profile_config_t) :: profile

        profile%profile_name = profile_name
        profile%compiler = compiler
        profile%os_type = os_type
        if (present(flags)) then
          profile%flags = flags
        else
          profile%flags = ""
        end if
        if (present(c_flags)) then
          profile%c_flags = c_flags
        else
          profile%c_flags = ""
        end if
        if (present(cxx_flags)) then
          profile%cxx_flags = cxx_flags
        else
          profile%cxx_flags = ""
        end if
        if (present(link_time_flags)) then
          profile%link_time_flags = link_time_flags
        else
          profile%link_time_flags = ""
        end if
        if (present(file_scope_flags)) then
           profile%file_scope_flags = file_scope_flags
        end if
        if (present(is_built_in)) then
           profile%is_built_in = is_built_in
        else
           profile%is_built_in = .false.
        end if

      end function new_profile

      !> Construct an array of built-in profiles
      function get_default_profiles(error) result(default_profiles)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(profile_config_t), allocatable :: default_profiles(:)


        default_profiles = [ &
              & new_profile( &
                    profile_name='release', &
                    compiler='caf', &
                    os_type=OS_ALL, &
                    flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='gfortran', &
                    os_type=OS_ALL, &
                    flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops -fcoarray=single', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='f95', &
                    os_type=OS_ALL, &
                    flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -ffast-math -funroll-loops', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='nvfortran', &
                    os_type=OS_ALL, &
                    flags = ' -Mbackslash', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='ifort', &
                    os_type=OS_ALL, &
                    flags = ' -fp-model precise -pc64 -align all -error-limit 1 -reentrancy&
                          & threaded -nogen-interfaces -assume byterecl', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='ifort', &
                    os_type=OS_WINDOWS, &
                    flags = ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
                          & /nogen-interfaces /assume:byterecl', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='ifx', &
                    os_type=OS_ALL, &
                    flags = ' -fp-model=precise -pc64 -align all -error-limit 1 -reentrancy&
                          & threaded -nogen-interfaces -assume byterecl', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='ifx', &
                    os_type=OS_WINDOWS, &
                    flags = ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
                          & /nogen-interfaces /assume:byterecl', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='nagfor', &
                    os_type=OS_ALL, &
                    flags = ' -O4 -coarray=single -PIC', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='release', &
                    compiler='lfortran', &
                    os_type=OS_ALL, &
                    flags = ' flag_lfortran_opt', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='caf', &
                    os_type=OS_ALL, &
                    flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -fbacktrace', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='gfortran', &
                    os_type=OS_ALL, &
                    flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -fbacktrace -fcoarray=single', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='f95', &
                    os_type=OS_ALL, &
                    flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -Wno-maybe-uninitialized -Wno-uninitialized -fbacktrace', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='nvfortran', &
                    os_type=OS_ALL, &
                    flags = ' -Minform=inform -Mbackslash -g -Mbounds -Mchkptr -Mchkstk -traceback', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='ifort', &
                    os_type=OS_ALL, &
                    flags = ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='ifort', &
                    os_type=OS_WINDOWS, &
                    flags = ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl /traceback', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='ifx', &
                    os_type=OS_ALL, &
                    flags = ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='ifx', &
                    os_type=OS_WINDOWS, &
                    flags = ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='ifx', &
                    os_type=OS_WINDOWS, &
                    flags = ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl', &
                    is_built_in=.true.), &
              & new_profile( &
                    profile_name='debug', &
                    compiler='lfortran', &
                    os_type=OS_ALL, &
                    flags = '', &
                    is_built_in=.true.) &
              &]
      end function get_default_profiles


    !> Apply package defaults
    subroutine package_defaults(package, root, error)

        use fpm_error, only : error_t, fatal_error

        !> Parsed package meta data
        type(package_config_t), intent(inout) :: package

        !> Current working directory
        character(len=*), intent(in) :: root

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        ! Populate library in case we find the default src directory
        if (.not.allocated(package%library) .and. &
            & (is_dir(join_path(root, "src")) .or. &
            &  is_dir(join_path(root, "include")))) then

            allocate(package%library)
            call default_library(package%library)
        end if

        ! Populate executable in case we find the default app
        if (.not.allocated(package%executable) .and. &
            & exists(join_path(root, "app", "main.f90"))) then
            allocate(package%executable(1))
            call default_executable(package%executable(1), package%name)
        end if

        ! Populate example in case we find the default example directory
        if (.not.allocated(package%example) .and. &
            & exists(join_path(root, "example", "main.f90"))) then
            allocate(package%example(1))
            call default_example(package%example(1), package%name)
        endif

        ! Populate test in case we find the default test directory
        if (.not.allocated(package%test) .and. &
            & exists(join_path(root, "test", "main.f90"))) then
            allocate(package%test(1))
            call default_test(package%test(1), package%name)
        endif

        if (.not.(allocated(package%library) &
            & .or. allocated(package%executable) &
            & .or. allocated(package%example) &
            & .or. allocated(package%test))) then
            call fatal_error(error, "Neither library nor executable found, there is nothing to do")
            return
        end if

    end subroutine package_defaults


    !> Obtain package meta data from a configuation file
    subroutine get_package_data(package, file, error, apply_defaults)

        !> Parsed package meta data
        type(package_config_t), intent(out) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        !> Apply package defaults (uses file system operations)
        logical, intent(in), optional :: apply_defaults

        type(toml_table), allocatable :: table
        character(len=:), allocatable :: root

        call read_package_file(table, file, error)
        if (allocated(error)) return

        if (.not. allocated(table)) then
            call fatal_error(error, "Unclassified error while reading: '"//file//"'")
            return
        end if

        call new_package(package, table, dirname(file), error)
        if (allocated(error)) return

        if (present(apply_defaults)) then
            if (apply_defaults) then
                root = dirname(file)
                if (len_trim(root) == 0) root = "."
                call package_defaults(package, root, error)
                if (allocated(error)) return
            end if
        end if

    end subroutine get_package_data


end module fpm_manifest
