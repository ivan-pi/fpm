!># Interface to TOML processing library
!>
!> This module acts as a proxy to the `toml-f` public Fortran API and allows
!> to selectively expose components from the library to `fpm`.
!> The interaction with `toml-f` data types outside of this module should be
!> limited to tables, arrays and key-lists, most of the necessary interactions
!> are implemented in the building interface with the `get_value` and `set_value`
!> procedures.
!>
!> This module allows to implement features necessary for `fpm`, which are
!> not yet available in upstream `toml-f`.
!>
!> For more details on the library used see the
!> [TOML-Fortran](https://toml-f.github.io/toml-f) developer pages.
module fpm_toml
    use iso_fortran_env, only: int64
    
    use fpm_strings, only: string_t, str_ends_with, lower
    use fpm_error, only: error_t, fatal_error, file_not_found_error
    
    use tomlf, only: toml_table, toml_array, toml_key, toml_stat, get_value, &
        & set_value, toml_error, new_table, add_table, add_array, &
        & toml_serialize, len, toml_load

    implicit none
    private

    ! Re-export TOMLF entities
    public :: toml_table, toml_array, toml_key, toml_stat, toml_error, &
              get_value, set_value, new_table, add_table, add_array, len, &
              toml_serialize, toml_load

    ! Fpm-added methods
    public :: read_package_file, check_keys, get_list, set_list, set_string, &
              name_is_json

    !> An abstract interface for any fpm class that should be fully serializable to/from TOML/JSON
    type, abstract, public :: serializable_t

        contains

        !> Dump to TOML table, unit, file
        procedure(to_toml), deferred :: dump_to_toml
        procedure, non_overridable, private :: dump_to_file
        procedure, non_overridable, private :: dump_to_unit
        generic :: dump => dump_to_toml, dump_to_file, dump_to_unit

        !> Load from TOML table, unit, file
        procedure(from_toml), deferred :: load_from_toml
        procedure, non_overridable, private :: load_from_file
        procedure, non_overridable, private :: load_from_unit
        generic :: load => load_from_toml, load_from_file, load_from_unit

        !> Serializable entities need a way to check that they're equal
        procedure(is_equal), deferred :: serializable_is_same
        generic :: operator(==) => serializable_is_same

        !> Test load/write roundtrip
        procedure, non_overridable :: test_serialization

    end type serializable_t

    !> add_table: fpm interface
    interface add_table
        module subroutine add_table_fpm(table, key, ptr, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            type(toml_table), pointer, intent(out) :: ptr
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
    end interface add_table

    !> set_value: fpm interface
    interface set_value
        module subroutine set_logical(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            logical, intent(in) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
        module subroutine set_integer(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            integer, intent(in) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
        module subroutine set_integer_64(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
    end interface set_value

    !> get_value: fpm interface
    interface get_value
        module subroutine get_logical(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            logical, intent(inout) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
        module subroutine get_integer(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            integer, intent(inout) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
        module subroutine get_integer_64(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            integer(int64), intent(inout) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
    end interface get_value

    interface set_string
        !> Function wrapper to set a character(len=:), allocatable variable to a toml table
        module subroutine set_character(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            character(len=*), optional, intent(in) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
        module subroutine set_string_type(table, key, var, error, whereAt)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            type(string_t), intent(in) :: var
            type(error_t), allocatable, intent(out) :: error
            character(len=*), intent(in), optional :: whereAt
        end subroutine
    end interface set_string

    interface
        ! Set string array
        module subroutine set_list(table, key, list, error)
            type(string_t), allocatable, intent(in) :: list(:)
            character(len=*), intent(in) :: key
            type(toml_table), intent(inout) :: table
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        ! Get string array
        module subroutine get_list(table, key, list, error)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: key
            type(string_t), allocatable, intent(out) :: list(:)
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

    ! Deferred methods of serializable
    abstract interface

      !> Write object to TOML datastructure
      subroutine to_toml(self, table, error)
        import serializable_t,toml_table,error_t
        implicit none

        !> Instance of the serializable object
        class(serializable_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

      end subroutine to_toml

      !> Read dependency tree from TOML data structure
      subroutine from_toml(self, table, error)
        import serializable_t,toml_table,error_t
        implicit none

        !> Instance of the serializable object
        class(serializable_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

      end subroutine from_toml

      !> Compare two serializable objects
      logical function is_equal(this,that)
         import serializable_t
         class(serializable_t), intent(in) :: this,that
      end function is_equal

    end interface

    interface
        !> Test serialization of a serializable object
        module subroutine test_serialization(self, message, error)
            class(serializable_t), intent(inout) :: self
            character(len=*), intent(in) :: message
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Write serializable object to a formatted Fortran unit
        module subroutine dump_to_unit(self, unit, error, json)
            !> Instance of the dependency tree
            class(serializable_t), intent(inout) :: self
            !> Formatted unit
            integer, intent(in) :: unit
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
            !> Optional JSON format requested?
            logical, optional, intent(in) :: json
        end subroutine
        !> Write serializable object to file
        module subroutine dump_to_file(self, file, error, json)
            !> Instance of the dependency tree
            class(serializable_t), intent(inout) :: self
            !> File name
            character(len=*), intent(in) :: file
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
            !> Optional JSON format
            logical, optional, intent(in) :: json
        end subroutine
        !> Read dependency tree from file
        module subroutine load_from_file(self, file, error, json)
            !> Instance of the dependency tree
            class(serializable_t), intent(inout) :: self
            !> File name
            character(len=*), intent(in) :: file
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
            !> Optional JSON format
            logical, optional, intent(in) :: json
        end subroutine
        !> Read dependency tree from file
        module subroutine load_from_unit(self, unit, error, json)
            !> Instance of the dependency tree
            class(serializable_t), intent(inout) :: self
            !> File name
            integer, intent(in) :: unit
            !> Error handling
            type(error_t), allocatable, intent(out) :: error
            !> Optional JSON format
            logical, optional, intent(in) :: json
        end subroutine
    end interface

    interface
        !> Process the configuration file to a TOML data structure
        module subroutine read_package_file(table, manifest, error)
            type(toml_table), allocatable, intent(out) :: table
            character(len=*), intent(in) :: manifest
            type(error_t), allocatable, intent(out) :: error
        end subroutine
        !> Check if table contains only keys that are part of the list. If a key is
        !> found that is not part of the list, an error is allocated.
        module subroutine check_keys(table, valid_keys, error)
            type(toml_table), intent(inout) :: table
            character(len=*), intent(in) :: valid_keys(:)
            type(error_t), allocatable, intent(out) :: error
        end subroutine
    end interface

contains


    !> Choose between JSON or TOML based on a file name
    logical function name_is_json(filename)
        character(*), intent(in) :: filename
        character(*), parameter :: json_identifier = ".json"

        name_is_json = .false.
        if (len_trim(filename)<len(json_identifier)) return
        name_is_json = str_ends_with(lower(filename),json_identifier)
    end function name_is_json

end module fpm_toml
