!> Implementation of the meta data for libraries.
!>
!> A library table can currently have the following fields
!>
!>```toml
!>[library]
!>source-dir = "path"
!>include-dir = ["path1","path2"]
!>build-script = "file"
!>```
submodule (fpm_manifest) fpm_manifest_library
    
    use fpm_error, only : error_t, syntax_error
    use fpm_strings, only: string_t, string_cat, operator(==)
    use fpm_toml, only : toml_key, toml_stat, get_value, get_list, serializable_t, set_value, &
                          set_list, set_string, get_value, get_list
    
    character(*), parameter:: class_name = 'library_config_t'

contains


    !> Construct a new library configuration from a TOML data structure
    module subroutine new_library(self, table, error)

        !> Instance of the library configuration
        type(library_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "source-dir", self%source_dir, "src")
        call get_value(table, "build-script", self%build_script)

        call get_list(table, "include-dir", self%include_dir, error)
        if (allocated(error)) return

        ! Set default value of include-dir if not found in manifest
        if (.not.allocated(self%include_dir)) then
            self%include_dir = [string_t("include")]
        end if

    end subroutine new_library


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        integer :: ikey

        call table%get_keys(list)

        ! table can be empty
        if (size(list) < 1) return

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in library")
                exit

            case("source-dir", "include-dir", "build-script")
                continue

            end select
        end do

    end subroutine check


    !> Write information on instance
    module subroutine library_info(self, unit, verbosity)

        !> Instance of the library configuration
        class(library_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Library target"
        if (allocated(self%source_dir)) then
            write(unit, fmt) "- source directory", self%source_dir
        end if
        if (allocated(self%include_dir)) then
            write(unit, fmt) "- include directory", string_cat(self%include_dir,",")
        end if
        if (allocated(self%build_script)) then
            write(unit, fmt) "- custom build", self%build_script
        end if

    end subroutine library_info

    module function library_is_same(this,that)
        class(library_config_t), intent(in) :: this
        class(serializable_t), intent(in) :: that
        logical :: library_is_same

        library_is_same = .false.

        select type (other=>that)
           type is (library_config_t)
              if (.not.this%include_dir==other%include_dir) return
              if (.not.allocated(this%source_dir).eqv.allocated(other%source_dir)) return
              if (.not.this%source_dir==other%source_dir) return
              if (.not.allocated(this%build_script).eqv.allocated(other%build_script)) return
              if (.not.this%build_script==other%build_script) return
           class default
              ! Not the same type
              return
        end select

        !> All checks passed!
        library_is_same = .true.

    end function library_is_same

    !> Dump install config to toml table
    module subroutine library_dump(self, table, error)

        !> Instance of the serializable object
        class(library_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call set_string(table, "source-dir", self%source_dir, error, class_name)
        if (allocated(error)) return
        call set_string(table, "build-script", self%build_script, error, class_name)
        if (allocated(error)) return
        call set_list(table, "include-dir", self%include_dir, error)
        if (allocated(error)) return

    end subroutine library_dump

    !> Read install config from toml table (no checks made at this stage)
    module subroutine library_load(self, table, error)

        !> Instance of the serializable object
        class(library_config_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call get_value(table, "source-dir", self%source_dir)
        if (allocated(error)) return
        call get_value(table, "build-script", self%build_script)
        if (allocated(error)) return
        call get_list(table, "include-dir", self%include_dir, error)

    end subroutine library_load


end submodule fpm_manifest_library
