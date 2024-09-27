!> Implementation of the installation configuration.
!>
!> An install table can currently have the following fields
!>
!>```toml
!>library = bool
!>```
submodule (fpm_manifest) fpm_manifest_install

  use fpm_error, only : error_t, fatal_error, syntax_error
  use fpm_toml, only : toml_key, toml_stat, get_value, set_value, serializable_t

  character(*), parameter :: class_name = 'install_config_t'

contains

  !> Create a new installation configuration from a TOML data structure
  module subroutine new_install_config(self, table, error)

    !> Instance of the install configuration
    type(install_config_t), intent(out) :: self

    !> Instance of the TOML data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call check(table, error)
    if (allocated(error)) return

    call get_value(table, "library", self%library, .false.)

  end subroutine new_install_config


  !> Check local schema for allowed entries
  subroutine check(table, error)

    !> Instance of the TOML data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_key), allocatable :: list(:)
    integer :: ikey

    call table%get_keys(list)
    if (size(list) < 1) return

    do ikey = 1, size(list)
      select case(list(ikey)%key)
      case default
        call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in install table")
        exit
      case("library")
        continue
      end select
    end do
    if (allocated(error)) return

  end subroutine check

  !> Write information on install configuration instance
  module subroutine install_info(self, unit, verbosity)

    !> Instance of the build configuration
    class(install_config_t), intent(in) :: self

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

    write(unit, fmt) "Install configuration"
    write(unit, fmt) " - library install", &
      & trim(merge("enabled ", "disabled", self%library))

  end subroutine install_info

  module function install_is_same(this,that)
    class(install_config_t), intent(in) :: this
    class(serializable_t), intent(in) :: that
    logical :: install_is_same

    install_is_same = .false.

    select type (other=>that)
       type is (install_config_t)
          if (this%library.neqv.other%library) return
       class default
          ! Not the same type
          return
    end select

    !> All checks passed!
    install_is_same = .true.

  end function install_is_same

  !> Dump install config to toml table
  module subroutine install_dump(self, table, error)

    !> Instance of the serializable object
    class(install_config_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call set_value(table, "library", self%library, error, class_name)

  end subroutine install_dump

  !> Read install config from toml table (no checks made at this stage)
  module subroutine install_load(self, table, error)

    !> Instance of the serializable object
    class(install_config_t), intent(inout) :: self

    !> Data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: stat

    call get_value(table, "library", self%library, error, class_name)
    if (allocated(error)) return

  end subroutine install_load

end submodule fpm_manifest_install
