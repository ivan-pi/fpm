!># The fpm meta-package model
!>
!> This is a wrapper data type that encapsulate all pre-processing information
!> (compiler flags, linker libraries, etc.) required to correctly enable a package
!> to use a core library.
!>
!>
!>### Available core libraries
!>
!> - OpenMP
!> - MPI
!> - HDF5
!> - fortran-lang stdlib
!> - fortran-lang minpack
!>
!>
!> @note Core libraries are enabled in the [build] section of the fpm.toml manifest
!>
!>
module fpm_meta

use fpm_error, only: error_t
use fpm_model, only: fpm_model_t
use fpm_manifest, only: package_config_t
use fpm_command_line, only: fpm_build_settings

implicit none
private

public :: resolve_metapackages

interface resolve_metapackages
    !> Resolve all metapackages into the package config
    module subroutine resolve_metapackage_model(model,package,settings,error)
        type(fpm_model_t), intent(inout) :: model
        type(package_config_t), intent(inout) :: package
        class(fpm_build_settings), intent(inout) :: settings
        type(error_t), allocatable, intent(out) :: error
    end subroutine
end interface resolve_metapackages

integer, parameter :: MPI_TYPE_NONE    = 0
integer, parameter :: MPI_TYPE_OPENMPI = 1
integer, parameter :: MPI_TYPE_MPICH   = 2
integer, parameter :: MPI_TYPE_INTEL   = 3
integer, parameter :: MPI_TYPE_MSMPI   = 4
public             :: MPI_TYPE_NAME

contains

!> Return a name for the MPI library
pure function MPI_TYPE_NAME(mpilib) result(name)
   integer, intent(in) :: mpilib
   character(len=:), allocatable :: name
   select case (mpilib)
      case (MPI_TYPE_NONE);    name = "none"
      case (MPI_TYPE_OPENMPI); name = "OpenMPI"
      case (MPI_TYPE_MPICH);   name = "MPICH"
      case (MPI_TYPE_INTEL);   name = "INTELMPI"
      case (MPI_TYPE_MSMPI);   name = "MS-MPI"
      case default;            name = "UNKNOWN"
   end select
end function MPI_TYPE_NAME

end module fpm_meta
