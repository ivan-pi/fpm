!># Parsing of package source files
!>
!> This module exposes two functions, `[[parse_f_source]]` and `[[parse_c_source]]`,
!> which perform a rudimentary parsing of fortran and c source files
!> in order to extract information required for module dependency tracking.
!>
!> Both functions additionally calculate and store a file digest (hash) which
!> is used by the backend ([[fpm_backend]]) to skip compilation of unmodified sources.
!>
!> Both functions return an instance of the [[srcfile_t]] type.
!>
!> For more information, please read the documentation for each function:
!>
!> - `[[parse_f_source]]`
!> - `[[parse_c_source]]`
!>
module fpm_source_parsing

use fpm_error, only: error_t
use fpm_model, only: srcfile_t

implicit none
private

public :: parse_f_source, parse_c_source

interface
    !> Parsing of free-form fortran source files
    module function parse_f_source(f_filename,error)
        character(*), intent(in) :: f_filename
        type(error_t), allocatable, intent(out) :: error
        type(srcfile_t) :: parse_f_source
    end function
    !> Parsing of c, cpp source files
    module function parse_c_source(c_filename,error)
        character(*), intent(in) :: c_filename
        type(error_t), allocatable, intent(out) :: error
        type(srcfile_t) :: parse_c_source
    end function
end interface

end module fpm_source_parsing

