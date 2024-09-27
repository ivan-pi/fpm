!># Build backend
!> Uses a list of `[[build_target_ptr]]` and a valid `[[fpm_model]]` instance
!> to schedule and execute the compilation and linking of package targets.
!>
!> The package build process (`[[build_package]]`) comprises three steps:
!>
!> 1. __Target sorting:__ topological sort of the target dependency graph (`[[sort_target]]`)
!> 2. __Target scheduling:__ group targets into schedule regions based on the sorting (`[[schedule_targets]]`)
!> 3. __Target building:__ generate targets by compilation or linking
!>
!> @note If compiled with OpenMP, targets will be build in parallel where possible.
!>
!>### Incremental compilation
!> The backend process supports *incremental* compilation whereby targets are not
!> re-compiled if their corresponding dependencies have not been modified.
!>
!> - Source-based targets (*i.e.* objects) are not re-compiled if the corresponding source
!>   file is unmodified AND all of the target dependencies are not marked for re-compilation
!>
!> - Link targets (*i.e.* executables and libraries) are not re-compiled if the
!>   target output file already exists AND all of the target dependencies are not marked for
!>   re-compilation
!>
!> Source file modification is determined by a file digest (hash) which is calculated during
!> the source parsing phase ([[fpm_source_parsing]]) and cached to disk after a target is
!> successfully generated.
!>
module fpm_backend

use fpm_model, only: fpm_model_t
use fpm_targets, only: build_target_t, build_target_ptr

implicit none
private

public :: build_package, sort_target, schedule_targets

interface
    !> Top-level routine to build package described by `model`
    module subroutine build_package(targets,model,verbose)
        type(build_target_ptr), intent(inout) :: targets(:)
        type(fpm_model_t), intent(in) :: model
        logical, intent(in) :: verbose
    end subroutine
    !> Topologically sort a target for scheduling by recursing over its dependencies.
    module recursive subroutine sort_target(target)
        type(build_target_t), intent(inout), target :: target
    end subroutine
    !> Construct a build schedule from the sorted targets.
    module subroutine schedule_targets(queue, schedule_ptr, targets)
        type(build_target_ptr), allocatable, intent(out) :: queue(:)
        integer, allocatable :: schedule_ptr(:)
        type(build_target_ptr), intent(in) :: targets(:)
    end subroutine
end interface

end module fpm_backend
