submodule (fpm_git) fpm_git_impl

    use fpm_error, only: error_t, fatal_error
    use fpm_filesystem, only : get_temp_filename, getline, join_path, execute_and_read_output, run
    use fpm_toml, only: get_value, set_value, toml_stat, set_string

    !> Common output format for writing to the command line
    character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

contains

    !> Check that two git targets are equal
    logical function git_is_same(this,that)
        class(git_target_t), intent(in) :: this
        class(serializable_t), intent(in) :: that

        git_is_same = .false.

        select type (other=>that)
           type is (git_target_t)

              if (.not.(this%descriptor==other%descriptor)) return
              if (.not.(this%url==other%url)) return
              if (.not.(this%object==other%object)) return

           class default
              ! Not the same type
              return
        end select

        !> All checks passed!
        git_is_same = .true.

    end function git_is_same

    !> Check that a cached dependency matches a manifest request
    logical function git_matches_manifest(cached,manifest,verbosity,iunit)

        !> Two input git targets
        type(git_target_t), intent(in) :: cached,manifest

        integer, intent(in) :: verbosity,iunit

        git_matches_manifest = cached%url == manifest%url
        if (.not.git_matches_manifest) then
            if (verbosity>1) write(iunit,out_fmt) "GIT URL has changed: ",cached%url," vs. ", manifest%url
            return
        endif

        !> The manifest dependency only contains partial information (what's requested),
        !> while the cached dependency always stores a commit hash because it's built
        !> after the repo is available (saved as git_descriptor%revision==revision).
        !> So, comparing against the descriptor is not reliable
        git_matches_manifest = allocated(cached%object) .eqv. allocated(manifest%object)
        if (git_matches_manifest .and. allocated(cached%object)) &
        git_matches_manifest = cached%object == manifest%object
        if (.not.git_matches_manifest) then
            if (verbosity>1) write(iunit,out_fmt) "GIT OBJECT has changed: ",cached%object," vs. ", manifest%object
        end if

    end function git_matches_manifest


    subroutine git_checkout(self, local_path, error)

        !> Instance of the git target
        class(git_target_t), intent(in) :: self

        !> Local path to checkout in
        character(*), intent(in) :: local_path

        !> Error
        type(error_t), allocatable, intent(out) :: error

        integer :: stat
        character(len=:), allocatable :: object, workdir

        if (allocated(self%object)) then
            object = self%object
        else
            object = 'HEAD'
        end if
        workdir = "--work-tree="//local_path//" --git-dir="//join_path(local_path, ".git")

        call execute_command_line("git init "//local_path, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while initiating git repository for remote dependency')
            return
        end if

        call execute_command_line("git "//workdir//" fetch --depth=1 "// &
                                  self%url//" "//object, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while fetching git repository for remote dependency')
            return
        end if

        call execute_command_line("git "//workdir//" checkout -qf FETCH_HEAD", exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while checking out git repository for remote dependency')
            return
        end if

    end subroutine git_checkout


    subroutine git_revision(local_path, object, error)

        !> Local path to checkout in
        character(*), intent(in) :: local_path

        !> Git object reference
        character(len=:), allocatable, intent(out) :: object

        !> Error
        type(error_t), allocatable, intent(out) :: error

        integer :: stat, unit, istart, iend
        character(len=:), allocatable :: temp_file, line, iomsg, workdir
        character(len=*), parameter :: hexdigits = '0123456789abcdef'

        workdir = "--work-tree="//local_path//" --git-dir="//join_path(local_path, ".git")
        allocate(temp_file, source=get_temp_filename())
        line = "git "//workdir//" log -n 1 > "//temp_file
        call execute_command_line(line, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error, "Error while retrieving commit information")
            return
        end if

        open(file=temp_file, newunit=unit)
        call getline(unit, line, stat, iomsg)

        if (stat /= 0) then
            call fatal_error(error, iomsg)
            return
        end if
        close(unit, status="delete")

        ! Tokenize:
        ! commit 0123456789abcdef (HEAD, ...)
        istart = scan(line, ' ') + 1
        iend = verify(line(istart:), hexdigits) + istart - 1
        if (iend < istart) iend = len(line)
        object = line(istart:iend)

    end subroutine git_revision


    !> Show information on git target
    subroutine git_info(self, unit, verbosity)

        !> Instance of the git target
        class(git_target_t), intent(in) :: self

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

        write(unit, fmt) "Git target"
        if (allocated(self%url)) then
            write(unit, fmt) "- URL", self%url
        end if
        if (allocated(self%object)) then
            select case(self%descriptor)
            case default
                write(unit, fmt) "- object", self%object
            case(git_descriptor%tag)
                write(unit, fmt) "- tag", self%object
            case(git_descriptor%branch)
                write(unit, fmt) "- branch", self%object
            case(git_descriptor%revision)
                write(unit, fmt) "- sha1", self%object
            end select
        end if

    end subroutine git_info

    !> Dump dependency to toml table
    subroutine git_dump_to_toml(self, table, error)

        !> Instance of the serializable object
        class(git_target_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: ierr

        call set_string(table, "descriptor", descriptor_name(self%descriptor), error, 'git_target_t')
        if (allocated(error)) return
        call set_string(table, "url", self%url, error, 'git_target_t')
        if (allocated(error)) return
        call set_string(table, "object", self%object, error, 'git_target_t')
        if (allocated(error)) return

    end subroutine git_dump_to_toml

    !> Read dependency from toml table (no checks made at this stage)
    subroutine git_load_from_toml(self, table, error)

        !> Instance of the serializable object
        class(git_target_t), intent(inout) :: self

        !> Data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Local variables
        character(len=:), allocatable :: descriptor_name

        call get_value(table, "descriptor", descriptor_name)
        self%descriptor = parse_descriptor(descriptor_name)

        if (self%descriptor==git_descriptor%error) then
            call fatal_error(error,"invalid descriptor ID <"//descriptor_name//"> in TOML entry")
            return
        end if

        !> Target URL of the git repository
        call get_value(table, "url", self%url)

        !> Additional descriptor of the git object
        call get_value(table,"object", self%object)

    end subroutine git_load_from_toml

    !> Parse git descriptor identifier from a string
    pure integer function parse_descriptor(name)
        character(len=*), intent(in) :: name

        select case (name)
           case ("default");  parse_descriptor = git_descriptor%default
           case ("branch");   parse_descriptor = git_descriptor%branch
           case ("tag");      parse_descriptor = git_descriptor%tag
           case ("revision"); parse_descriptor = git_descriptor%revision
           case default;      parse_descriptor = git_descriptor%error
        end select

    end function parse_descriptor

    !> Code git descriptor to a string
    pure function descriptor_name(descriptor) result(name)
       integer, intent(in) :: descriptor
       character(len=:), allocatable :: name

       select case (descriptor)
          case (git_descriptor%default);   name = "default"
          case (git_descriptor%branch);    name = "branch"
          case (git_descriptor%tag);       name = "tag"
          case (git_descriptor%revision);  name = "revision"
          case default;                    name = "ERROR"
       end select

    end function descriptor_name

  !> Archive a folder using `git archive`.
  subroutine git_archive(source, destination, ref, additional_files, verbose, error)
    !> Directory to archive.
    character(*), intent(in) :: source
    !> Destination of the archive.
    character(*), intent(in) :: destination
    !> (Symbolic) Reference to be archived.
    character(*), intent(in) :: ref
    !> (Optional) list of additional untracked files to be added to the archive.
    character(*), optional, intent(in) :: additional_files(:)
    !> Print additional information if true.
    logical, intent(in) :: verbose
    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    integer :: stat,i
    character(len=:), allocatable :: cmd_output, archive_format, add_files

    call execute_and_read_output('git archive -l', cmd_output, error, verbose)
    if (allocated(error)) return

    if (index(cmd_output, 'tar.gz') /= 0) then
      archive_format = 'tar.gz'
    else
      call fatal_error(error, "Cannot find a suitable archive format for 'git archive'."); return
    end if

    allocate(character(len=0) :: add_files)
    if (present(additional_files)) then 
       do i=1,size(additional_files)
          add_files = trim(add_files)//' --add-file='//adjustl(additional_files(i))
       end do
    endif

    call run('git archive '//ref//' &
        & --format='//archive_format// &
        & add_files//' &
        & -o '//destination, &
        & echo=verbose, &
        & exitstat=stat)
        
    if (stat /= 0) then
      call fatal_error(error, "Error packing '"//source//"'."); return
    end if
  end subroutine git_archive

end submodule