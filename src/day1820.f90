module day1820_mod
    use parse_mod, only : string_t, read_strings
    implicit none
    private
    public parse, crawler, maze_accessible, CH_ROOM

    type, public :: maze_t
        integer :: minpos(2)=0, maxpos(2)=0
        character(len=1), allocatable :: b(:,:)
    contains
        procedure :: add_door => maze_adddoor
        procedure :: alloc => maze_alloc
        procedure :: remove_unknowns => maze_remove_unknowns
        procedure :: display => maze_display
    end type

    integer, parameter :: MOVE(2,4) = reshape([0,-1, 0,1, 1,0, -1,0], [2,4])
    character(len=1), parameter :: MOVECH(4) = ['N', 'S', 'E', 'W']
    character(len=1), parameter :: CH_ROOM='.', CH_WALL='#', CH_UKNW='?', CH_DOOR(2)=['-','|']

contains

    recursive subroutine crawler(str, pos0, maze)
        character(len=*), intent(in) :: str
        integer, intent(in) :: pos0(2)
        type(maze_t), intent(inout) :: maze
!
! Parse the string to follow all paths in the maze. Should be called twice:
! (1) The corners of maze are identified
! (2) After the maze array is allocated, the maze is beeing drawn.
!
        type(string_t), allocatable :: paths(:)
        integer :: ind, iend, k, pos(2)
        logical :: is_loop, before_x
              
        if (len_trim(str)==0) error stop 'crawler - empty input'
        
        pos = pos0
        call update_maze_statistics                                              
        before_x = .true.
        ind = 1
        MAIN: do  
            if (ind > len(str)) exit MAIN
            select case(str(ind:ind))
            case('(')
                call parse(str(ind+1:),paths,iend)
                ind = ind + iend
                
                is_loop = .false.
                do k=1, size(paths)
                    if (len(paths(k)%str)== 0) then
                        is_loop = .true.
                    else
                        call crawler(paths(k)%str, pos, maze)
                    end if
                end do
                if (.not. is_loop) before_x = .false.
            
            case('E','W','N','S')
                ! if called after a cross-section, we do not know the current position
                if (.not. before_x) error stop 'crawler - position has been lost'
                
                ! add door and then move
                k = findloc(MOVECH, str(ind:ind), dim=1)
                if (k==0) error stop 'crawler - error'
                call maze%add_door(pos, k) 
                pos = pos + 2*MOVE(:,k)
                call update_maze_statistics
                ind = ind + 1
            
            case(')', '^', '$', '|')
                error stop 'crawler - this char should have been parsed-out'
            case default
                print *, '*'//str(ind:ind)//'*'
                error stop 'crawler - uknown character'
            end select 
        end do MAIN
    contains
        subroutine update_maze_statistics
            ! update minimum or maximum location
            where (maze%maxpos<pos) maze%maxpos = pos
            where (maze%minpos>pos) maze%minpos = pos
        end subroutine
    end subroutine crawler


    recursive subroutine parse(inpstr, parsed, iend, mode)
        character(len=*), intent(in) :: inpstr
        type(string_t), allocatable, intent(out) :: parsed(:)
        integer, intent(out) :: iend
        logical, intent(in), optional :: mode

        logical :: counting_mode
        integer :: scope, left_mark, nchunks

        counting_mode = .false.
        if (present(mode)) counting_mode = mode

        ! Run itself to count number of tokens and allocate output array to a required length
        if (.not. counting_mode) call parse(inpstr, parsed, iend, mode=.true.)

        iend = 0
        left_mark = 0
        scope = 0
        nchunks = 0

        MAIN: do 
            iend = iend + 1
            if (iend > len_trim(inpstr)) then
                if (left_mark /= 0) call write_chunk
                exit MAIN
            end if
            select case (inpstr(iend:iend))
            case('(','^') ! open scope
                if (inpstr(iend:iend)=='^' .and. scope /=0) error stop 'parse - character ^ not allowed inside scope'
                scope = scope + 1
                if (scope==1) then
                    left_mark = iend+1
                end if
            case(')','$') ! close scope
                if (inpstr(iend:iend)=='$' .and. scope /=1) error stop 'parse - character $ not allowed inside scope'                
                if (scope==1) then
                    call write_chunk
                    iend = iend + 1
                    exit MAIN
                end if
                scope = scope - 1
            case('|')     ! delimiter
                if (scope==1) then
                call write_chunk
                left_mark = iend+1
                end if
            case('E','W','N','S') ! regular character
                ! open scope, if given expression not in parentheses
                if (scope==0) then
                    if (iend /= 1) error stop 'parse - unexpected'
                    scope = 1
                    left_mark = iend
                end if 
            case default
                print *, inpstr(iend:iend), iend
                error stop 'parse - uknown token'
            end select
        end do MAIN        
        
        if (counting_mode) allocate(parsed(nchunks))
    contains
        subroutine write_chunk
            nchunks = nchunks + 1
            if (left_mark == 0) error stop 'parse - left mark not set'
            if (.not. counting_mode) then
                if (size(parsed)<nchunks) error stop 'parse - forgoten allocation?'
                parsed(nchunks) = string_t(inpstr(left_mark:iend-1))
            end if            
        end subroutine    
    end subroutine parse


    subroutine maze_adddoor(this, pos, idirection)
        class(maze_t), intent(inout) :: this
        integer, intent(in) :: pos(2), idirection
!
! Before moving through the door: add doors, door frame and both the
! rooms to the maze map.
!
        integer :: pos1(2), pos2(2), pos0(2)
        if (.not. allocated(this%b)) return
        if (any(pos>ubound(this%b)-1) .or. any(pos<lbound(this%b)+1)) &
            error stop 'maze_add door - out of bounds'

        ! given position: assumed before the step through door
        pos0 = pos + MOVE(:,idirection)
        select case (idirection)
        case(1,2) ! N/S            
            pos1 = pos0 + MOVE(:,3)
            pos2 = pos0 + MOVE(:,4)
            call safely_add(pos0(1),pos0(2),CH_DOOR(1))
            call safely_add(pos1(1),pos1(2),CH_WALL)
            call safely_add(pos2(1),pos2(2),CH_WALL)
        case(3,4) ! E/W
            pos1 = pos0 + MOVE(:,1)
            pos2 = pos0 + MOVE(:,2)
            call safely_add(pos0(1),pos0(2),CH_DOOR(2))
            call safely_add(pos1(1),pos1(2),CH_WALL)
            call safely_add(pos2(1),pos2(2),CH_WALL)
        case default
            error stop 'maze add door - direction invalid'
        end select

        ! add room before and after passed doors
        call safely_add(pos(1), pos(2), CH_ROOM)
        pos0 = pos0 + MOVE(:,idirection)
        call safely_add(pos0(1), pos0(2), CH_ROOM)

    contains
        subroutine safely_add(ii,jj,newch)
            integer, intent(in) :: ii, jj
            character(len=1), intent(in) :: newch
            if (this%b(ii,jj)==CH_UKNW) then
                this%b(ii,jj) = newch
            else if (this%b(ii,jj)==newch) then
                continue
            else
                print *,this%b(ii,jj)//' -> '//newch
                error stop 'maze_add door - rewriting unexpected'
            end if
        end subroutine
    end subroutine maze_adddoor


    subroutine maze_alloc(this)
        class(maze_t), intent(inout) :: this
!
! Allocate maze to the required size and draw outer walls
!
        allocate(this%b(this%minpos(1)-1:this%maxpos(1)+1, &
                        this%minpos(2)-1:this%maxpos(2)+1))
        this%b = CH_UKNW
        this%b(:,lbound(this%b,2)) = CH_WALL
        this%b(:,ubound(this%b,2)) = CH_WALL
        this%b(lbound(this%b,1),:) = CH_WALL
        this%b(ubound(this%b,1),:) = CH_WALL
    end subroutine


    subroutine maze_remove_unknowns(this)
        class(maze_t), intent(inout) :: this
        where(this%b==CH_UKNW) this%b=CH_WALL
    end subroutine


    subroutine maze_display(this)
        class(maze_t), intent(in) :: this
        integer :: i,j
        associate(b=>this%b)
        do j=lbound(b,2), ubound(b,2)
          do i=lbound(b,1), ubound(b,1)
            write(*,'(a1)',advance='no') b(i,j)
          end do          
          write(*,*)
        end do
        end associate
    end subroutine maze_display


    pure function maze_accessible(ch) result(is)
        character(len=1), intent(in) :: ch 
        logical :: is
        is = (any(ch==CH_DOOR) .or. ch==CH_ROOM)
    end function
end module