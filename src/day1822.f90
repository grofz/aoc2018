module day1822_mod
    use iso_fortran_env, only : I8 => int64
    implicit none

    integer(I8), parameter :: DEPTH = 5913, TX = 8, TY = 701
    !integer(I8), parameter :: DEPTH = 510, TX = 10, TY = 10
    integer(I8), parameter :: NULL_VAL = -1
    character(len=1), parameter :: CH_ROCK = '.', CH_NARR = '|', CH_WET='='
    integer, parameter :: ID_TORCH=1, ID_CLIMB=2, ID_NONE=3, D_STEP=1, D_CHGEAR=7

    type cave_t
        integer(I8), allocatable :: gi(:,:)
        character(len=1), allocatable :: type(:,:)
        logical, allocatable :: accessible(:,:,:)
    contains
        procedure :: display => cave_display
        procedure :: risklevel => cave_risklevel
    end type
contains
    subroutine djikstra(this, shortest)
        class(cave_t), intent(in) :: this
        integer, intent(out) :: shortest

        integer, allocatable :: d(:,:,:)
        logical, allocatable :: visited(:,:,:)
        integer :: nx, ny, ix, iy, ig, cur(3), j, counter
        nx = size(this%accessible,2)
        ny = size(this%accessible,3)
        allocate(d(3,0:nx-1,0:ny-1))
        allocate(visited(3,0:nx-1,0:ny-1))

        ! set starting point
        d = huge(d)
        d(ID_TORCH,0,0) = 0
        where (this%accessible)
            visited = .false.
        elsewhere
            visited = .true.
        end where

        counter = 0
        MAIN: do
            cur = lbound(d)+findmin(d, visited)-1 
            !print '("curr = ",i1,1x,i2,i2,3x,i0)',cur,d(cur(1),cur(2),cur(3))
            if (cur(1)==0) then
                print *, 'djikstra - all nodes visited'
                exit MAIN
            end if
            counter = counter+1
            if (mod(counter,10000)==0) then
            print *, 'unvisited ',count(.not. visited)
            end if

            ! update all neighbors (direction-wise and equipment-wise)
            associate(curx=>cur(2), cury=>cur(3), curg=>cur(1))
            ig = curg
            do ix = max(curx-1, 0), min(curx+1, nx-1)
            do iy = max(cury-1, 0), min(cury+1, ny-1)
                if (abs(cury-iy)+abs(curx-ix) /= 1) cycle
                if (visited(ig,ix,iy)) cycle
                if (d(ig,ix,iy)>d(curg,curx,cury)+D_STEP) then
                   !print '("addr = ",i1,1x,i2,i2,3x,i0,1x,i0)',ig,ix,iy,d(ig,ix,iy),d(curg,curx,cury)+D_STEP
                    d(ig,ix,iy)=d(curg,curx,cury)+D_STEP
                end if
            end do
            end do

            ix = curx
            iy = cury
           !ig = curg
           !do j=1,2
           !    ig = mod(ig,3)+1
            do ig=1,3
                if (ig==curg) cycle
                if (.not. visited(ig,ix,iy) .and. d(ig,ix,iy)>d(curg,curx,cury)+D_CHGEAR) then
                   ! print '("addr = ",i1,1x,i2,i2,3x,i0,1x,i0)',ig,ix,iy,d(ig,ix,iy),d(curg,curx,cury)+D_CHGEAR
                   d(ig,ix,iy)=d(curg,curx,cury)+D_CHGEAR
                end if

            end do
            !print *, 'node visited = ',curg,curx,cury, d(curg,curx,cury)
            !print *
            visited(curg, curx, cury) = .true.
            end associate

            if (visited(ID_TORCH, TX, TY)) then
                print *, 'djikstra - target found'
                exit MAIN
            end if
        end do MAIN

        shortest = d(ID_TORCH, TX, TY)
        return

        print *, 'Torch '
        do iy=lbound(d,3),ubound(d,3)
            print '(*(i4,1x))',(d(1,ix,iy),ix=lbound(d,2),ubound(d,2))
        end do
        print *, 'Climb '
        do iy=lbound(d,3),ubound(d,3)
            print '(*(i4,1x))',(d(2,ix,iy),ix=lbound(d,2),ubound(d,2))
        end do
        print *, 'Nothing '
        do iy=lbound(d,3),ubound(d,3)
            print '(*(i4,1x))',(d(3,ix,iy),ix=lbound(d,2),ubound(d,2))
        end do
    end subroutine


    function findmin(d, visited) result(minloc)
        integer, intent(in) :: d(:,:,:)
        logical, intent(in) :: visited(:,:,:)

        integer :: minloc(3), minval
        integer :: i, j, k
        minloc = 0
        minval = huge(minval)
        do i=1,size(d,1)
        do j=1,size(d,2)
        do k=1,size(d,3)
            if (visited(i,j,k)) cycle
            if (d(i,j,k) < minval) then
                minval = d(i,j,k)
                minloc = [i, j, k]
            end if
        end do
        end do
        end do
    end function

    subroutine determine_gi(gi)
        integer(I8), allocatable, intent(out) :: gi(:,:)

        integer :: i, j
        allocate(gi(0:8*TX, 0:2*TY))
        gi = NULL_VAL

        do i=0, ubound(gi,1)
        do j=0, ubound(gi,2)
            call gi_recursive(i,j,gi)
        end do
        end do
    end subroutine determine_gi

    recursive subroutine gi_recursive(i0,j0,gi)
        integer, intent(in) :: i0, j0
        integer(I8), intent(inout) :: gi(0:,0:)

        if (gi(i0,j0) /= NULL_VAL) return
        if (i0==0 .and. j0==0) then
            gi(i0,j0) = 0
        elseif (i0==TX .and. j0==TY) then
            gi(i0,j0) = 0
        elseif (j0==0) then
            gi(i0,j0) = i0*16807
        elseif (i0==0) then
            gi(i0,j0) = j0*48271
        else
            if (gi(i0-1,j0)==NULL_VAL) call gi_recursive(i0-1,j0,gi)
            if (gi(i0,j0-1)==NULL_VAL) call gi_recursive(i0,j0-1,gi)
            gi(i0,j0)=determine_el(gi(i0-1,j0))*determine_el(gi(i0,j0-1))
        end if
    end subroutine

    elemental function determine_el(gi) result(el)
        integer(I8), intent(in) :: gi
        integer(I8) :: el
        el = mod(gi + DEPTH, 20183)
    end function

    elemental function determine_type(el) result(type)
        integer(I8), intent(in) :: el
        character(len=1) :: type
        select case(mod(el,3))
        case(0)
            type = CH_ROCK
        case(1)
            type = CH_WET
        case(2)
            type = CH_NARR
        end select
    end function



    subroutine cave_display(this)
        class(cave_t), intent(in) :: this
        integer :: i, j
        do j=0,ubound(this%type,2)
            write(*,'(*(a1))') (this%type(i,j), i=0,ubound(this%type,1))
        end do
    end subroutine



    integer function cave_risklevel(this) result(rl)
        class(cave_t), intent(in) :: this

        rl = 0
        rl = count(this%type(0:TX,0:TY)==CH_WET) + 2*count(this%type(0:TX,0:TY)==CH_NARR)
    end function
end module day1822_mod