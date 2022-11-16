module day1822_mod
    use iso_fortran_env, only : I8 => int64
    implicit none

    integer(I8), parameter :: DEPTH = 5913, TX = 8, TY = 701
    !integer(I8), parameter :: DEPTH = 510, TX = 10, TY = 10
    integer(I8), parameter :: NULL_VAL = -1
    character(len=1), parameter :: CH_ROCK = '.', CH_NARR = '|', CH_WET='='

    type cave_t
        integer(I8), allocatable :: gi(:,:), el(:,:)
        character(len=1), allocatable :: type(:,:)
    contains
        procedure :: display => cave_display
        procedure :: risklevel => cave_risklevel
    end type
contains

    subroutine determine_gi(gi)
        integer(I8), allocatable, intent(out) :: gi(:,:)

        integer :: i, j
        allocate(gi(0:5*TX, 0:2*TY))
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