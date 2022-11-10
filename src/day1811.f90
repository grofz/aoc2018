module day1811_mod
    implicit none
    private
    public find_max_square, power_level

    integer, parameter, public :: NMAX = 300

    type, public :: grid_t
        integer :: pl(NMAX,NMAX)
    end type

    interface grid_t
        module procedure grid_new
    end interface
contains

    pure function grid_new(sernum) result(new)
        integer, intent(in) :: sernum
        type(grid_t) :: new
        integer :: i,j
        do i=1,NMAX
        do j=1,NMAX
            new%pl(i,j) = power_level(i,j,sernum)
        end do
        end do
    end function


    pure function power_level(x,y,sernum) result(pl)
        integer, intent(in) :: x,y,sernum
        integer :: pl

        integer :: rack

        rack = x+10
        pl = rack*y
        pl = pl + sernum
        pl = pl * rack
        pl = hundred(pl)
        pl = pl - 5
    end function


    pure function hundred(x) result(h)
        integer, intent(in) :: x
        integer :: h
        h = x/100
        h = mod(h,10)
        !print '(i0,"-->",i0)', x, h
    end function


    function find_max_square(this,sz) result(pos)
        type(grid_t), intent(in) :: this
        integer, intent(in) :: sz
        integer :: pos(3)
        integer :: mx, i, j, pl

        mx = -huge(mx)
        pos = -1
        do i=1, NMAX-sz+1
        do j=1, NMAX-sz+1
            pl = sum(this%pl(i:i+sz-1,j:j+sz-1))
            if (pl > mx) then
                mx = pl
                pos(1:2) = [i, j]
                pos(3) = mx
            end if
        end do
        end do
    end function

end module
