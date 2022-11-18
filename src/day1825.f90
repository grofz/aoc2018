module day1825_mod
    use parse_mod, only : string_t, read_strings
    implicit none

    type spacepoint_t
        integer :: x(4)
        integer :: lab
    end type spacepoint_t
    interface spacepoint_t
        module procedure spacepoint_new
    end interface

    interface operator(+)
        module procedure manhattan
    end interface

contains

    subroutine connect(arr,ncon)
        type(spacepoint_t), intent(inout) :: arr(:)
        integer, intent(out) :: ncon

        integer, parameter :: LIMDIS = 3
        integer :: i, j, n, newlab, oldlab

        n = size(arr)
        do i=1,n
            arr(i)%lab = i
        end do

        do i=1,n-1
            do j=i+1,n
                if (arr(i)%lab == arr(j)%lab) cycle
                if (arr(i) + arr(j) > LIMDIS) cycle
                newlab = min(arr(i)%lab, arr(j)%lab)
                oldlab = max(arr(i)%lab, arr(j)%lab)
                where(arr%lab==oldlab)
                    arr%lab = newlab
                end where
            end do
        end do

        ncon = 0
        do i=1,n
            if (count(arr%lab==i)> 0) ncon = ncon + 1
        end do
    end subroutine

    subroutine read_spacepoints(file, arr)
        character(len=*), intent(in) :: file
        type(spacepoint_t), allocatable, intent(out) :: arr(:)
        
        type(string_t), allocatable :: lines(:)
        integer :: n, i
        lines = read_strings(file)
        n = size(lines)
        allocate(arr(n))
        do i=1, n
            arr(i) = spacepoint_t(lines(i)%str)
            arr(i)%lab=i
        end do
    end subroutine


    type(spacepoint_t) function spacepoint_new(str) result(new)
        character(len=*), intent(in) :: str
        read(str,*) new%x
    end function


    pure function manhattan(a,b) result(man)
        type(spacepoint_t), intent(in) :: a, b
        integer :: man

        man = sum(abs(a%x-b%x))
    end function
end module day1825_mod
