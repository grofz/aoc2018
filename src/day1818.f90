module day1818_mod
    use parse_mod, only : string_t, read_strings
    implicit none
    private

    type, public :: board_t
        character(len=1), allocatable :: b(:,:)
        integer :: nx=0, ny=0
        integer :: time=0
    contains
        procedure :: display => board_display
        procedure :: count_ngbs
        procedure :: evolve => board_evolve
        procedure :: resval => board_resval
    end type
    interface board_t
        module procedure board_new
    end interface

    character(len=1), parameter :: CH_OPEN='.', CH_TREE='|', CH_LUMB='#', CH_NULL='0'


contains

    type(board_t) function board_new(file) result(new)
        character(len=*), intent(in) :: file
        type(string_t), allocatable :: lines(:)

        integer :: nx, ny, i, j
        lines = read_strings(file)
        ny = size(lines)
        if (ny>0) then
            nx = len_trim(lines(1)%str)
        else
            error stop 'boartd_new - empty input file'
        end if
        allocate (new%b(0:nx+1,0:ny+1))
        new%b = CH_NULL
        do j=1,ny
            do i=1,len_trim(lines(j)%str)
                new%b(i,j) = lines(j)%str(i:i)
            end do
        end do
        new%nx = nx
        new%ny = ny
    end function



    subroutine board_display(this)
        class(board_t), intent(in) :: this
        integer :: i, j
        do j=1, this%ny 
            write(*,'(*(a1))') (this%b(i,j),i=1,this%nx)
        end do
    end subroutine



    function count_ngbs(this,ch) result(arr)
        class(board_t), intent(in) :: this
        character(len=1), intent(in) :: ch
        integer :: arr(1:this%nx,1:this%ny)

        integer :: i, j, di, dj
        arr = 0
        do i=1,this%nx
            do j=1,this%ny
                do di=i-1,i+1
                    do dj=j-1,j+1
                        if (i==di .and. j==dj) cycle
                        if (this%b(di,dj)==ch) arr(i,j)=arr(i,j)+1
                    end do
                end do
            end do
        end do
    end function



    subroutine print_array(arr)
        integer, intent(in) :: arr(:,:)
        integer :: i, j
        do j=1,size(arr,2)
            write(*,'(*(i2))') (arr(i,j),i=1,size(arr,1))
        end do
        write(*,*)
    end subroutine



    subroutine board_evolve(this)
        class(board_t), intent(inout) :: this

        integer, allocatable :: ntrees(:,:), nlumbs(:,:)

        ntrees = this%count_ngbs(CH_TREE)
        nlumbs = this%count_ngbs(CH_LUMB)
        associate(b=>this%b(1:this%nx, 1:this%ny))
        where(b==CH_OPEN .and. ntrees >= 3)
            b = CH_TREE
        else where(b==CH_TREE .and. nlumbs >= 3)
            b = CH_LUMB
        else where(b==CH_LUMB .and. (nlumbs<1 .or. ntrees<1))
            b = CH_OPEN
        end where
        end associate
        this%time = this%time+1
    end subroutine



    integer function board_resval(this) result(resval)
        class(board_t), intent(in) :: this

        integer :: nlumbs, ntrees
        nlumbs = count(this%b==CH_LUMB)
        ntrees = count(this%b==CH_TREE)
        resval = nlumbs*ntrees
    end function
end module 