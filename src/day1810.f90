module day1810_mod
    use parse_mod, only : string_t, read_strings, split
    implicit none
    private

    !logical, parameter :: DEBUG = .true.
    logical, parameter :: DEBUG = .false.

    type, public :: stars_t
        private
        integer, allocatable, dimension(:) :: x, y, vx, vy
        integer :: time = 0
    contains
        procedure :: add => stars_add
        procedure :: move => stars_move
        procedure :: limits => stars_limits
        procedure :: align => stars_align
        procedure :: view => stars_view
        procedure :: print => stars_print
        procedure :: gettime => stars_gettime
    end type 
    
    interface stars_t
        module procedure stars_init
        module procedure stars_readfile
    end interface

contains

   function stars_init() result(new)
       type(stars_t) :: new
       allocate(new%x(0), new%y(0), new%vx(0), new%vy(0))
   end function


   function stars_readfile(file) result(new)
       type(stars_t) :: new
       character(len=*), intent(in) :: file
       type(string_t), allocatable :: lines(:), s1(:), s2(:), s3(:), s4(:), s5(:)
       integer :: i, x, y, vx, vy
       lines = read_strings(file)
       new = stars_init()
       do i=1,size(lines)
           call split(lines(i)%str,'<',s1)
           if (size(s1)/=3) error stop 'readfile - error s1'
           call split(s1(2)%str,',',s2)
           call split(s1(3)%str,',',s3)
           if (size(s2)/=2) error stop 'readfile - error s2'
           if (size(s3)/=2) error stop 'readfile - error s3'
           call split(s2(2)%str,'>',s4)
           call split(s3(2)%str,'>',s5)
           read(s2(1)%str,*) x
           read(s3(1)%str,*) vx
           read(s4(1)%str,*) y
           read(s5(1)%str,*) vy
           call new%add(x,y,vx,vy)
       end do

       if (.not. DEBUG) return
       do i=1,size(new%x)
           print *, new%x(i), new%y(i), new%vx(i), new%vy(i)
       end do
   end function


   subroutine stars_add(this,x,y,vx,vy)
       class(stars_t), intent(inout) :: this
       integer, intent(in) :: x,y,vx,vy
       this%x = [this%x, x]
       this%y = [this%y, y]
       this%vx = [this%vx, vx]
       this%vy = [this%vy, vy]
   end subroutine


   subroutine stars_move(this,dt)
       class(stars_t), intent(inout) :: this
       integer, intent(in) :: dt
       this%x = this%x + this%vx * dt
       this%y = this%y + this%vy * dt
       this%time = this%time + dt
   end subroutine


   function stars_limits(this) result(limits)
       class(stars_t), intent(in) :: this
       integer :: limits(4)
       limits(1) = minval(this%x)
       limits(2) = minval(this%y)
       limits(3) = maxval(this%x)
       limits(4) = maxval(this%y)
   end function


   subroutine stars_align(this)
       class(stars_t), intent(inout) :: this       
       integer :: lims(4), oldlims(4), i
       integer, parameter :: MAXSTEPS=100000

       oldlims = [-huge(i),-huge(i),huge(i),huge(i)]
       do i=1,MAXSTEPS
           lims = this%limits()
           if (DEBUG) print *, 'aligning... ', this%time, lims
           if (lims(1)<oldlims(1)) then
               call this%move(-1)
               exit
           end if
           oldlims=lims
           call this%move(1)
       end do
       if (i==MAXSTEPS+1) error stop 'align failed - too many steps'
    end subroutine


    subroutine stars_view(this, view)
        class(stars_t), intent(in) :: this
        character(len=1), allocatable, intent(out) :: view(:,:)

        integer :: i, j, ip, lims(4)
    
        lims = this%limits()
        allocate(view(lims(3)-lims(1)+1, lims(4)-lims(2)+1))
        view = ' '
        do ip=1, size(this%x)
            i = this%x(ip) - lims(1) + 1
            j = this%y(ip) - lims(2) + 1
            view(i,j)(1:1) = '#'
        end do
    end subroutine


    subroutine stars_print(this)
        class(stars_t), intent(in) :: this
        character(len=1), allocatable :: view(:,:)

        integer :: i,j
        call this%view(view)

        write(*,*)
        do j=1,size(view,2)
            do i=1,size(view,1)
                write(*,'(a)',advance='no') view(i,j)
            end do
            write(*,*)
        end do
    end subroutine 


    pure function stars_gettime(this) result(time)
        class(stars_t), intent(in) :: this
        integer :: time
        time = this%time
    end function

end module day1810_mod
