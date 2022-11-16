module day1819_mod
    use day1816_mod, only : computer_t
    implicit none
    private
    public faster

    integer, parameter :: NREG=6

    type, public, extends(computer_t) :: compver2_t
        integer :: ip = 0     ! instruction pointer (IP)
        integer :: ipbind     ! register bound to IP
        integer, allocatable :: iargs(:,:) ! (1:3, 0:ninstr)
        character(len=4), allocatable :: iops(:) ! (o:ninstr)
        logical :: halted = .false.
        integer :: counter = 0
    contains
        procedure :: onestep => compver2_onestep
        procedure :: reset => compver2_reset
        procedure :: list => compver2_list
    end type
    interface compver2_t
        module procedure compver2_new
    end interface
contains
    
    function compver2_new(file) result(new)
        use parse_mod, only : string_t, read_strings
        character(len=*), intent(in) :: file
        type(compver2_t) :: new

        type(string_t), allocatable :: lines(:)
        integer :: n, i

        allocate(new%r(0:NREG-1))
        new%r = 0
        lines = read_strings(file)
        if (lines(1)%str(1:3) /= '#ip') error stop 'reading program - invalid format'
        read(lines(1)%str(4:),*) new%ipbind
        print '("Instruction pointer bound to register ",i0)',new%ipbind
        n = size(lines)-1
        allocate(new%iargs(3,0:n-1), new%iops(0:n-1))
        do i=0,n-1
            new%iops(i)=lines(i+2)%str(1:4)
            read(lines(i+2)%str(6:),*) new%iargs(:,i)
        end do
        print '("Instructions read ",i0)', n
    end function


    subroutine compver2_onestep(this)
        class(compver2_t), intent(inout) :: this

        if (this%ip > ubound(this%iops,1) .or. this%ip < 0) then
            print '("Computer halts, ip = ",i0," is outside instructions range")', this%ip
            this%halted = .true.
            return
        end if

        ! send IP to the bound register
        this%r(this%ipbind) = this%ip

        ! execute the instruction
        associate(ip=>this%ip)
            if (this%isdebug) print '("Executing ip = ",i0)',ip
            call this%exec(this%iops(ip),this%iargs(:,ip))
        end associate

        ! receive IP and update it for the next step
        this%ip = this%r(this%ipbind) 
        this%ip = this%ip + 1
        this%counter = this%counter + 1
    end subroutine



    subroutine compver2_reset(this)
        class(compver2_t), intent(inout) :: this

        this%ip = 0
        this%halted = .false.
        this%counter = 0
        this%r = 0
    end subroutine



    subroutine compver2_list(this)
        class(compver2_t), intent(in) :: this

        integer :: i
        character(len=1) :: ch_curr
        if (.not. allocated(this%iops)) then
            print '("Program not loaded yet")'
            return
        end if
        do i=0,ubound(this%iops,1)
            ch_curr=' '
            if (i==this%ip) ch_curr = '*'
            print '(a1,"[",i2,"] ",a4,2x,*(i0,1x))', &
            & ch_curr, i, this%iops(i), this%iargs(:,i)
        end do
    end subroutine



    function faster(r3) result(r0)
        integer, intent(in) :: r3
        integer :: r0

        integer :: i        
        r0 = 0
        do i=1,r3
            if (mod(r3,i)==0) r0=r0+i
        end do
    end function

end module