module day1816_mod
    use iso_fortran_env, only : IK=>int64
    implicit none

    character(len=4),parameter :: inst_list(0:15) = &
    & ['addr','addi','mulr','muli','banr','bani','borr','bori',&
    &  'setr','seti','gtir','gtri','gtrr','eqir','eqri','eqrr']

    integer, parameter :: NOPS = 16, NBITS=63, NREGTEST=4, nreg=4
    type, public :: computer_t
        integer(kind=IK), allocatable :: r(:)
        integer :: dict(0:NOPS-1)=[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
        logical :: isdebug = .false.
    contains
        procedure, private :: computer_exec, computer_exec_name
        generic :: exec => computer_exec, computer_exec_name
    end type
    interface computer_t
        module procedure computer_new
    end interface

    type, public :: test_t
        integer, dimension(1:NREGTEST) :: befo, inst, afte
    end type

    interface operator(.band.)
        module procedure op_band
    end interface

    interface operator(.bor.)
        module procedure op_bor
    end interface
contains
    type(computer_t) function computer_new(nreg0) result(new)
        integer, intent(in) :: nreg0
        allocate(new%r(0:nreg0-1))
    end function

    subroutine run_tests(tests, map, ans1)
        type(test_t), intent(in) :: tests(:)
        logical , intent(out):: map(0:NOPS-1,0:NOPS-1)
        integer, intent(out) :: ans1 
        
        integer :: i
        logical, allocatable :: results(:)
        map = .true.    
        ans1 = 0
        do i=1,size(tests)
            results = test_line(tests(i)%befo,tests(i)%inst,tests(i)%afte)
            where(.not. results) map(tests(i)%inst(1),:) = .false.            
            if (count(results)>=3) ans1 = ans1 + 1
        end do    
    end subroutine


    function decode_opcodes(map) result(decode)
        logical, intent(inout) :: map(0:NOPS-1,0:NOPS-1)
        integer :: decode(0:NOPS-1)

        integer :: i, ksafe
        
        decode = -1
        ksafe = 0
        do
            print *
            print '("Step ",i0,"    Not yet identified opcodes ",i0)', &
            & ksafe, count(decode<0)
            do i=0,NOPS-1
                print '("Opcode ",i2, 3x, *(a))', &
                & i, emptylog(map(i,:))
            end do
            print '(12x,*(i2,1x))', decode
            if (count(decode<0)==0) exit
            call map_reduce_step(map,decode)            
            ksafe = ksafe+1
            if (ksafe > 10) error stop 'decode opcodes - failed'
        end do
    contains
        elemental function emptylog(lg)
            logical, intent(in) :: lg
            character(len=3) :: emptylog
            emptylog = ' . '
            if (lg) emptylog =' T '
        end function
    end function


    subroutine map_reduce_step(map, decode)
        logical, intent(inout) :: map(:,:)
        integer, intent(inout) :: decode(:)
        
        integer :: i, j
        do i=1, size(map,1)
            if (count(map(i,:))==1) then               
                j = findloc(map(i,:),.true.,dim=1)
                if (decode(i) /= j-1) then                
                    map(:,j) = .false.
                    map(i,j) = .true.
                    print '("Opcode ",i2," identified as instruction ",i2)', &
                    &  i-1,j-1
                    decode(i) = j-1
                end if
            end if
        end do
    end subroutine

    subroutine read_tests(file, tests, code)
        use parse_mod, only : string_t, read_strings
        character(len=*), intent(in) :: file
        type(test_t), allocatable, intent(out) :: tests(:)
        integer, allocatable :: code(:,:)

        integer :: i,j
        type(string_t), allocatable :: lines(:)
        type(test_t) :: new

        lines = read_strings(file)
        allocate(tests(0))
        do i=1,size(lines),4
            if (len_trim(lines(i)%str)==0) exit
            if (lines(i)%str(1:9)/='Before: [') then
                print *, lines(i)%str(1:9)
                 error stop 'read_tests - before expected'
            end if
            read(lines(i)%str(10:19),*) new%befo
            read(lines(i+1)%str,*) new%inst
            read(lines(i+2)%str(10:19),*) new%afte
            tests = [tests, new]
        end do
        print *, 'Tests read ', size(tests)
        print *, i, lines(i)%str
        print *, i+1, lines(i+1)%str
        print *, i+2, lines(i+2)%str

        allocate(code(4,size(lines)-(i+2)+1))
        do j=i+2, size(lines)
            read(lines(j)%str,*) code(:,j-(i+2)+1)
        end do

    end subroutine


    function test_line(befo,inst,afte) result(valid)
        integer, intent(in), dimension(NREGTEST) :: befo, inst, afte
        logical :: valid(0:NOPS-1)

        type(computer_t) :: zx
        integer :: i

        zx = computer_t(4)
        do i=lbound(valid,1),ubound(valid,1)
            zx%dict = i
            zx%r = befo
            call zx%exec(inst)
            valid(i) = all(zx%r==afte)
        end do
    end function test_line


    subroutine computer_exec_name(this, inst_name, inst_rest)
        class(computer_t), intent(inout) :: this
        character(len=4), intent(in) :: inst_name
        integer, intent(in) :: inst_rest(3)

        integer :: i, j
        do i=0,NOPS-1
            if (inst_list(i)(1:4)==inst_name(1:4)) exit
        end do
        if (i>NOPS-1) then
            error stop 'instruction name not found'
        else
            do j=0,NOPS-1
                if (this%dict(j)==i) exit
            end do
            if (j<=NOPS-1) then
                call computer_exec(this,[j,inst_rest])
            else
                error stop 'computer - invalid dictionary'
            endif
        end if
    end subroutine


    subroutine computer_exec(this, inst)
        class(computer_t), intent(inout) :: this
        integer, intent(in) :: inst(4)

        integer(IK) :: op1, op2
        
        associate(a=>inst(2), b=>inst(3), c=>inst(4))
            select case(this%dict(inst(1)))
            case(0) ! addr
                op1 = this%r(a)
                op2 = this%r(b)
                this%r(c) = op1 + op2           
            case(1) ! addi
                op1 = this%r(a)
                op2 = b
                this%r(c) = op1 + op2
            case(2) ! mulr
                op1 = this%r(a)
                op2 = this%r(b)
                this%r(c) = op1 * op2
            case(3) ! muli
                op1 = this%r(a)
                op2 = b
                this%r(c) = op1 * op2
            case(4) ! banr
                op1 = this%r(a)
                op2 = this%r(b)
                this%r(c) = op1 .band. op2
            case(5) ! bani
                op1 = this%r(a)
                op2 = b
                this%r(c) = op1 .band. op2
            case(6) ! borr
                op1 = this%r(a)
                op2 = this%r(b)
                this%r(c) = op1 .bor. op2
            case(7) ! bori
                op1 = this%r(a)
                op2 = b
                this%r(c) = op1 .bor. op2
            case(8) ! setr
                op1 = this%r(a)
                this%r(c) = op1
            case(9) ! seti
                op1 = a
                this%r(c) = op1
            case(10) ! gtir
                op1 = a
                op2 = this%r(b)
                this%r(c) = 0
                if (op1 > op2) this%r(c) = 1
            case(11) ! gtri
                op1 = this%r(a)
                op2 = b
                this%r(c) = 0
                if (op1 > op2) this%r(c) = 1
            case(12) ! gtrr
                op1 = this%r(a)
                op2 = this%r(b)
                this%r(c) = 0
                if (op1 > op2) this%r(c) = 1
            case(13) ! eqir
                op1 = a
                op2 = this%r(b)
                this%r(c) = 0
                if (op1 == op2) this%r(c) = 1
            case(14) ! eqri
                op1 = this%r(a)
                op2 = b
                this%r(c) = 0
                if (op1 == op2) this%r(c) = 1
            case(15) ! eqrr
                op1 = this%r(a)
                op2 = this%r(b)
                this%r(c) = 0
                if (op1 == op2) this%r(c) = 1
            case default
                error stop 'exec - unknown opcode'
            end select
        end associate
        if (this%isdebug) write(*,'(a4,1x,3(i0,1x)," -> ",*(i0,1x))') &
        &   inst_list(this%dict(inst(1))),inst(2:4),this%r
    end subroutine computer_exec


    elemental function op_band(a,b) result(c)
        integer(IK), intent(in) :: a, b
        integer(IK) :: c
        c = b2i(i2b(a) .and. i2b(b))
    end function


    elemental function op_bor(a,b) result(c)
        integer(IK), intent(in) :: a, b
        integer(IK) :: c
        c = b2i(i2b(a) .or. i2b(b))
    end function


    pure function i2b(i) result(b)
        integer(IK), intent(in) :: i
        logical :: b(NBITS)
        integer(IK) :: j1, j2
        integer :: k

        b = .false.
        j1 = i
        do k=1,NBITS
            j2 = j1 / 2_IK**(NBITS-k)
            if (j2>1) error stop 'i2b - value to large'
            if (j2==1) b(k)=.true.
            j1 = j1 - j2*2_IK**(NBITS-k)
        end do
    end function


    pure function b2i(b) result(i)
        logical, intent(in) :: b(:)
        integer(IK) :: i
        integer :: k
        i = 0
        do k=1,size(b)
            if (b(k)) i = i + 2**(size(b)-k)
        end do 
    end function

end module day1816_mod