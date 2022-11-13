module day1817_mod
    use parse_mod, only : string_t, split, read_strings
    implicit none

    character, parameter :: CH_FREZ='~', CH_FLOW='|', CH_SAND='.', CH_CLAY='#', CH_SPRING='+'

    ! Spring object
    type spring_t
        integer :: xy(2)
        integer :: tm
        logical :: is_active = .false.
        integer :: id = -1
    contains
        procedure :: onestep => spring_onestep
        procedure :: report => spring_report
    end type
    interface spring_t
        module procedure spring_new
    end interface

    ! Map
    ! - statistics (number of water/ running water)
    type, public :: map_t
        character(len=1), allocatable :: b(:,:)
        integer :: mxpos(2), mnpos(2)
        type(spring_t), allocatable :: springs(:)
        integer :: nsprings=0
        integer :: idcounter=0
        integer :: isteps=0
    contains
        procedure :: display => map_display
        procedure :: fill => map_fill
    end type
    interface map_t
        module procedure map_new
    end interface

    type instruction_t
        integer :: p1(2), p2(2)
    end type
    interface instruction_t
        module procedure instruction_new
    end interface

contains

    !=============================================================
    ! Spring object
    !=============================================================
    
    subroutine spring_onestep(this, map)
        class(spring_t), intent(inout) :: this
        type(map_t), intent(inout) :: map

        integer :: xleft, xright
        if (.not. this%is_active) return

        ! Check the current position:
        if (map%b(this%xy(1),this%xy(2))==CH_FREZ) then
            !call this%report('is now under freezed water (move-up)')
            this%xy(2)=this%xy(2)-1
            return
        end if
        ! if reached minimum: delete the spring
        if (this%xy(2)>map%mxpos(2)) then
            call this%report('reached the bottom of the cave (delete)')
            this%is_active = .false.
        end if
        if (map%b(this%xy(1),this%xy(2))==CH_CLAY) then
            error stop 'spring in clay'
        end if

        if (.not. this%is_active) return

        ! Check the pixel bellow 
        select case(map%b(this%xy(1),this%xy(2)+1))
        case(CH_SAND, CH_FLOW) ! if penetrable move down
            map%b(this%xy(1),this%xy(2))=CH_FLOW
            this%xy(2) = this%xy(2)+1

        case(CH_CLAY, CH_FREZ) ! if not penetrable: resolve left / right margins
            xleft = scanbed(map, this%xy, -1)
            xright = scanbed(map, this%xy, 1)
            associate(cl=>map%b(xleft,this%xy(2)), cr=>map%b(xright,this%xy(2)))

            if (cl==CH_CLAY .and. cr==CH_CLAY) then
                ! closed/closed: freeze water layer, move up
                map%b(xleft+1:xright-1,this%xy(2))=CH_FREZ
                this%xy(2)=this%xy(2)-1

            else if (cl==CH_CLAY .and. cr==CH_SAND) then
                ! closed/open: move over the ridge
                map%b(xleft+1:xright-1,this%xy(2))=CH_FLOW
                this%xy(1)=xright
            else if (cl==CH_SAND .and. cr==CH_CLAY) then
                ! open/closed: move over the ridge
                map%b(xleft+1:xright-1,this%xy(2))=CH_FLOW
                this%xy(1)=xleft

            else if (cl==CH_SAND .and. cr==CH_SAND) then
                ! open/open: move over the ridge, create a new spring at the other end
                map%b(xleft+1:xright-1,this%xy(2))=CH_FLOW
                this%xy(1)=xleft
                map%idcounter = map%idcounter+1
                call sparr_add(map%springs, map%nsprings,&
                &   spring_t([xright,this%xy(2)], this%tm, map%idcounter))
                call map%springs(map%nsprings)%report('forked of from other spring')

            else if (cl==CH_FLOW .and. cr==CH_SAND) then
                ! runnning water/open: ignore running water side
                map%b(xleft+1:xright-1,this%xy(2))=CH_FLOW
                this%xy(1)=xright
            else if (cl==CH_SAND .and. cr==CH_FLOW) then
                ! open/runnning water: ignore running water side
                map%b(xleft+1:xright-1,this%xy(2))=CH_FLOW
                this%xy(1)=xleft

            else if ((cl==CH_FLOW .and. cr==CH_CLAY) .or. &
                     (cl==CH_CLAY .and. cr==CH_FLOW)) then
                ! closed/running water: delete the spring
                call this%report('run into flowing water (deleted)')
                this%is_active = .false.
               ! if (cl==CH_FLOW) then
               !     this%xy(1)=xleft
               ! else
               !     this%xy(1)=xright
               ! end if
            else if (cl==CH_FLOW .and. CR==CH_FLOW) then
                this%is_active=.false.
                call this%report('flowing water on both sides (deleted)')
            else
                ! should not run into still water during left-right resolving
                this%is_active=.false.
                call map%display(this%xy(2)+5)
                print *, 'A'//cl,cr
                error stop 'spring_onestep - unexpected branch'
            end if
            end associate
        case default
            error stop 'spring_onestep - uknown char'
        end select
    end subroutine


    subroutine spring_report(this,message)
        class(spring_t), intent(in) :: this
        character(len=*), intent(in) :: message
        write(*,'("id ",i0," at ",i0,",",i0,2x,a)') this%id, this%xy, message
    end subroutine


    function spring_new(xy, tm, id) result(new)
        type(spring_t) :: new
        integer, intent(in) :: xy(2), tm, id

        new%xy = xy
        new%tm = tm
        new%id = id
        new%is_active = .true.
    end function


    !=============================================================
    ! Map
    ! - initialize using instructions (x-layer, y-layer)
    ! - visualize
    !=============================================================

    subroutine map_fill(this)
        class(map_t), intent(inout) :: this

        integer :: is
        do
            print '(a,i0,a,*(i0,1x))', 'Step ',this%isteps,' begins with springs ', (this%springs(is)%id,is=1,this%nsprings)
            if (this%nsprings==0) exit
            is = 1
            do 
                if (is > this%nsprings) exit
                call this%springs(is)%onestep(this)
                if (.not. this%springs(is)%is_active) then
                    call sparr_remove(this%springs, this%nsprings, is)
                else
                    is = is + 1
                end if
            end do
            call sparr_remdup(this%springs, this%nsprings)
            this%isteps = this%isteps+1
        end do
    end subroutine


    function map_new(inst_arr) result(new)
        type(map_t) :: new
        class(instruction_t), intent(in) :: inst_arr(:)

        integer :: i
        new%mxpos = -huge(new%mxpos)
        new%mnpos = huge(new%mxpos)
        do i=1,size(inst_arr)
            if (inst_arr(i)%p1(1) < new%mnpos(1)) new%mnpos(1)=inst_arr(i)%p1(1)
            if (inst_arr(i)%p1(2) < new%mnpos(2)) new%mnpos(2)=inst_arr(i)%p1(2)
            if (inst_arr(i)%p2(1) < new%mnpos(1)) new%mnpos(1)=inst_arr(i)%p2(1)
            if (inst_arr(i)%p2(2) < new%mnpos(2)) new%mnpos(2)=inst_arr(i)%p2(2)
            if (inst_arr(i)%p1(1) > new%mxpos(1)) new%mxpos(1)=inst_arr(i)%p1(1)
            if (inst_arr(i)%p1(2) > new%mxpos(2)) new%mxpos(2)=inst_arr(i)%p1(2)
            if (inst_arr(i)%p2(1) > new%mxpos(1)) new%mxpos(1)=inst_arr(i)%p2(1)
            if (inst_arr(i)%p2(2) > new%mxpos(2)) new%mxpos(2)=inst_arr(i)%p2(2)
        end do
        associate(mn=>new%mnpos, mx=>new%mxpos)
            allocate(new%b(mn(1)-1:mx(1)+1, 0:mx(2)+1))
            if (mn(2)<=0) error stop 'map_new - clay at spring level or above'
        end associate

        ! set map layout
        new%b = CH_SAND
        do i=1,size(inst_arr)
            associate(p1=>inst_arr(i)%p1, p2=>inst_arr(i)%p2)
                new%b(p1(1):p2(1), p1(2):p2(2)) = CH_CLAY
            end associate
        end do

        ! add the first spring
        new%nsprings = 0
        new%idcounter = 1
        call sparr_add(new%springs, new%nsprings, spring_t([500,0],0,new%idcounter))
    end function


    subroutine map_display(this,maxy)
        class(map_t), intent(in) :: this
        integer, intent(in), optional :: maxy

        character(len=1) :: cop(lbound(this%b,1):ubound(this%b,1), lbound(this%b,2):ubound(this%b,2))
        integer :: i, j, is, maxy0
        cop = this%b
        do is=1,this%nsprings
            associate(x=>this%springs(is)%xy(1), y=>this%springs(is)%xy(2))
                if (this%springs(is)%is_active) then
                    cop(x,y)=CH_SPRING 
                else 
                    cop(x,y)='Z'
                end if
            end associate
        end do
        maxy0 = ubound(this%b,2)
        if (present(maxy)) maxy0 = maxy
        do j=lbound(this%b,2),maxy0  !ubound(this%b,2)
            write(*,'(*(a1))') (cop(i,j),i=lbound(this%b,1),ubound(this%b,1))
        end do
    end subroutine map_display



    !=============================================================
    ! Auxilary
    ! - parse input file, create instructions
    ! - get left/right type of ridge (examine the ridge)
    !=============================================================

    subroutine read_from_file(file, instr)
        character(len=*), intent(in) :: file
        type(instruction_t), allocatable, intent(out) :: instr(:)

        type(string_t), allocatable :: lines(:)
        integer :: i
        lines = read_strings(file)
        allocate(instr(size(lines)))
        do i=1,size(lines)
            instr(i) = instruction_t(lines(i)%str)
            !print '("P1 =",i5,1x,i5,"    P2 = ",i5,1x,i5)', &
            !instr(i)%p1, instr(i)%p2
        end do
    end subroutine


    type(instruction_t) function instruction_new(str) result(new)
        character(len=*), intent(in) :: str
        type(string_t), allocatable :: toks(:), scal(:), vec(:)
        integer :: iscal, ivec

        ! Sample example
        !x=495, y=2..7
        !y=7, x=495..501

        call split(str,' ',toks)
        if (size(toks)/=2) error stop 'instruction new - error 1'
        select case(toks(1)%str(1:1))
        case('x')
            iscal = 1
        case('y')
            iscal = 2
        case default
            error stop 'instruction new - error 2a'
        end select
        select case(toks(2)%str(1:1))
        case('x')
            ivec = 1
        case('y')
            ivec = 2
        case default
            error stop 'instruction new - error 2a'
        end select
        if (ivec==iscal) error stop 'instruction new - error 3'
        call split(toks(1)%str(3:), ',', scal)
        call split(toks(2)%str(3:), '.', vec)
        if (size(scal)/=2) error stop 'instruction new - error 4'
        if (size(vec) /=3) error stop 'instruction new - error 5'

        read(scal(1)%str,*) new%p1(iscal)
        read(scal(1)%str,*) new%p2(iscal)
        read(vec(1)%str,*) new%p1(ivec)
        read(vec(3)%str,*) new%p2(ivec)
    end function


    function scanbed(this, pos, dir) result(xend)
        type(map_t), intent(in) :: this
        integer, intent(in) :: pos(2), dir
        integer :: xend
!
! Moving to the set direction as long as possible. Returns
! the x-coordinate of the first pixel after the allowed
! move.
!
        xend = pos(1)
        if (abs(dir) /= 1) error stop 'scanbed - dir must be +1/-1'
        ! move to the selected direction...
        do 
            !...until bumping into wall
            select case(this%b(xend,pos(2)))
            case(CH_SAND, CH_FLOW)
            case(CH_CLAY, CH_FREZ)
                exit
            case default
                error stop 'scanbed - unespected char'
            end select
            !...or loosing solid bed below
            select case(this%b(xend,pos(2)+1))
            case(CH_CLAY, CH_FREZ)
            case(CH_SAND, CH_FLOW)
                exit
            case default
                error stop 'scanbed - unespected char2'
            end select
            xend = xend + dir
        end do
    end function



    !=============================================================
    ! Manage the field of springs
    ! - sort active / add new / remove deleted / expand / contract
    !=============================================================

    subroutine sparr_add(arr,n,item)
        type(spring_t), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        type(spring_t), intent(in) :: item

        integer :: nmax
        type(spring_t), allocatable :: warr(:)
        if (.not. allocated(arr)) allocate(arr(20))
        nmax = size(arr,1)
        n = n + 1
        if (n <= nmax) then
            arr(n) = item
        else
            ! expand array
            allocate(warr(2*nmax))
            warr(1:n-1) = arr(1:n-1)
            warr(n) = item
            deallocate(arr)
            call move_alloc(warr, arr)
            print *, 'array expansion to ', size(arr,1), ' items currently', n
        end if
    end subroutine


    subroutine sparr_remove(arr, n, rem_ind)
        type(spring_t), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        integer, intent(in) :: rem_ind

        integer :: nmax
        type(spring_t), allocatable :: warr(:)
        arr(rem_ind) = arr(n)
        n = n - 1
        nmax = size(arr,1)
        if (nmax > 4*max(n, 20)) then
            ! contract array
            allocate(warr(nmax/2))
            warr(1:n) = arr(1:n)
            deallocate(arr)
            call move_alloc(warr, arr)
            print *, 'array contracted to ', size(arr,1), ' items currently', n
        end if
    end subroutine


    subroutine sparr_remdup(arr, n)
        type(spring_t), intent(inout), allocatable :: arr(:)
        integer, intent(inout) :: n
!
! Remove duplicit springs
!
        integer :: i, j, xy(2)
        i = 1
        do
            if (i > n) exit
            if (.not. arr(i)%is_active) error stop 'remdup - inactive spring'
            xy = arr(i)%xy
            j = i+1
            do 
                if (j > n) exit
                if (all(arr(j)%xy==xy)) then
                    call arr(i)%report('has a duplicit spring...')
                    call arr(j)%report('...will be deleted')
                    call sparr_remove(arr, n, j)
                else 
                    j = j+1
                end if
            end do
            i = i+1
        end do
    end subroutine
end module
    