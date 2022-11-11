module day1813_mod
    implicit none

    type mine_t
        character(len=1), allocatable :: board(:,:)
        type(cart_t), allocatable :: carts(:)
        integer :: ncarts = 0
        integer :: tick = 0
    contains
        procedure :: displayboard => mine_displayboard
        procedure :: movecarts => mine_movecarts
    end type

    interface mine_t
        module procedure mine_new
    end interface

    type cart_t
        integer :: id = -1
        integer :: pos(2)
        integer :: ori  
        integer :: crossing = 1
        logical :: crashed = .false. 
    contains
        procedure :: move => cart_move
        procedure :: turn => cart_turn
        procedure :: collide => cart_collide
        procedure, private :: cart_greater_than
        generic :: operator(>) => cart_greater_than
    end type

    interface cart_t
        module procedure cart_new
    end interface

    character(len=1), parameter :: CHCAR(4) = ['^', '>', 'v', '<'], &
    &   CHVER='|', CHHOR='-', CHNW='/', CHNE='\', CHCR='+'
    integer, parameter :: DIR(2,4) = reshape([0,-1, 1,0, 0,1, -1,0],[2,4]), &
    &   LEFT_TURN=-1, RIGHT_TURN=+1, ORI_UP=1, ORI_RIGHT=2, ORI_DOWN=3, ORI_LEFT=4

contains
    function mine_new(file) result(new)
        use parse_mod, only : string_t, read_strings
        type(mine_t) :: new
        character(len=*), intent(in) :: file
        type(string_t), allocatable :: lines(:)
        integer :: maxcol, i, j, ncarts

        lines = read_strings(file)
        maxcol = 0
        do i=1,size(lines)
            associate(nn=>len_trim(lines(i)%str))
                if(nn > maxcol) maxcol = nn
            end associate
        end do
        allocate(new%board(maxcol,size(lines)))
        new%board = ' '
        ncarts = 0
        allocate(new%carts(0))
        do j=1,size(lines)
            do i=1,len_trim(lines(j)%str)
                associate(ch=>lines(j)%str(i:i))
                    if (any(ch==CHCAR)) then
                        ncarts = ncarts + 1
                        new%carts = [new%carts, cart_t(ncarts,[i,j],ch)]
                        new%board(i,j)=track_under_cart(ch)
                        print *, 'cart -',new%carts(ncarts)
                    else
                        new%board(i,j)=ch
                    end if
                end associate
            end do
        end do
        new%ncarts = ncarts
        print '("Board size =",i0,1x,i0,"  Carts =",i0)', &
        &   shape(new%board), new%ncarts
    end function


    pure function cart_new(id, pos, ch) result(new)
        type(cart_t) :: new
        integer, intent(in) :: id, pos(2)
        character(len=1), intent(in) :: ch
        integer :: i
        new%id = id
        new%pos = pos
        do i=1,4
            if (ch==CHCAR(i)) then
                new%ori=i
                exit
            end if
        end do
        if (i==5) error stop 'cart_new - orientation not recognized'
    end function


    pure character(len=1) function track_under_cart(ch) result(res)
        character(len=1), intent(in) :: ch
        select case(ch)
        case (CHCAR(1),CHCAR(3))
            res=CHVER
        case (CHCAR(2),CHCAR(4))
            res=CHHOR
        case default
            error stop 'track_under_cart - not a cart'
        end select
    end function


    subroutine mine_displayboard(this)
        class(mine_t), intent(in) :: this
        integer :: i,j
        associate(b=>this%board)
            do j=lbound(b,2), ubound(b,2)
                write(*,'(*(a1))') (b(i,j),i=lbound(b,1),ubound(b,1))
            end do
        end associate
    end subroutine


    subroutine mine_movecarts(this, first_crash, first_crash_pos)
        class(mine_t), intent(inout) :: this
        logical, intent(inout) :: first_crash
        integer, intent(inout) :: first_crash_pos(2) 

        integer :: i, collide_with, ncrashed

        ncrashed = 0
        call sort_carts(this%carts(1:this%ncarts))

        ! move carts, disable crashed ones
        do i=1,this%ncarts
            if (this%carts(i)%crashed) cycle
            call this%carts(i)%move(this%board)
            collide_with=this%carts(i)%collide(this%carts(1:this%ncarts))
            if (collide_with /= 0) then
                associate (c=>this%carts(i), d=>this%carts(collide_with))
                    print '("tick = ",i5,"  Collision ",i2," with ",i2," at ",i0,",",i0)', &
                    &  this%tick, c%id, d%id, c%pos(1)-1, c%pos(2)-1
                    if (.not. first_crash) then
                        first_crash = .true.
                        first_crash_pos = c%pos-1
                    end if
                    c%crashed = .true.
                    d%crashed = .true.
                end associate
                ncrashed = ncrashed + 2
            end if           
        end do

        ! move crashed carts out of sight (the end of array)
        do i=1,this%ncarts
            associate(c=>this%carts(i))
                if (c%crashed) then
                    c%pos(2) = c%pos(2) + size(this%board,2)
                    c%pos(1) = c%pos(1) + c%id
                end if
            end associate
        end do
        call sort_carts(this%carts(1:this%ncarts))
        this%ncarts = this%ncarts - ncrashed
        this%tick = this%tick + 1
    end subroutine mine_movecarts


    subroutine sort_carts(arr)
        type(cart_t), intent(inout) :: arr(:)
        integer :: i
        logical :: was_swap
        
        ! Bubble-sort (defined operator >)
        do 
            was_swap = .false.
            do i=1,size(arr)-1
                if (arr(i)>arr(i+1)) then
                    call swap(arr(i),arr(i+1))
                    was_swap = .true.
                end if
            end do
            if (.not. was_swap) exit
        end do
    contains
        pure subroutine swap(a,b)
            type(cart_t), intent(inout) :: a, b
            type(cart_t) :: tmp
            tmp = a
            a = b
            b = tmp
        end subroutine
    end subroutine


    pure integer function cart_collide(this, others) result(collision_with)
        class(cart_t), intent(in) :: this
        class(cart_t), intent(in) :: others(:)
        integer :: i

        collision_with = 0
        do i=1,size(others)
            ! ignore same cart, ignore disabled carts
            if (this%id==others(i)%id) cycle
            if (others(i)%crashed) cycle
            if (any(this%pos/=others(i)%pos)) cycle
            ! collision detected
            collision_with = i
            exit
        end do
    end function cart_collide


    subroutine cart_move(this,board)
        class(cart_t), intent(inout) :: this
        character(len=1), intent(in) :: board(:,:)

        ! move in current direction, check destination
        this % pos = this % pos + DIR(:,this%ori)
        if (this%pos(1)<1 .or. this%pos(1)>ubound(board,1) .or. & 
        &   this%pos(2)<1 .or. this%pos(2)>ubound(board,2)) error stop 'move - out of board'
        select case(board(this%pos(1),this%pos(2)))
        case(CHHOR) ! "-"
            if (this%ori==ORI_UP .or. this%ori==ORI_DOWN) error stop 'move - horizontal track not expected' 
        case(CHVER) ! "|"
            if (this%ori==ORI_RIGHT .or. this%ori==ORI_LEFT) error stop 'move - vertical track not expected'
        case(CHNW)  ! "/"
            if (this%ori==ORI_UP .or. this%ori==ORI_DOWN) then
                call this%turn(RIGHT_TURN)
            else if (this%ori==ORI_LEFT .or. this%ori==ORI_RIGHT) then
                call this%turn(LEFT_TURN)
            end if
        case(CHNE)  ! "\"
            if (this%ori==ORI_UP .or. this%ori==ORI_DOWN) then
                call this%turn(LEFT_TURN)
            else if (this%ori==ORI_LEFT .or. this%ori==ORI_RIGHT) then
                call this%turn(RIGHT_TURN)
            end if
        case(CHCR)  ! "+"
            select case (this%crossing)
            case(1)
                call this%turn(LEFT_TURN)
                this%crossing = 2
            case(2)
                this%crossing = 3
            case(3)
                call this%turn(RIGHT_TURN)
                this%crossing = 1
            case default
                error stop 'move - invalid crossing mode'
            end select
        case default
            error stop 'move - uknown character'
        end select
    end subroutine cart_move


    subroutine cart_turn(this, turndir)
        class(cart_t), intent(inout) :: this
        integer, intent(in) :: turndir
        this%ori = this%ori + turndir
        if (this%ori>4) this%ori=1
        if (this%ori<1) this%ori=4
    end subroutine cart_turn


    elemental logical function cart_greater_than(a, b) result(gt)
        class(cart_t), intent(in) :: a, b
        ! greater moves later (carts at a lower row move later)
        if (a%pos(2)>b%pos(2)) then
            gt = .true.
        else if (a%pos(2)<b%pos(2)) then
            gt = .false.
        else
            ! greater moves later (carts more right move later)
            if (a%pos(1)>b%pos(1)) then
                gt = .true.
            else
                gt = .false.
            end if
        end if
    end function cart_greater_than
end module