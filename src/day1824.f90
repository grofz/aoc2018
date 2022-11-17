module day1824_mod
    use parse_mod, only : string_t, read_strings
    implicit none

    integer, save :: idpool(2)=0
    integer, parameter :: SIDE_INF=2, SIDE_IS=1, MLEN=20
    type unit_t
        integer :: id
        integer :: side
        integer :: units
        integer :: hp
        integer :: adam
        character(len=MLEN) :: atype
        integer :: init
        character(len=MLEN), allocatable :: weakness(:)
        character(len=MLEN), allocatable :: imunity(:)
        integer :: attacking=-1, defending=-1
    contains
        procedure :: effpow
        procedure :: calculate_damage
        procedure :: display => unit_display
    end type unit_t
    interface unit_t
        module procedure unit_new
    end interface

    abstract interface
        pure logical function sortfun_i(a,b,c)
            import unit_t
            implicit none
            type(unit_t), intent(in) :: a,b
            type(unit_t), intent(in), optional :: c
        end function
    end interface
contains

    subroutine fight_battle(units, boost, result)
        type(unit_t), intent(inout) :: units(:)
        integer, intent(in) :: boost
        integer, intent(out) :: result

        integer, parameter :: MAXROUNDS = 100000
        integer :: iround, i, unit_remains(2), unit0(2)
        logical :: stalled

        ! Add boost 
        do i=1,size(units)
            if (units(i)%side==SIDE_IS) units(i)%adam=units(i)%adam+boost
        end do

        stalled = .false.
        unit_remains = count_units(units)
        do iround = 1, MAXROUNDS
            unit0 = unit_remains
            if (any(unit_remains==0)) exit

            call target_selection(units)
            call attack_phase(units)

            unit_remains = count_units(units)
            if (all(unit0==unit_remains)) then
                stalled = .true.
                print '("Battle stalls")'
                exit
            end if
        end do
        print '("With boost "i0," immune system ",i0," : infection ",i0)',&
        & boost, unit_remains

        if (iround==MAXROUNDS+1) then
            error stop 'increase maxround'
        end if
        if (stalled) then
            result = 0
        else
            result = unit_remains(SIDE_IS) - unit_remains(SIDE_INF)
        end if
    end subroutine


    subroutine target_selection(units)
        type(unit_t), intent(inout) :: units(:)

        integer, allocatable :: srt(:), opo(:)
        integer :: i, j, n

        n = size(units)
        do i=1,n
            units(i)%attacking = -1
            units(i)%defending = -1
        end do

        srt = sort(units, inorder1)
        !srt = sort(units, inorder1, units(1))
        do i=1,n
            associate(at=>units(srt(i)))
            ! skip dead units
            if (at%effpow()==0) cycle

            ! prepare sorted list of oponenents
            opo = sort(units, inorder2, at)
            OPONENTS_SEL: do j=1,n
                associate(df=>units(opo(j)))
                ! ignore friendly units
                if (df%side==at%side) cycle
                ! skip dead oponents
                if (df%effpow()==0) cycle
                ! skip already selected oponents
                if (df%defending /= -1) cycle
                ! skip oponents immune to attack
                if (at%calculate_damage(df)==0) cycle

                ! "at" chooses "df"
                at%attacking = opo(j)
                df%defending = srt(i)
                exit OPONENTS_SEL
                end associate
            end do OPONENTS_SEL
            end associate
        end do
    end subroutine


    subroutine attack_phase(units)
        type(unit_t), intent(inout) :: units(:)

        integer, allocatable :: srt(:)
        integer :: i, n, dam, nkill

        n = size(units)
        srt = sort(units, inorder3)
        !srt = sort(units, inorder3, units(1))
        do i=1,n
            associate(at=>units(srt(i)))
            ! Skip dead units
            if (at%effpow()==0) cycle
            ! Skip units that have no target
            if (at%attacking==-1) cycle
                associate(df=>units(at%attacking))
                ! Skip dead targets
                if (df%effpow()==0) cycle

                ! Calculate damage and remove hit-points
                dam = at%calculate_damage(df)
                nkill = dam/df%hp
                nkill = min(nkill, df%units)
                df%units = df%units - nkill
!print '("Unit ",i0," attacking ",i0," killing ",i0," units")',&
!& at%id, df%id, nkill

                end associate
            end associate
        end do
    end subroutine


    pure function calculate_damage(this, defender) result(dam)
        class(unit_t), intent(in) :: this, defender
        integer :: dam

        integer :: factor, i
        factor = 1
        associate(wk=>defender%weakness, im=>defender%imunity)
        do i = 1, size(wk)
            if (this%atype/=wk(i)) cycle
            factor = 2
            exit
        end do
        do i = 1, size(im)
            if (this%atype/=im(i)) cycle
            factor = 0
            exit
        end do
        end associate
        dam = this%effpow()*factor
    end function


    function count_units(units) result(n)
        type(unit_t), intent(in) :: units(:)
        integer :: n(2), i
        n = 0
        do i=1,size(units)
            associate(u=>units(i))
            if (u%units>0) n(u%side)=n(u%side)+u%units
            end associate
        end do
    end function


    elemental integer function effpow(this)
        class(unit_t), intent(in) :: this
        effpow = this%units * this%adam
    end function


    function unit_new(side, str) result(new)
        integer, intent(in) :: side
        character(len=*), intent(in) :: str
        type(unit_t) :: new

        integer, parameter :: MAXUNITS=100
        integer :: il, ir, i
        character(len=*), parameter :: CHHP='each with '
        character(len=*), parameter :: CHAT='an attack that does '
        character(len=*), parameter :: CHIN='at initiative '

        if (idpool(side) >= MAXUNITS-1) error stop 'new_unit: pool of id exceeded'
        idpool(side) = idpool(side)+1
        new%id = side*MAXUNITS + idpool(side)
        new%side = side
        read(str,*) new%units

        il = index(str, CHHP)
        if (il/=0) then
            read(str(il+len(CHHP):),*) new%hp
        else
            error stop 'new_unit - hp not identified'
        end if

        il = index(str, CHAT)
        if (il/=0) then
            read(str(il+len(CHAT):),*) new%adam, new%atype
        else
            error stop 'new_unit - attack not identified'
        end if

        il = index(str, CHIN)
        if (il/=0) then
            read(str(il+len(CHIN):),*) new%init
        else
            error stop 'new_unit - initiative not identified'
        end if

        il = scan(str,'(')
        ir = scan(str,')')
        if (il/=0 .and. ir/=0) then
            call get_attributes(new%weakness, new%imunity, str(il+1:ir-1))
        else
            allocate(new%weakness(0), new%imunity(0))
        end if

        ! debugging information
        return
        print *, new%side, new%id, new%units, new%hp 
        print *, new%adam, new%atype, new%init
        do i=1,size(new%imunity)
            print *,'immunity: ',new%imunity(i)
        end do
        do i=1,size(new%weakness)
            print *,'weakness: ',new%weakness(i)
        end do
        print *
    end function


    subroutine get_attributes(wk, im, str)
        character(len=MLEN), allocatable, intent(out) :: wk(:), im(:)
        character(len=*), intent(in) :: str

        character(len=*), parameter :: CHWK='weak to '
        character(len=*), parameter :: CHIM='immune to '
        integer :: poswk, posim

        allocate(wk(0), im(0))
        poswk = index(str,CHWK)
        posim = index(str,CHIM)
        if (poswk/=0) then
            poswk=poswk+len(CHWK)
            call get_subattribute(poswk, str, wk)
        end if
        if (posim/=0) then
            posim=posim+len(CHIM)
            call get_subattribute(posim, str, im)
        end if
    end subroutine get_attributes


    subroutine get_subattribute(pos,str,attr)
        integer, intent(in) :: pos
        character(len=*), intent(in) :: str
        character(len=MLEN), allocatable, intent(out) :: attr(:)

        character(len=MLEN), allocatable :: attr0(:)
        integer :: pos0, posend, n
        logical :: islast
        character(len=MLEN) :: item
        allocate(attr(0))
        pos0 = pos
        do
            if (pos0==0) exit
            islast = .true.
            posend = pos0-1 + scan(str(pos0:),',;)')
            if (posend-pos0+1 == 0) then
                posend=len(str)+1
            else
                if (str(posend:posend)==',') islast=.false.
            end if
            read(str(pos0:posend-1),*) item
            n = size(attr)
            allocate(attr0(n+1))
            attr0(1:n)=attr
            attr0(n+1) = item
            call move_alloc(attr0, attr)
            if (islast) exit
            pos0 = posend + 2
            if (pos0 > len(str)) exit
        end do
    end subroutine get_subattribute


    function read_units(file) result(units)
        character(len=*), intent(in) :: file
        type(unit_t), allocatable :: units(:)

        type(string_t), allocatable :: lines(:)
        integer :: i, n, side
        lines = read_strings(file)
        n = 0
        idpool = 0
        do i=1,size(lines)
            if (0==len_trim(lines(i)%str)) cycle
            if (lines(i)%str=='Immune System:') cycle
            if (lines(i)%str=='Infection:') cycle
            n = n + 1
        end do
        allocate(units(n))
        side = -1
        n = 0
        do i=1,size(lines)
            if (0==len_trim(lines(i)%str)) cycle
            if (lines(i)%str=='Immune System:') then
                side = SIDE_IS
            else if (lines(i)%str=='Infection:') then
                side = SIDE_INF
            else
                n = n + 1
                units(n)=unit_t(side,lines(i)%str)
            end if
        end do
    end function


    pure function sort(arr, inorder, item) result(sind)
        type(unit_t), intent(in) :: arr(:)
        type(unit_t), intent(in), optional :: item
        procedure(sortfun_i) :: inorder
        integer :: sind(size(arr))

        integer :: i, n, tmp
        logical :: swapped
        n = size(arr)
        sind = [(i,i=1,n)]

        ! Bubble sort
        do
            swapped = .false.
            do i=1, n-1
                if (inorder(arr(sind(i)),arr(sind(i+1)),item)) cycle
                tmp = sind(i)
                sind(i) = sind(i+1)
                sind(i+1) = tmp
                swapped = .true.
            end do
            if (.not. swapped) exit
        end do
    end function

    ! Who first chooses the oponent
    pure function inorder1(a,b,c) result(inorder)
        type(unit_t), intent(in) :: a,b
        type(unit_t), intent(in), optional :: c
        logical :: inorder
        if (a%effpow()>b%effpow()) then
            inorder = .true.
        else if (a%effpow()==b%effpow() .and. a%init>=b%init) then
            inorder = .true.
        else
            inorder = .false.
        end if
    end function

    ! Which oponent to attack first
    pure function inorder2(a,b,c) result(inorder)
        type(unit_t), intent(in) :: a,b
        type(unit_t), intent(in), optional :: c
        logical :: inorder
        if (.not. present(c)) error stop 'inorder2 - needs third argument'
        associate(adam=>c%calculate_damage(a), bdam=>c%calculate_damage(b))
        if (adam>bdam) then
            inorder = .true.
        else if (adam==bdam .and. a%effpow()>b%effpow()) then
            inorder = .true.
        else if (adam==bdam .and. a%effpow()==b%effpow() &
            & .and. a%init>=b%init) then
            inorder = .true.
        else
            inorder = .false.
        end if
        end associate
    end function

    ! Attacking order 
    pure function inorder3(a,b,c) result(inorder)
        type(unit_t), intent(in) :: a,b
        type(unit_t), intent(in), optional :: c
        logical :: inorder
        if (a%init > b%init) then
            inorder = .true.
        else
            inorder = .false.
        end if
    end function


    subroutine unit_display(this)
        class(unit_t), intent(in) :: this
        integer :: i
        print '(i0," units ",i0," att ",i0,1x,a," hp ",i0)', &
        & this%id, this%units, this%adam, trim(this%atype),this%hp
        do i=1,size(this%imunity)
            print *,'  immune ',this%imunity(i)
        end do
        print *, 'attacking/defending', this%attacking, this%defending
        print *
    end subroutine

end module day1824_mod