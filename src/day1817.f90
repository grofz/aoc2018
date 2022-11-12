module day1817_mod
    use parse_mod, only : string_t, split, read_strings
    implicit none

    character, parameter :: CH_FREZ='~', CH_FLOW='|', CH_SAND='.', CH_CLAY='#'

    ! Spring object
    ! - has position, has time
    ! - if bellow sand: move down
    ! - if bellow running water, or reached minimum: delete the spring
    ! - if bellow clay/water: resolve left / right margins
    !   - closed/closed: freeze water layer, move up
    !   - open/closed: move over the ridge
    !   - open/open: move over the ridge, create a new spring at the other end
    !   - open/runnning water: ignore running water side
    !   - closed/running water: delete the spring
    !   - should not run into still water during left-right resolving
    ! - if below minimum y: delete the spring
    ! - if has been flooded: delete the spring  
    type spring_t
        integer :: xy(2)
        integer :: tm
        logical :: is_active = .false.
        integer :: id = -1
    end type

    ! Map
    ! - fill using instructions (x-layer, y-layer)
    ! - minimum / maximum clay
    ! - statistics (number of water/ running water)
    ! - visualize
    type map_t
        character(len=1), allocatable :: b(:,:)
        integer :: mxpos(2), mnpos(2)
        type(spring_t), allocatable :: springs(:)
        integer :: nsprings
    end type

    type instruction_t
        integer :: p1(2), p2(2)
    end type
    interface instruction_t
        module procedure instruction_new
    end interface

contains

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
            print '("P1 =",i5,1x,i5,"    P2 = ",i5,1x,i5)', &
            instr(i)%p1, instr(i)%p2
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


    subroutine sparr_sort(arr, n)
        type(spring_t), intent(inout) :: arr(:)
        integer, intent(in), optional :: n
        ! TODO - if needed
    end subroutine
end module
    