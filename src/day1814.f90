module day1814_mod
    use dll_mod, only : node_t, dll_newnode, dll_addnodebehind, dll_removeall
    implicit none
    private

    integer, parameter :: SCORE_DIGITS=10

    type, public :: recipes_t
        type(node_t), pointer :: head
        type(node_t), pointer :: tail
        type(node_t), pointer :: elf1, elf2
        integer :: n = -1
        integer, allocatable :: pattern(:)
        integer :: pattern_match = 0
        integer :: ans2 = 0
    contains
        procedure :: add_recipe => recipes_add_recipe
        procedure :: move_elves => recipes_move_elves
        procedure :: score => recipes_score
        procedure :: test_match => recipes_test_match
        final :: recipes_final
    end type

    interface recipes_t
        module procedure recipes_init
    end interface

contains

function recipes_init(input) result(new)
    type(recipes_t) :: new
    integer, intent(in) :: input

    integer, parameter :: FIRST(2) = [3, 7]
    type(node_t), pointer :: node1, node2
    character(len=9) :: inputch
    integer :: i

    node1 => dll_newnode(FIRST(1))
    node2 => dll_newnode(FIRST(2))
    call dll_addnodebehind(node1, node2)
    new%head => node1
    new%tail => node2
    new%elf1 => node1
    new%elf2 => node2
    new%n = 2

    ! store input as an array of one digit numbers
    write(inputch,'(i9)') input
    inputch = adjustl(inputch)
    allocate(new%pattern(len_trim(inputch)))
    do i=1,len_trim(inputch)
        read(inputch(i:i),*) new%pattern(i)
    end do

    ! formally test for the initial recipes
    call new%test_match(FIRST(1))
    call new%test_match(FIRST(2))
end function

! Add new recipes to the list
subroutine recipes_add_recipe(this)
    class(recipes_t), intent(inout) :: this
    integer :: ntot, n1, n2

    ntot = this%elf1%val + this%elf2%val
    n1 = ntot/10
    n2 = mod(ntot,10)
    if (n1==1) then
        call dll_addnodebehind(this%tail, dll_newnode(n1))
        this%tail => this%tail%next
        this%n = this%n + 1
        call this%test_match(n1)
    elseif (n1 /= 0) then
        error stop 'add_recipe - n1 not 0 or 1'
    end if
    call dll_addnodebehind(this%tail, dll_newnode(n2))
    this%tail => this%tail%next
    this%n = this%n + 1
    call this%test_match(n2)
end subroutine

! Move elves
function move_elf(elf, recipes) result (elfmoved)
    type(node_t), pointer, intent(in) :: elf
    type(recipes_t), intent(in) :: recipes
    type(node_t), pointer :: elfmoved

    integer :: i

    if (.not. associated(elf)) error stop 'move_elf - no current elf'
    elfmoved => elf
    do i = 1, 1+elf%val
        if (associated(elfmoved%next)) then
            elfmoved => elfmoved%next
        else
            elfmoved => recipes%head
        end if
    end do
end function move_elf


subroutine recipes_move_elves(this)
    class(recipes_t), intent(inout) :: this
    this%elf1 => move_elf(this%elf1, this)
    this%elf2 => move_elf(this%elf2, this)
end subroutine

! Back-track and sum the score
function recipes_score(this,n) result(arr)
    class(recipes_t), intent(in) :: this
    integer, intent(in) :: n
    integer :: arr(SCORE_DIGITS)

    type(node_t), pointer :: current
    integer :: back_steps, i

    back_steps = this%n - (n+SCORE_DIGITS)
    if (back_steps < 0) error stop 'score - not enough recipes cooked'
    current => this%tail
    do i=1,back_steps
        current => current%prev
    end do
    do i=SCORE_DIGITS,1,-1
        if (.not. associated(current)) error stop 'score - invalid pointer'
        arr(i) = current%val
        if (i/=1) current => current%prev
    end do
end function recipes_score


subroutine recipes_test_match(this, val)
    class(recipes_t), intent(inout) :: this
    integer, intent(in) :: val
    logical, save :: warning_printed = .false.

    ! if value matches the next digit in the pattern, add matched counter
    ! otherwise, reset the counter
    ! store the actual number of recipes at the moment pattern is found
    if (this%pattern_match >= size(this%pattern)) then
        if (.not. warning_printed) print *, 'test_match - pattern already found, skipping'
        warning_printed = .true.
        return
    end if
    if (val == this%pattern(this%pattern_match+1)) then
        this%pattern_match = this%pattern_match+1
    else 
        this%pattern_match = 0
    end if
    if (this%pattern_match==size(this%pattern)) then
        this%ans2 = this%n-size(this%pattern)
        print *, 'test_match - pattern found', this%ans2
    end if
end subroutine


subroutine recipes_final(this)
    type(recipes_t), intent(inout) :: this
    print '("Cleaning of ",i0," nodes")',this%n
    call dll_removeall(this%head)
    this%n = 0
    nullify(this%tail, this%elf1, this%elf2)
end subroutine

end module day1814_mod
