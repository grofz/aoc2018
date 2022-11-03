module day1805_mod
    implicit none
    private
    public read_unit, stack_final

    type, public :: unit_t
        character(len=1) :: m
    contains
        procedure, private :: is_reducible
        generic :: operator(.reducible.) => is_reducible
    end type

    type node_t
        type(unit_t) :: data
        type(node_t), pointer :: next => null()
    end type

    type, public :: stack_t
        integer :: n = 0
        type(node_t), pointer :: top => null()
    contains
        procedure :: push, pop, peek, is_empty, size=>stack_size
        final :: stack_final
    end type

contains

    logical function is_reducible(a, b)
        class(unit_t), intent(in) :: a, b
        integer :: i1, i2, del

        i1 = min(iachar(a%m), iachar(b%m))
        i2 = max(iachar(a%m), iachar(b%m))
        if (i1 < iachar('A') .or. i2 > iachar('z')) error stop 'is_reducible - invalid range'
        del = iachar('a') - iachar('A')
        if (i2-i1 == del) then
            is_reducible = .true.
        else 
            is_reducible = .false.
        end if
        !print *, a%m, b%m, is_reducible
    end function

    subroutine read_unit(file, unit, is_end)
        use iso_fortran_env, only : iostat_eor, iostat_end
        character(len=*), intent(in) :: file
        type(unit_t), intent(out)    :: unit
        logical, intent(out)         :: is_end

        logical :: is_open
        integer :: fid, ios

        inquire(file=file, opened=is_open, number=fid)
        if (.not. is_open) then
            open(newunit=fid, file=file, status='old')
        end if

        read(fid,'(a1)',advance='no',iostat=ios) unit%m
        if (ios==0) then
            ! read ok
            is_end = .false.
        else if (ios==iostat_end .or. ios==iostat_eor) then
            ! end of file/line
            is_end = .true.
            close(fid)
        else
            error stop 'read_unit - reading error'
        end if
    end subroutine


    function node_new(unit, next) result(new)
        type(unit_t), intent(in) :: unit
        type(node_t), pointer, intent(in) :: next
        type(node_t), pointer :: new
        allocate(new)
        new%data = unit
        new%next => next
    end function node_new

    subroutine push(this, unit)
        class(stack_t), intent(inout) :: this
        type(unit_t), intent(in) :: unit
        type(node_t), pointer :: newnode

        newnode => node_new(unit, this%top)
        this%top => newnode
        this%n = this%n + 1
    end subroutine push

    logical function is_empty(this)
        class(stack_t), intent(in) :: this
        if (.not. associated(this%top)) then
            is_empty = .true.
        else
            is_empty = .false.
        end if
    end function is_empty

    type(unit_t) function peek(this) result(unit)
        class(stack_t), intent(in) :: this
        if (this%is_empty()) error stop 'peek - stack is empty'
        unit = this%top%data
    end function

    subroutine pop(this, unit)
        class(stack_t), intent(inout) :: this
        type(unit_t), intent(out), optional :: unit
        type(node_t), pointer :: deleted
        if (this%is_empty()) error stop 'pop - stack is empty'
        if (present(unit)) unit = this%top%data
        deleted => this%top
        this%top => this%top%next
        deallocate(deleted)
        this%n = this%n - 1
    end subroutine

    integer function stack_size(this) result(n)
        class(stack_t), intent(in) :: this
        n = this%n
    end function

    subroutine stack_final(this)
        type(stack_t), intent(inout) :: this
        do
            if (this%is_empty()) exit
            call this%pop()
        end do
    end subroutine
        
end module