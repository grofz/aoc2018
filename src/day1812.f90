module day1812_mod
    use parse_mod, only : read_strings, string_t, split
    implicit none
    private
    public :: LONG, predict_score

    character(len=1), parameter :: PLANT='#',  NOPLANT='.'
    integer, parameter :: LONG = selected_int_kind(18)

    type rule_t
        character(len=1) :: pattern(5)
        character(len=1) :: result
    end type rule_t

    type, public :: pots_t
        character(len=1), allocatable :: pots(:)
        type(rule_t), allocatable :: rules(:)
        integer :: lind, rind
        integer :: generation=0
    contains
        procedure :: grow 
        procedure :: score => pots_score
    end type

    interface pots_t
        module procedure pots_init
    end interface

contains
    function pots_init(file) result(new)
        type(pots_t) :: new
        character(len=*), intent(in) :: file
        type(string_t), allocatable :: lines(:), s(:)
        integer :: i, j
        integer, parameter :: OFFSET = 500

        lines = read_strings(file)
        call split(lines(1)%str,' ',s)
        if (s(1)%str /= 'initial' .or. s(2)%str/='state:' .or. size(s)/=3) &
            error stop 'pots_init: invalid first line'
        new%lind = 0
        new%rind = len_trim(s(3)%str)-1
        allocate(new%pots(new%lind-OFFSET:new%rind+OFFSET))
        new%pots = NOPLANT
        do i=1,len_trim(s(3)%str)
            new%pots(new%lind+i-1) = s(3)%str(i:i)
        end do

        ! read rules
        if (lines(2)%str /= '') error stop 'post_init: second line not empty'
        allocate(new%rules(size(lines)-2))
        do i=1,ubound(new%rules,dim=1)
            if (allocated(s)) deallocate(s)
            call split(lines(i+2)%str,' ',s)
            if (size(s)/=3 .or. s(2)%str/='=>') error stop 'post_init: rules line error'
            do j=1,5
              new%rules(i)%pattern(j)=s(1)%str(j:j)
            end do
            new%rules(i)%result=s(3)%str(1:1)
            !print *,(new%rules(i)%pattern(j),j=1,5),'->'//new%rules(i)%result
        end do
        !print '(*(a1))', (new%pots(j),j=new%lind-2,new%rind+2)
        !print *, new%lind, new%rind
    end function


    function pots_score(this) result(score)
        class(pots_t), intent(in) :: this
        integer :: score
        integer :: i

        score=0
        do i=this%lind, this%rind
            if (this%pots(i)==PLANT) score=score+i
        end do
    end function pots_score


    function predict_score(steady, ngen) result(score)
        integer(LONG) :: score
        integer(LONG), intent(in) :: ngen
        type(pots_t), intent(in) :: steady

        integer(LONG) :: del
        integer :: i

        del = ngen - int(steady%generation,LONG)
        score = 0
        do i=steady%lind, steady%rind
            if (steady%pots(i)==PLANT) score=score+int(i,LONG)+del
        end do
    end function predict_score


    subroutine grow(this)
        class(pots_t), intent(inout) :: this
        character(len=1), allocatable :: copy(:)

        integer :: il, ir, i, pattern_id
        logical :: isfirst

        il = this%lind-4
        ir = this%rind+4
        isfirst = .true.
        allocate(copy(il:ir))
        copy = this%pots(il:ir)
        do i=this%lind-2, this%rind+2
            pattern_id = ismatch(copy(i-2:i+2),this%rules)
            if (pattern_id==0) then
                this%pots(i) = NOPLANT
            else if (this%rules(pattern_id)%result==PLANT) then
                this%pots(i) = PLANT
                if (isfirst) then
                    this%lind = i
                    isfirst = .false.
                end if
                if (i>this%rind) this%rind=i
            else if (this%rules(pattern_id)%result==NOPLANT) then
                this%pots(i) = NOPLANT
            end if
        end do
        this%generation = this%generation+1
    end subroutine


    function ismatch(arr,patterns) result(pattern_id)
        integer :: pattern_id
        character(len=1) :: arr(5)
!
! return index of the matching pattern or zero if no match
!
        type(rule_t), intent(in) :: patterns(:)
        integer :: i
        pattern_id = 0
        do i=1,size(patterns)
            if (all(arr==patterns(i)%pattern)) then
                pattern_id = i
                exit
            end if
        end do
    end function
end module
