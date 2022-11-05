module day1807_mod
    use parse_mod, only : read_strings, string_t, split
    implicit none
    private
    public solve_part1, solve_part2

    integer, parameter :: MAX_INDEX = iachar('Z')-iachar('A')+1
    integer, parameter :: NO_TASK_AVAILABLE=0   
    integer, parameter :: WORKER_WORKING=-1, WORKER_FREE=-2

    integer, protected :: base_time = 0  ! 60 in real case


    type, public :: graph_t
        private
        integer :: dep(MAX_INDEX,MAX_INDEX)=0 ! 1=task, 2=prerequisity 
        logical, public :: finished(MAX_INDEX) = .true.
        integer :: assigned(MAX_INDEX) = 0       
    contains
        private
        procedure :: has_prereq_done
        procedure :: get_free_task, assign_task, complete_task
    end type
    interface graph_t
        module procedure read_graph
    end interface


    type :: worker_t
        integer :: assigned_task = 0
        integer :: time_end = -1
    contains
        procedure :: get_status => worker_get_status
        procedure :: assign_task => worker_assign_task
    end type

contains

    function worker_get_status(this, time) result(status)
        class(worker_t), intent(in) :: this
        integer, intent(in) :: time
        integer :: status

        if (time < this%time_end) then
            status = WORKER_WORKING
        else if (time == this%time_end) then
            ! reporting the task is done
            status = this%assigned_task 
        else
            status = WORKER_FREE
        end if
    end function worker_get_status



    subroutine worker_assign_task(this, time, itask)
        class(worker_t), intent(inout) :: this
        integer, intent(in) :: time, itask

        this%assigned_task = itask
        this%time_end = time + base_time + itask
    end subroutine worker_assign_task



    function get_free_task(this) result(index)
        class(graph_t), intent(in) :: this
        integer :: index

        integer :: i
        ! return index to the first available task to be assigned
        index = NO_TASK_AVAILABLE

        do i=1, MAX_INDEX
            ! ignore finished tasks
            if (this%finished(i)) cycle
            ! skip tasks not completed
            if (.not. this%has_prereq_done(i)) cycle
            ! skip already assigned tasks
            if (this%assigned(i)/=0) cycle
            ! this task is available
            index = i
            exit
        end do
    end function get_free_task



    subroutine assign_task(this, itask, iworker)
        class(graph_t), intent(inout) :: this
        integer, intent(in) :: itask, iworker
        if (.not. this%has_prereq_done(itask)) &
            error stop 'assign - prereq not done'
        if (this%assigned(itask)/=0) &
            error stop 'assign - task already assigned'
        if (this%finished(itask)) &
            error stop 'assign - already completed'
        
        this%assigned(itask) = iworker
    end subroutine assign_task



    subroutine complete_task(this, itask)
        class(graph_t), intent(inout) :: this
        integer, intent(in) :: itask
        if (.not. this%has_prereq_done(itask)) &
            error stop 'complete - prereq not done'
        if (this%assigned(itask)==0) &
            error stop 'complete - task not assigned'
        if (this%finished(itask)) &
            error stop 'complete - already completed'
        
        this%finished(itask) = .true.
        this%assigned(itask) = 0
    end subroutine complete_task



    function has_prereq_done(this, task) result(has)
        class(graph_t), intent(in) :: this
        integer, intent(in) :: task
        logical :: has
        integer :: i

        ! prerequisities are fullfilled until found otherwise
        has = .true.
        do i=1,MAX_INDEX
            if (this%dep(task,i)==1 .and. .not. this%finished(i)) then
                has = .false.
                exit
            end if
        end do
    end function has_prereq_done



    function index2key(index) result(key)
        integer, intent(in) :: index
        character(len=1) :: key
        key = achar(index+iachar('A')-1)
    end function



    function key2index(ch) result(index)
        character(len=*), intent(in) :: ch
        integer :: index
        index = iachar(ch(1:1))-iachar('A')+1
        if (index < 1 .or. index > MAX_INDEX) &
            error stop 'key2index - invalid index'
    end function key2index



    function read_graph(file) result(new)
        character(len=*), intent(in) :: file
        type(graph_t) :: new
        integer :: i
        type(string_t), allocatable :: lines(:), one(:)
        character(len=1) :: prereq, task

        lines = read_strings(file)
        do i=1,size(lines)
            call split(lines(i)%str,' ', one)
            if (size(one)/=10) error stop 'read_graph - wrong line'
            prereq = one(2)%str
            task = one(8)%str
            print *, prereq,'-->',task

            if (new%dep(key2index(task),key2index(prereq))/=0) error stop 'duplicit dependence'
            new%dep(key2index(task),key2index(prereq)) = 1
            new%finished(key2index(task)) = .false.
            new%finished(key2index(prereq)) = .false.
        end do

        !do i=1,size(new%dep,1)
        !    step = key_t(i)
        !    print '(a1,1x,l1,1x,*(i2))',step%ch,new%finished(i),new%dep(i,:)
        !end do 
    end function read_graph



    function solve_part1(main0) result(list_of_steps)
        type(graph_t), intent(in) :: main0
        character(len=:), allocatable :: list_of_steps

        type(graph_t) :: main
        integer :: inext, i
        character(len=1) :: next

        allocate(character(len=0)::list_of_steps)
        main = main0
        do
            if (all(main%finished)) exit
            inext = 0
            do i=1,MAX_INDEX
                if (main%finished(i)) cycle
                if (.not. main%has_prereq_done(i)) cycle
                inext = i
                exit
            end do
            if (inext == 0) error stop 'could not find next step'
            main%finished(inext) = .true.
            next = index2key(inext)
            list_of_steps = list_of_steps//next           
        end do
    end function solve_part1



    subroutine solve_part2(main0, nworkers, base_time0, time)
        type(graph_t), intent(in) :: main0
        integer, intent(in) :: nworkers, base_time0
        integer, intent(out) :: time

        type(graph_t) :: main
        type(worker_t) :: workers(nworkers)
        integer :: iworker, istatus, itask
        logical :: was_assigned

        main = main0
        base_time = base_time0
        time = -1

        TIME_LOOP: do
            time = time + 1

            ! retrieve completed tasks from workers
            do iworker=1,nworkers
                istatus = workers(iworker)%get_status(time)
                if (istatus > 0) then
                    call main%complete_task(istatus)
print '("time=",i0,"  worker=",i0,a)', time, iworker,' completed task '//index2key(istatus)
                end if
            end do

            ! assign tasks to workers
            do
                itask = main%get_free_task()
                if (itask == NO_TASK_AVAILABLE) exit

                was_assigned = .false.
                do iworker=1,nworkers
                    istatus = workers(iworker)%get_status(time)
                    if (istatus == WORKER_WORKING) cycle
                    ! worker is available
                    was_assigned = .true.
                    call workers(iworker)%assign_task(time,itask)
                    call main%assign_task(itask, iworker)
print '("time=",i0,"  worker=",i0,a)', time, iworker,' assigned task '//index2key(itask)
                    exit
                end do
                if (.not. was_assigned) exit
            end do

            ! are all tasks complete?
            if (all(main%finished)) exit TIME_LOOP            
        end do TIME_LOOP
print '("time=",i0,"  all tasks completed")', time
    end subroutine solve_part2

end module day1807_mod