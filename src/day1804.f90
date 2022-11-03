  module day1804_mod
    use parse_mod, only : string_t
    implicit none
    private
    public shift_t, make_shifts, find_longest_sleeper
    public most_frequent_minute

    type shift_t
      character(len=5) :: date
      integer          :: id
      logical          :: asleep(0:59)=.false.
    contains
      procedure :: time_asleep => shift_time_asleep
    end type
    integer, parameter :: MSG_TYPE_ID = 0, MSG_TYPE_WAKE = 1, MSG_TYPE_SLEEP = 2

    type time_t
      integer :: month, day, hour, minute
    contains
      procedure :: setdate => time_setdate, nextday => time_nextday
      procedure :: to_ch => time_to_ch
    end type
    interface time_t
      module procedure time_new
    end interface

    type guard_t
      integer :: id = -1
      integer :: time_asleep = 0
      integer :: nshifts(0:59) = 0      ! number shifts at the selected minute
    end type 

  contains

    subroutine string_sort(arr)
      type(string_t), intent(inout) :: arr(:)
!
! insertion sort of string_t array
!
      integer :: n, i, j
      type(string_t) :: key

      n = size(arr,1)
      do i = 2, n
        if (allocated(key%str)) deallocate(key%str)
        key = string_t(arr(i)%str)
        j = i-1

        do
          if (j < 1) exit
          if (arr(j)%str <= key%str) exit
          if (allocated(arr(j+1)%str)) deallocate(arr(j+1)%str)
          arr(j+1) = string_t(arr(j)%str)
          j = j - 1
        end do
        if (allocated(arr(j+1)%str)) deallocate(arr(j+1)%str)
        arr(j+1) = key
      end do
    end subroutine string_sort



    subroutine make_shifts(records0, shifts)
      type(string_t), intent(in) :: records0(:)
      type(shift_t), intent(out), allocatable :: shifts(:)
!
! Make shift records from the input lines
!
      integer :: i, msg_type, msg_id, j
      integer :: current_id
      logical :: current_asleep = .false.
      integer :: current_asleep_time = -1
      type(string_t) :: message
      type(shift_t) :: newshift
      type(time_t) :: time
      type(string_t), allocatable :: records(:)

      records = records0
      call string_sort(records)

      allocate(shifts(0))
      current_id = -1
      j = 0
      do i=1, size(records)
        call decode(records(i)%str, time, msg_type, msg_id, message)
        select case(msg_type)
        case(MSG_TYPE_ID)
          if (current_asleep) error stop 'make_shifts - sleep not recorded'
          ! new guard - add new shift record
          current_id = msg_id
          current_asleep = .false.
          shifts = [shifts, newshift]
          j = j + 1
          shifts(j)%id = current_id
          shifts(j)%date = time%setdate()
        case(MSG_TYPE_SLEEP)
          if (time%to_ch() /= shifts(j)%date) error stop 'make_shifts - invalid shift sheet date'
          if (current_asleep) error stop 'make_shifts - already asleep'
          current_asleep = .true.
          current_asleep_time = time%minute
        case(MSG_TYPE_WAKE)
          if (time%to_ch() /= shifts(j)%date) error stop 'make_shifts - invalid shift sheet date'
          if (.not. current_asleep) error stop 'make_shifts - not asleep'
          shifts(j)%asleep(current_asleep_time:time%minute-1) = .true.
          current_asleep = .false.
        case default
          error stop 'make_shifts - invalid message type'
        end select
      end do
    end subroutine make_shifts



    subroutine find_longest_sleeper(shifts, sleeper_id, sleeper_shifts, minute, sleeper_id2)
      type(shift_t), intent(in) :: shifts(:)
      integer, intent(out) :: sleeper_id
      type(shift_t), intent(out), allocatable :: sleeper_shifts(:)
      integer, intent(out) :: minute, sleeper_id2

      type(guard_t), allocatable :: guards(:)
      type(guard_t) :: newguard
      integer :: i, index, tas_max, freq_max

      ! Populate guards table / Fill their sleep statistics
      allocate(guards(0))
      do i=1,size(shifts)
        index = find_guard(guards, shifts(i)%id)
        if (index==0) then
          ! add new guard to the table
          newguard%id = shifts(i)%id
          guards = [guards, newguard]
          index = size(guards)
        end if

        associate(tas=>guards(index)%time_asleep)
          tas = tas + shifts(i)%time_asleep()
        end associate
        associate(n=>guards(index)%nshifts)
          where (shifts(i)%asleep) n = n + 1
        end associate
      end do

      ! Select the longest sleeping guard
      sleeper_id = 0
      sleeper_id2 = 0
      tas_max = -1
      freq_max = -1 
      do i=1,size(guards)
        if (guards(i)%time_asleep > tas_max) then
          sleeper_id = guards(i)%id
          tas_max = guards(i)%time_asleep
        end if
        if (maxval(guards(i)%nshifts) > freq_max) then
          sleeper_id2 = guards(i)%id
          freq_max = maxval(guards(i)%nshifts)
          minute = findloc(guards(i)%nshifts,freq_max,dim=1)-1
        end if
      end do

      ! Extract shifts of the selected guard
      allocate(sleeper_shifts(0))
      do i=1,size(shifts)
        if (shifts(i)%id == sleeper_id) sleeper_shifts = [sleeper_shifts, shifts(i)]
      end do

      ! Print guards table
      do i=1,size(guards)
        print '("#",i4," total asleep = ",i0,/,3(20(i2,1x),/))', guards(i)%id, guards(i)%time_asleep, guards(i)%nshifts
        print *
      end do
    end subroutine find_longest_sleeper



    subroutine most_frequent_minute(shifts, minute)
      type(shift_t), intent(in) :: shifts(:)
      integer, intent(out) :: minute

      integer :: freq(0:59), i

      freq = 0
      do i=1,size(shifts)
        where (shifts(i)%asleep)
          freq = freq + 1
        end where
      end do
      minute = maxloc(freq,dim=1)-1
    end subroutine most_frequent_minute



    function find_guard(guards, guard_id) result(index)
      type(guard_t), intent(in) :: guards(:)
      integer, intent(in) :: guard_id
      integer :: index
      integer :: i

      index = 0
      do i=1,size(guards)
        if (guards(i)%id == guard_id) then
          index = i
          return
        end if
      end do
    end function



    subroutine decode(line, time, msg_type, msg_id, message)
      character(len=*), intent(in)  :: line
      type(time_t), intent(out)     :: time
      type(string_t), intent(out)   :: message
      integer, intent(out)          :: msg_type
      integer, intent(out)          :: msg_id

      integer :: ios

      time = time_t(line(1:18))
      message = string_t(line(20:))

      print *, time%month, time%day, time%hour, time%minute
      print *, message%str

      ! decode message
      msg_id = -1
      if (message%str(1:7)=='Guard #') then
        read(message%str(8:),*,iostat=ios) msg_id
        if (ios /= 0) error stop 'decode - guard id read fail'
        msg_type = MSG_TYPE_ID
      else if (message%str=='wakes up') then
        msg_type = MSG_TYPE_WAKE
      else if (message%str=='falls asleep') then
        msg_type = MSG_TYPE_SLEEP
      else
        error stop 'decode - invalid message'
      end if
    end subroutine



    ! =====================
    ! time_t implementation
    ! =====================

    function time_new(ch) result(new)
      character(len=*), intent(in) :: ch
      type(time_t) :: new
      integer :: ios(4)

      if (len(ch)/=18) error stop 'new_time - invalid ch'
      if (ch(1:1)/='[' .or. ch(len(ch):)/=']') error stop 'new_time - missing brackets'
      if (ch(2:5)/='1518') error stop 'new_time - wrong year'
      read(ch(7:8),*,iostat=ios(1)) new%month
      if (ch(9:9)/='-') error stop 'new_time - dash expected'
      read(ch(10:11),*,iostat=ios(2)) new%day
      if (ch(12:12)/=' ') error stop 'new_time - space expected'
      read(ch(13:14),*,iostat=ios(3)) new%hour
      if (ch(15:15)/=':') error stop 'new_time - colon expected'
      read(ch(16:17),*,iostat=ios(4)) new%minute
      if (any(ios/=0)) error stop 'new_time - reading error'
    end function time_new



    function time_setdate(this) result(ch)
      class(time_t), intent(in) :: this
      character(len=5) :: ch
      type(time_t) :: this0
!
! Return the current or the next day
!
      this0 = this
      select case(this0%hour)
      case(0)
        continue
      case(23)
        call this0%nextday()
      case default
        error stop 'time_setdate - hour 0 or 23 expected'
      end select
      ch = this0%to_ch()
    end function



    function time_to_ch(this) result(ch)
      class(time_t), intent(in) :: this
      character(len=5) :: ch
      character(len=2) :: dd, mm
      write(dd,'(i2.2)') this%day
      write(mm,'(i2.2)') this%month
      ch = mm//'-'//dd
    end function



    subroutine time_nextday(this)
      class(time_t), intent(inout) :: this
      integer :: last_day_of_month

      select case(this%month)
      case(1,3,5,7,8,10)
        last_day_of_month = 31
      case(4,6,9,11)
        last_day_of_month = 30
      case(2)
        last_day_of_month = 28
      case(12)
        error stop 'time_nextday: december not supported'
      case default
        error stop 'time_nextday: invalid month'
      end select

      this%day = this%day + 1
      if (this%day > last_day_of_month) then
        this%day = 1
        this%month = this%month + 1
      end if
    end subroutine time_nextday



    integer function shift_time_asleep(this) result(ast)
      class(shift_t), intent(in) :: this
      ast = count(this%asleep)
    end function

  end module day1804_mod
