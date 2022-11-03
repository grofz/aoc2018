  module day1804_mod
    use parse_mod, only : string_t
    implicit none
    private
    public string_sort, decode

    type shift_t
      character(len=5) :: date
      integer          :: id
      logical          :: asleep(0:59)=.false.
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



    subroutine decode(line, date, minute, message)
      character(len=*), intent(in)  :: line
      character(len=5), intent(out) :: date
      integer, intent(out)          :: minute
      type(string_t), intent(out)   :: message

      integer :: hour
      date = line(7:11)
      read(line(13:14),*) hour
      read(line(16:17),*) minute
      message = string_t(line(20:))

      print *, date
      print *, minute
      print *, message%str
    end subroutine

  end module day1804_mod
