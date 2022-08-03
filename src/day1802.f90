  module day1802_mod
    use parse_mod, only : string_t
    implicit none
    private
    public read_and_count, find_diff_one

    type freq_t
      character(len=1) :: ch
      integer          :: cnt=0
    end type freq_t

  contains

    subroutine read_and_count(file, cnt2, cnt3)
      character(len=*), intent(in) :: file
      integer, intent(out)         :: cnt2, cnt3

      integer :: fid, ios, n, i
      character(len=100) :: line
      type(freq_t), allocatable :: freq(:)
      logical :: is2, is3

      open(newunit=fid, file=file, status='old')
      cnt2 = 0
      cnt3 = 0
      do
        read(fid,'(a)',iostat=ios) line
        if (ios/=0) exit
        do i=1,len_trim(line)
          call add_freq(freq, n, line(i:i))
        end do
        is2 = .false.
        is3 = .false.
        do i=1,n
          if (freq(i) % cnt ==2) is2 = .true.
          if (freq(i) % cnt ==3) is3 = .true.
        end do
        deallocate(freq)
        if (is2) cnt2 = cnt2 + 1
        if (is3) cnt3 = cnt3 + 1
      end do
      close(fid)
    end subroutine



    subroutine add_freq(freq, n,  ch)
      type(freq_t), intent(inout), allocatable :: freq(:)
      integer, intent(inout)                   :: n
      character(len=1), intent(in)             :: ch

      type(freq_t), allocatable :: wrk(:)
      integer :: i

      if (.not. allocated(freq)) then
        allocate(freq(10))
        n = 0
      end if
      if (size(freq)==n) then
        allocate(wrk(2*size(freq)))
        wrk(1:n) = freq
        call move_alloc(wrk, freq)
      end if

      do i=1,n+1
        if (i==n+1) then
          ! "ch" not found, add new line
          n = n + 1
          freq(n) % ch  = ch
          freq(n) % cnt = 1
          exit
        end if

        if (freq(i) % ch == ch) then
          ! "ch" found, increase count
          freq(i) % cnt = freq(i) % cnt + 1
          exit
        end if
      end do
    end subroutine



    function find_diff_one(a, b) result(res)
      type(string_t), intent(in) :: a, b
      type(string_t)             :: res
      integer :: i, cnt_diff, loc_diff

      ! default result is empty string
      allocate(character(len=0) :: res % str)

      if (len_trim(a%str) /= len_trim(b%str)) return

      cnt_diff = 0
      do i=1,len_trim(a%str)
        if (a%str(i:i) == b%str(i:i)) cycle
        ! difference found
        select case(cnt_diff)
        case(0)
          cnt_diff = 1
          loc_diff = i
        case default
          ! more than one difference
          return
        end select
      end do

      ! only one difference was found
      if (cnt_diff==1) then
        deallocate(res%str)
        allocate(character(len=len_trim(a%str)-1) :: res % str)
        res % str(:loc_diff-1) = a%str(:loc_diff-1)
        res % str(loc_diff:) = a%str(loc_diff+1:len_trim(a%str))
      end if
    end function

  end module day1802_mod
