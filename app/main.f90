  program main
    implicit none
    goto 05

 01 call day01('inp/1801/input.txt')

 02 call day02('inp/1802/input.txt')

 03 call day03('inp/1803/input.txt')

 !04 call day04('inp/1804/test.inp')
 04 call day04('inp/1804/input.txt')

 05 call day05('inp/1805/input.txt')
 !05 call day05('inp/1805/test.txt')

  end program main



  subroutine day01(file)
    use day1801_mod
    use tree_m, only : rbtr_t
    implicit none
    integer :: ans1, ans2, cur
    type(rbtr_t) :: tree
    character(len=*), intent(in) :: file

    cur = 0
    tree = rbtr_t(compare)
    call read_and_count(file, cur, ans1, ans2, tree)
    print '("Answer 1/1 is ",i0,l2)', ans1, ans1==411

    do
      if (ans2 /= 0) exit
      call read_and_count(file, cur, ans1, ans2, tree)
    end do
    print '("Answer 1/2 is ",i0,l2)', ans2, ans2==56360

  end subroutine day01



  subroutine day02(file)
    use day1802_mod, only : read_and_count, find_diff_one
    use parse_mod, only : string_t, read_strings
    implicit none
    integer :: cnt2, cnt3, i, j
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: strings(:)
    type(string_t) :: wrk

    call read_and_count(file, cnt2, cnt3)
    print '("Answer 2/1 is ",i0,l2)', cnt2*cnt3, cnt2*cnt3==6474

    ! Part 2
    strings = read_strings(file)
    do i=1, size(strings)-1
    do j=i+1, size(strings)
      wrk = find_diff_one(strings(i),strings(j))
      if (len(wrk%str)==0) cycle
      print '("Answer 2/2 is ",a,l2)', wrk%str, wrk%str=='mxhwoglxgeauywfkztndcvjqr'
    end do
    end do
  end subroutine day02



  subroutine day03(file)
    use day1803_mod
    !use parse_mod, only : string_t, read_strings
    implicit none
    character(len=*), intent(in) :: file
    integer, allocatable :: claim(:,:), id(:,:)
    integer, parameter   :: MAXI = 1000
    integer :: i, j, ans2

    allocate(claim(MAXI,MAXI))
    allocate(id(MAXI,MAXI))
    claim = 0
    id = 0
    call make_claims(file, claim, id)
    print '("Answer 3/1 is ",i0,l2)', count(claim>1), count(claim>1)==107663

    MLOOP: do i=1,MAXI
    do j=1,MAXI
      if (id(i,j) == 0) cycle
      ans2 = id(i,j)
      exit MLOOP
    end do
    end do MLOOP
    print '("Answer 3/2 is ",i0,l2)', ans2, ans2==1166

  end subroutine day03



  subroutine day04(file)
    use day1804_mod, only : shift_t, make_shifts, find_longest_sleeper, most_frequent_minute
    use parse_mod, only : string_t, read_strings
    implicit none
    character(len=*), intent(in) :: file
    type(shift_t), allocatable :: shifts(:), sleeper_shifts(:)

    type(string_t) :: message
    integer :: i, sleeper_id, minute, minute_all, sleeper_id2
    character(len=5) :: date

    call make_shifts(read_strings(file), shifts)
    call find_longest_sleeper(shifts, sleeper_id, sleeper_shifts, minute_all, sleeper_id2)
    call most_frequent_minute(sleeper_shifts, minute)

    ! Answer Part 1
    print '("Sleeper #",i4," Most frequent minute = ",i0)', sleeper_id, minute
    print '("Answer 4/1 = ",i0,l2)', sleeper_id*minute, sleeper_id*minute==11367
    ! Answer Part 2
    print '("Sleeper #",i4," Most frequent minute = ",i0)', sleeper_id2, minute_all
    print '("Answer 4/2 = ",i0,l2)', sleeper_id2*minute_all, sleeper_id2*minute_all==36896
  end subroutine day04



  subroutine day05(file)
    use day1805_mod
    character(len=*), intent(in) :: file
    type(unit_t) u_read, u_top
    logical :: is_end
    type(stack_t) :: stk
    integer :: i, nmin

    do
      call read_unit(file, u_read, is_end)
      if (is_end) exit
      !print *, 'Read from file: ',u_read%m
      if (stk%is_empty()) then
        call stk%push(u_read)
      else
        u_top = stk%peek()
        if (u_top .reducible. u_read) then
          call stk%pop()          
        else
          call stk%push(u_read)          
        end if
      end if
    end do

    print '("Answer 5/1 ",i0,l2)',stk%size(), stk%size()==11668
    nmin = stk%size()
    call stack_final(stk)

    do i=iachar('A'), iachar('Z')
      do
        call read_unit(file, u_read, is_end)
        if (is_end) exit
        !print *, 'Read from file: ',u_read%m
        if (iachar(u_read%m)==i .or. iachar(u_read%m)==i+iachar('a')-iachar('A')) cycle
        if (stk%is_empty()) then
          call stk%push(u_read)
        else
          u_top = stk%peek()
          if (u_top .reducible. u_read) then
            call stk%pop()          
          else
            call stk%push(u_read)          
          end if
        end if
      end do
      print *, 'Done for '//achar(i)//' length ', stk%size()
      if (stk%size()<nmin) nmin = stk%size()
      call stack_final(stk)
    end do
    print '("Answer 5/2 ",i0,l2)', nmin, nmin==4652
  end subroutine 
