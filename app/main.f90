  program main
    implicit none

    call day01('inp/1801/input.txt')
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
