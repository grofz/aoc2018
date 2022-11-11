  program main
    implicit none
    goto 160

 010 call day01('inp/1801/input.txt')

 020 call day02('inp/1802/input.txt')

 030 call day03('inp/1803/input.txt')

 040 call day04('inp/1804/input.txt')
     goto 050
 041 call day04('inp/1804/test.inp')

 050 call day05('inp/1805/input.txt')
     goto 060
 051 call day05('inp/1805/test.txt')

 060 call day06('inp/1806/input.txt')
     goto 070
 061 call day06('inp/1806/test.txt')

 070 call day07('inp/1807/input.txt')
     goto 080
 071 call day07('inp/1807/test.txt')

 080 call day08('inp/1808/input.txt')
     goto 090
 081 call day08('inp/1808/test.txt')

 090 call day09(441,71032)
     goto 100
 091 call day09(30,5807)

 100 call day10('inp/1810/input.txt')
     goto 110
 101 call day10('inp/1810/test.txt')

 110 call day11(7689)
     goto 120

 120 call day12('inp/1812/input.txt')
     goto 130
 121 call day12('inp/1812/test.txt')

 130 call day13('inp/1813/input.txt')
     goto 140
 131 call day13('inp/1813/test2.txt')

 140 call day14(540561)
     goto 150
 141 call day14(3000)

 150 call day15('inp/1815/input.txt')
     goto 160
 151 call day15('inp/1815/test5.txt') ! test0.txt - test5.txt

 160 call day16('inp/1816/input.txt')

 999 continue

  end program main



  subroutine day16(file)
    use day1816_mod
    implicit none
    character(len=*) :: file
    type(test_t), allocatable :: tests(:)
    logical :: map(0:NOPS-1,0:NOPS-1)
    integer :: decode(0:NOPS-1)
    type(computer_t) :: zx

    integer :: inst(4), after(4), i, ans1, j, k
    logical, allocatable :: results(:)
    integer, allocatable :: code(:,:)

    map = .true.
    decode = -1
    call read_tests(file, tests, code)
    ans1 = 0
    do i=1,size(tests)
      results = test_line(tests(i)%befo,tests(i)%inst,tests(i)%afte)
      where(.not. results) map(tests(i)%inst(1),:) = .false.      
      !print *, tests(i)%inst(1), count(results), results
      if (count(results)>=3) ans1 = ans1 + 1
    end do
    print *, ans1, ans1==612
    
    do k=1,8
    do i=0,NOPS-1
      print *, i, dd(map(i,:))
    end do
    print *
    call map_reduce(map,decode)
    if (count(decode<0)==0) exit
  end do
  print '(*(i2,1x))', decode
  zx%dict = decode
  do i=1,size(tests)
    zx%r = tests(i)%befo
    call zx%exec(tests(i)%inst)
    if(.not. all(zx%r==tests(i)%afte)) error stop 'test sample fail'
  end do

    ! run test code
    zx%r=0
    do i=1,size(code,2)
      call zx%exec(code(:,i))
      print *, code(:,i), zx%r
    end do

    contains
      elemental function dd(lg)
        logical, intent(in) :: lg
        character(len=2) :: dd
        dd = '  '
        if (lg) dd =' T'
      end function
    
  end subroutine



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
    implicit none
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



  subroutine day06(file)
    use day1806_mod
    implicit none
    character(len=*), intent(in) :: file
    type(point_t), allocatable :: points(:)
    type(area_t) :: board
    integer :: i, ans1, thr, ans2
    integer, parameter :: SET_THRESHOLD(2) = [32, 10000]


    points = read_points_from_file(file)
    call board%populate(points)

    if (size(points)<10) then
      do i=lbound(board%a,dim=1), ubound(board%a,dim=1)
        write(*,'("[",*(i0,1x))',advance='no') board%a(i,:)
        write(*,'(a)') ']'
      end do
    end if

    call board%area_count(points,ans1)
    print '("Answer 6/1: ",i0, l2)', ans1, ans1==4290

    thr = SET_THRESHOLD(2)
    if (size(points)<10) thr = SET_THRESHOLD(1)
    ans2 = count(board%b < thr)
    print '("Answer 6/2: ",i0, l2)', ans2, ans2==37318
  end subroutine



  subroutine day07(file)
    use day1807_mod, only : graph_t, solve_part1, solve_part2
    implicit none
    character(len=*), intent(in) :: file
    type(graph_t) :: main
    character(len=:), allocatable :: list_of_steps
    integer :: total_time
    integer, parameter :: BASE_TIME_TEST = 0, BASE_TIME_LIVE = 60
    integer, parameter :: NWORKERS_TEST = 2, NWORKERS_LIVE = 5
    
    main = graph_t(file)
    list_of_steps = solve_part1(main)
    print '("Answer 7/1 ",a," valid? ",l1)', &
        list_of_steps, list_of_steps=='HPDTNXYLOCGEQSIMABZKRUWVFJ'

    if (count(main%finished)>10) then
      call solve_part2(main, NWORKERS_TEST, BASE_TIME_TEST, total_time)
    else
      call solve_part2(main, NWORKERS_LIVE, BASE_TIME_LIVE, total_time)
    end if
    print '("Answer 7/2 ",i0," valid? ",l1)', total_time, total_time==908
  end subroutine day07



  subroutine day08(file)
    use parse_mod, only : string_t, read_strings, split
    use day1808_mod, only : sum_meta
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), splits(:)
    integer, allocatable :: arr(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day08: only one line expected'
    call split(lines(1)%str, ' ', splits)
    allocate(arr(size(splits)))
    do i=1,size(arr)
      arr(i) = splits(i)%to_int()
    end do
    !print *, arr

    i = 1
    call sum_meta(i, arr, ans1, ans2)
    print '("Answer 8/1 = ",i0,l2)', ans1, ans1==38722
    print '("Answer 8/2 = ",i0,l2)', ans2, ans2==13935
  end subroutine day08



  subroutine day09(nplayers, nmarbles)
    use day1809_mod
    use kinds_m, only : I8B
    implicit none
    integer, intent(in) :: nplayers, nmarbles
    integer(I8B) :: ans1, ans2
    call main_play(nplayers, nmarbles, ans1)
    print '("Answer 9/1: ",i0,l2)', ans1, ans1==393229

    call main_play(nplayers, nmarbles*100, ans2)
    print '("Answer 9/2: ",i0,l2)', ans2, ans2==3273405195_I8B
  end subroutine day09



  subroutine day10(file)
    use day1810_mod, only : stars_t
    implicit none
    character(len=*), intent(in) :: file
    type(stars_t) :: obj

    obj = stars_t(file)
    print *, 'limits$', obj%limits()
    call obj%align()
    print *, 'limits$', obj%limits()
    call obj%print()
    print '("Answer 10/2 ",i0,l2)', obj%gettime(), obj%gettime()==10355
    print *
  end subroutine day10



  subroutine day11(sernum)
    use day1811_mod
    implicit none
    integer, intent(in) :: sernum

    integer :: i, pos(3), mxpos(2), mx, mxsize
    type(grid_t) :: grid

    ! test cases
    print *, power_level(3,5,8)==4, power_level(122,79,57)==-5,&
    &   power_level(217,196,39)==0, power_level(101,153,71)==4

    grid = grid_t(sernum)
    pos = find_max_square(grid,3)
    print '("Answer 11/1 : ",i0,",",i0, l2)', pos(1:2), pos(1)==20 .and. pos(2)==37

    mxsize = -1
    mxpos = -1
    mx = -huge(mx)
    do i=1,30
      pos = find_max_square(grid,i)
      if (pos(3)>mx) then
        mx = pos(3)
        mxsize = i
        mxpos = pos(1:2)
      end if
    end do

    print '("Answer 11/2 : ",3(i0,","),l2)', mxpos,mxsize, &
      mxpos(1)==90 .and. mxpos(2)==169 .and. mxsize==15
  end subroutine



  subroutine day12(file)
    use day1812_mod
    implicit none
    character(len=*), intent(in) :: file
    type(pots_t) :: obj, steady
    integer :: i, j, nguessed_ok
    integer(LONG) :: ans2

    obj = pots_t(file)
    do 
      print '(i3,1x,*(a))',obj%generation,(obj%pots(j),j=obj%lind,obj%rind)
      if (obj%generation>=20) exit
      call obj%grow()
    end do
    print *
    print *, 'Answer 12/1 is', obj%score(), obj%score()==4818
    print *

    ! Grow until steady-state reached
    ! Prediction three times in a row
    steady = obj
    nguessed_ok = 0
    do 
      call obj%grow()
      print '(i3,1x,*(a))',obj%generation,(obj%pots(j),j=obj%lind,obj%rind)
      if (predict_score(steady,int(obj%generation,LONG))==int(obj%score(),LONG)) then
        nguessed_ok = nguessed_ok + 1
        !print '("gen=",i0," seems steady for ",i0," generations")', obj%generation, nguessed_ok
        if (nguessed_ok >= 3) exit
      else
        !if (nguessed_ok /= 0) print '("gen=",i0," changing again")',obj%generation
        nguessed_ok = 0
        steady = obj
      end if
      if (obj%generation>=200) exit ! safe-guard
    end do

    ! Answer part 2
    ans2 = predict_score(steady,50000000000_LONG)
    print *, 'Answer 12/2 is', ans2, ans2==5100000001377_LONG
    print *
  end subroutine day12



  subroutine day13(file)
    use day1813_mod
    implicit none
    character(len=*), intent(in) :: file
    type(mine_t) :: mine
    integer :: ans1(2), ans2(2)
    logical :: first_crash

    mine = mine_t(file)
    first_crash = .false.
    call mine % displayboard()
    do 
      call mine%movecarts(first_crash, ans1)
      if (mine%tick>20000) exit
      if (mine%ncarts<=1) exit
    end do
    if (mine%ncarts /=1) error stop 'day 13 - more than one cart remains on tracks'
    ans2 = mine%carts(1)%pos-1
    print '("Answer 13/1 ",i0,",",i0,l2)', ans1, ans1(1)==139 .and. ans1(2)==65
    print '("Answer 13/2 ",i0,",",i0,l2)', ans2, ans2(1)==40 .and. ans2(2)==77
  end subroutine day13


  subroutine day14(input)
    use day1814_mod, only : recipes_t
    use dll_mod, only : dll_export, node_t
    implicit none
    integer, intent(in) :: input
    type(recipes_t) :: obj
    integer,allocatable :: ans1(:)
    integer :: i

    obj = recipes_t(input)
    print '(a,*(i1,:,1x))', 'Input: ',obj%pattern

    ! Part one
    do 
      call obj%add_recipe()
      call obj%move_elves()
      !print '(*(i1,:,1x))', dll_export(obj%head)
      if (obj%n > input+10) exit
    end do
    ans1 = obj%score(input) 
    write(*,'(a,*(i1))',advance='no') 'Answer 14/1 ', ans1 
    write(*,'(l3)') all(ans1==[1,4,1,3,1,3,1,3,3,9])

    ! Part two - continue until pattern has been found
    do i=1,100000000
      call obj%add_recipe()
      call obj%move_elves()
      if (obj%pattern_match==size(obj%pattern)) exit
    end do
    print '(a,i0,l2)','Answer 14/2 ',obj%ans2, obj%ans2==20254833
  end subroutine day14



  subroutine day15(file)
    use day1815_mod
    implicit none
    character(len=*), intent(in) :: file
    type(board_t) :: obj
    integer, allocatable :: test(:,:)
    logical, allocatable :: ltest(:,:)
    integer :: dest(2), score, ans1, ans2, nelves0, ap

    ! Part One
    obj = board_t(file)
    call obj%display()
    do
      call obj%oneround()
      !call obj%display()
      if (obj%round>500) exit
      if (any(obj%nunits==0)) exit
    end do
    score = obj%final_score()
    ans1 = score*obj%round
    print *, 'score ',score,ans1,319410==ans1

    ! Part two 
    ap = 4
    do
      obj = board_t(file,ap)
      nelves0 = obj%nunits(ID_ELF)
      do
        call obj%oneround()
        if (obj%round>500) exit
        if (any(obj%nunits==0)) exit
      end do
      call obj%display()
      if (obj%nunits(ID_ELF)/=nelves0) then
        ap = ap + 1
      else 
        exit
      end if
    end do
    score = obj%final_score()
    ans2 = score*obj%round
    print *, 'score ',score, ans2, 63168==ans2

    print '("Answer 15/1: ", i0, l2)', ans1, ans1==319410
    print '("Answer 15/2: ", i0, l2)', ans2, ans2==63168
  end subroutine day15