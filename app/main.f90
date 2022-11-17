  program main
    implicit none
    goto 240

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

 170 call day17('inp/1817/input.txt') ! test.txt

 180 call day18('inp/1818/input.txt') ! test.txt

 190 call day19('inp/1819/input.txt')

 200 call day20('inp/1820/input.txt') ! test cases available

 210 call day21('inp/1821/input.txt')

 220 call day22()

 230 call day23('inp/1823/input.txt') ! test.txt, test2.txt

 240 call day24('inp/1824/input.txt') ! test.txt
!240 call day24('inp/1824/test.txt') ! test.txt

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



  subroutine day16(file)
    use day1816_mod
    implicit none
    character(len=*) :: file
    type(test_t), allocatable :: tests(:)
    logical :: map(0:NOPS-1,0:NOPS-1)    
    integer, allocatable :: decode(:)
    type(computer_t) :: zx, zx2
    integer :: inst(4), after(4), i, ans1, j, k
    integer, allocatable :: code(:,:)

    call read_tests(file, tests, code)
    call run_tests(tests, map, ans1)
    print '("Answer 16/1 ",i0,l2)', ans1, ans1==612
    
    ! Part two
    decode = decode_opcodes(map)       
    zx = computer_t(4)
    !zx%isdebug = .true.
    zx%dict = decode
    zx%r=0
    do i=1,size(code,2)
      call zx%exec(code(:,i))
    end do    
    print *, zx%r(0)

  end subroutine day16



  subroutine day17(file)
    use day1817_mod
    implicit none
    character(len=*) :: file
    type(instruction_t), allocatable :: instr(:)
    type(map_t) :: map
    integer :: cnt_fl, cnt_st
    call read_from_file(file, instr)
    map = map_t(instr)
    !call map%display()
    print '("Map range ",i0,":",i0," , ",i0,":",i0)', map%mnpos(1), map%mxpos(1), map%mnpos(2), map%mxpos(2)
    print '("Map size  ",i0," x ",i0)', size(map%b,1), size(map%b,2)

    call map%fill()
    !call map%display()
    cnt_fl = count(map%b(:,map%mnpos(2):map%mxpos(2))==CH_FLOW)
    cnt_st = count(map%b(:,map%mnpos(2):map%mxpos(2))==CH_FREZ)
    print *, 'Ans 1 ',cnt_fl+cnt_st,  (cnt_fl+cnt_st)==29802
    print *, 'Ans 2 ',cnt_st,  (cnt_st)==24660

  end subroutine day17



  subroutine day18(file)
    use day1818_mod, only : board_t
    implicit none
    character(len=*), intent(in) :: file
    integer :: ans1, ans2, mx, lastmx, period
    integer, parameter :: REQ1=10, REQ2=1000000000
    type(board_t) :: board

    ! Part One
    board = board_t(file)
    do
      call board%evolve()
      !call board%display()
      if (board%time>=REQ1) exit
    end do
    ans1 = board%resval()
    print '("Answer 18/1 ",i0,l2)', ans1, ans1==505895

    ! Part Two
    ! Evolve until 500 to stabilize
    ! Then determine the length of the oscillation
    do
      call board%evolve()
      if (board%time>=500) exit
    end do
    mx = board%resval()
    lastmx = board%time
    period = -1
    do 
      call board%evolve()
      associate(resval=>board%resval())
        if (resval>mx) then
          mx=resval
        elseif (resval==mx) then
          print *, 'MX',mx, board%time, board%time-lastmx
          if (period == board%time-lastmx) then
            print *, 'oscillation period calibrated'
            exit
          end if
          period = board%time-lastmx
          lastmx = board%time
        end if
      end associate
      if (board%time>=1000) error stop 'day 18 - could not determine periodicity'
    end do

    ! Jump in time and evolve until requirement
    associate(t=>board%time)
      t = t + ((REQ2-t)/period)*period
    end associate
    do
      call board%evolve()
      if (board%time>=REQ2) exit
    end do
    call board%display()
    ans2 = board%resval()
    print '("Answer 18/2 ",i0,l2)', ans2, ans2==139590
  end subroutine day18



  subroutine day19(file)
    use day1819_mod, only : compver2_t, faster
    use iso_fortran_env, only : IK=>int64
    implicit none
    character(len=*), intent(in) :: file
    type(compver2_t) :: zx128
    integer(IK) :: i, ans1, ans2, r3val
    integer, parameter :: MAXSTEPS=10000000

    zx128 = compver2_t(file)
    !call zx128%list()

    r3val = -1
    !zx128%isdebug=.true.
    do i=1,MAXSTEPS
      call zx128%onestep()
      if (zx128%halted) exit
      if (zx128%ip==1 .and. r3val<0) r3val = zx128%r(3)
    end do
    if (i==MAXSTEPS+1) error stop 'day19 - maximum steps reached'
    print '("Instructions executed =",i0)', zx128%counter
    ans1 = zx128%r(0)
    print '("Answer 19/1 ",i0,l2,l2)', ans1, ans1==faster(int(r3val)), r3val==888

    ! Part Two
    ! Let us see, what number is in register 3, when main loops start
    r3val = -1
    call zx128%reset()
    zx128%r(0)=1 ! Run another program
    !zx128%isdebug=.true.
    do i=1,100_IK
      call zx128%onestep()
      if (zx128%halted) exit
      if (zx128%ip==1) then
        r3val = zx128%r(3)
        exit ! break point at instruction 1
      end if
    end do
    if (r3val<0) error stop 'day19 - could not obtain r3val'
    ans2 = faster(int(r3val))
    print '("Answer 19/2 ",i0,l2)', ans2, ans2==30481920
  end subroutine day19



  subroutine day20(file)
    use day1820_mod
    use day1815_mod, only : manhattan, accessible_i, array_display
    use parse_mod, only : string_t, read_strings
    implicit none
    character(len=*), intent(in) :: file
    type(string_t), allocatable :: lines(:), input(:)
    integer :: iend, pos(2), ans1, ans2
    type(maze_t) :: maze
    integer, allocatable :: manh(:,:)
    
    lines = read_strings(file)
    ! remove ^ and $ characters around the path
    call parse(lines(1)%str, input, iend)
    if (size(input)/=1) error stop 'day20 - something left out in the input'
    
    ! first pass to get maze size
    ! second pass to get maze layout
    pos = 0
    call crawler(input(1)%str, pos, maze)
    call maze%alloc()
    call crawler(input(1)%str, pos, maze)
    call maze%remove_unknowns()
    call maze%display()
    print '("Maze size ",i0," x ",i0)', size(maze%b,1), size(maze%b,2)
    
    ! manhattan distance map from the original position
    pos = pos - lbound(maze%b) + 1    
    manh = manhattan(pos, maze%b, maze_accessible)
    !call array_display(manh)
    
    ! part one
    ans1 = maxval(manh)/2
    print '("Answer 20/1 :",i0,l2)', ans1, ans1==3574    

    ! part two
    ans2 = count(manh>=2000 .and. maze%b==CH_ROOM)
    print '("Answer 20/1 :",i0,l2)', ans2, ans2==8444
  end subroutine day20



  subroutine day21(file)
    use day1819_mod, only : compver2_t
    use iso_fortran_env, only : IK=>int64
    implicit none
    character(len=*), intent(in) :: file
    type(compver2_t) :: zx128
    integer(IK) :: i, ans1, ans2, r3val
    integer, parameter :: MAXSTEPS=2500, MAXFOUND=15000
    integer :: found(MAXFOUND), nfound, j

    zx128 = compver2_t(file)

    ! First run
    call zx128%list()
    r3val = -1
    !zx128%isdebug = .true.
    do i=1,MAXSTEPS
      call zx128%onestep()
      if (zx128%ip==28) exit
    end do
    print '("Instructions executed = ",i0)', zx128%counter
    r3val = zx128%r(3)

    ! Second run
    call zx128%reset()
    zx128%r(0)=r3val         ! make computer halt?
    do i=1,MAXSTEPS
      call zx128%onestep()
      if (zx128%halted) exit
    end do
    print '("When halted. Instructions executed = ",i0)', zx128%counter
    ans1 = r3val
    print '("Answer 21/1 ",i0,l2)', ans1, ans1==3173684

    ! Third run
    nfound = 0
    ans2 = -1
    call zx128%reset()
    LOOP3: do i=1,MAXSTEPS*1000000
      call zx128%onestep()
      ! break-point at instruction @28 (comparison r0 and r3)
      if (zx128%ip==28) then
        associate(r3=>zx128%r(3))
          do j=1,nfound
            if (found(j)==r3) then
              ans2 = found(nfound)
              exit LOOP3
            end if
          end do
          nfound = nfound + 1
          if (nfound>MAXFOUND) error stop 'day 21 - increase buffer for found values'
          found(nfound) = r3
        end associate
      end if
      if (zx128%halted) error stop 'day 21 - unexpected halt'
    end do LOOP3
    print '("Instructions executed = ",i0)', zx128%counter
    print '("Answer 21/2 ",i0,l2)', ans2, ans2==12464363
  end subroutine day21



  subroutine day22()
    use day1822_mod
    implicit none

    type(cave_t) :: cave
    integer :: nx, ny, ans1, ans2

    call determine_gi(cave%gi)
    nx = size(cave%gi,1) 
    ny = size(cave%gi,2)
    print *, 'size cave = ', nx, ny
    allocate(cave%type(0:nx-1,0:ny-1))
    cave%type = determine_type(determine_el(cave%gi))
    !call cave%display()
    ans1 = cave%risklevel()
    print '("Answer 22/1 ",i0,l2)', ans1, ans1==6256

    ! Part Two
    allocate(cave%accessible(3,0:nx-1,0:ny-1))
    associate(a=>cave%accessible)
      a = .false.
      where (cave%type==CH_ROCK)
        a(ID_CLIMB,:,:) = .true.
        a(ID_TORCH,:,:) = .true.
      end where
      where (cave%type==CH_WET)
        a(ID_CLIMB,:,:) = .true.
        a(ID_NONE,:,:) = .true.
      end where
      where (cave%type==CH_NARR)
        a(ID_TORCH,:,:) = .true.
        a(ID_NONE,:,:) = .true.
      end where
    end associate

    call djikstra(cave, ans2)
    print '("Answer 22/2 ",i0,l2)', ans2, ans2==973
  end subroutine day22



  subroutine day23(file)
    use day1823_mod
    use parse_mod, only : string_t, read_strings
    implicit none
    character(len=*), intent(in) :: file
    type(nanobot_t), allocatable :: bots(:)
    type(string_t), allocatable :: lines(:)
    integer, parameter :: MAXJUMP=10000
    integer :: i, j, k, indmax, maxr, ans1, counter, jump, bir
    integer(i8b) :: dir(3), next(3), next2(3), cur(3), ans2

    ! import nanobots from the file, get the one with highest range
    indmax = -1
    maxr = -1
    lines = read_strings(file)
    allocate(bots(size(lines)))
    print '("Nanobots present ",i0)', size(bots)
    do i=1,size(bots)
      bots(i) = nanobot_t(lines(i)%str)
      !print *, bots(i)%pos, bots(i)%rad
      if (bots(i)%rad > maxr) then
        maxr = bots(i)%rad
        indmax = i
      end if
    end do
    print *, '("Nanobot wirth highest radius: ",4(i0,1x))', &
    & bots(indmax)%pos, bots(indmax)%rad

    ! Part One - count how many other bots are in range of the one
    ! with the highest range
    ans1 = 0
    do i=1,size(bots)
      if (bots(indmax)%isinrange(bots(i)%pos)) ans1 = ans1 + 1
    end do
    print '("Answer 23/1 ",i0,l2)', ans1, ans1==491

    ! Part Two
    !cur = [0,0,0]
    cur = [0,1,2]
    counter = 0
    jump=1000
    MAIN: do
      dir = best_dir(cur, bots)
      ! Accept jump?
      ACCEPT: do
        next = cur + jump*dir
        next2 = cur+2*jump*dir

        if (is_better(next,cur,bots)) then
          ! accept
          if (is_better(next2,next,bots)) jump = min(jump*2, MAXJUMP)
          exit ACCEPT
        end if

        if (jump==1) exit ACCEPT
        print *, 'reducing step'
        jump = max(jump/2,1)
      end do ACCEPT
      cur = next
      counter = counter + 1
      print *, 'jump =',jump
      print '(3(i0,1x)," -> ",i0,1x,g0)', cur, bots_in_range(cur,bots), bots_field(cur,bots)
      if (sum(abs(dir))==0) exit
    end do MAIN
    bir = bots_in_range(cur,bots)
    print *, 'result = ',cur, bir

    ! Phase Two of the search
    goto 100
    !MAIN2: do
    !  dir = best_dir2(cur, bots)
    !  print '(3(i0,1x)," -> ",i0,1x,i8)', cur, bots_in_range(cur,bots), manhattan(cur+dir,int([0,0,0],I8B))
    !  cur=cur+dir
    !  if (sum(abs(dir))==0) exit MAIN2
    !end do MAIN2

    100 ans2 = manhattan(cur,int([0,0,0],I8B))
    print '("Answer 23/2 ",i0,l2)', ans2, ans2==60474080

  end subroutine day23



  subroutine day24(file)
    use day1824_mod, only : unit_t, fight_battle, read_units
    implicit none
    character(len=*), intent(in) :: file
    type(unit_t), allocatable :: units(:)
    integer :: ans1, ans2, b(2), res(2), bmid, resmid

    units = read_units(file)
    call fight_battle(units, 0, ans1)
    print '("Answer 24/1 ",i0,l2)', -ans1, -ans1==18532

    ! Part Two
    b(1) = 0
    res(1) = ans1
    units = read_units(file)
    b(2) = 100000
    call fight_battle(units, b(2), res(2))
    do
      print '("Boost ",i0," (",i0,") --- ",i0," (",i0,")")', &
      & b(1),res(1),b(2),res(2)
      if (res(1)*res(2)>0) error stop 'day 24 - solution not in range'
      bmid = b(1)+(b(2)-b(1))/2
      if (bmid==b(1)) exit
      units = read_units(file)
      call fight_battle(units, bmid, resmid)
      if (resmid > 0) then
        b(2) = bmid
        res(2) = resmid
      else
        b(1) = bmid
        res(1) = resmid
      endif
    end do

    ans2 = res(2)
    print '("Answer 24/1 ",i0,l2)', ans2, ans2==6523
  end subroutine