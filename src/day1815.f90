  ! board
  ! - display unit position and health
  ! - place/remove/move unit on the board

  ! position
  ! - sort array of positions
  ! - remove duplicities (?)

  ! unit
  ! - select target to attack
  ! - attack
  ! - attack phase

  module day1815_mod
    implicit none
    private
    public array_display, ID_ELF, manhattan, accessible_i

    character(len=1), parameter :: CH_WALL='#', CH_EMPT='.', CH_UNIT(2)=['E','G']
    integer, parameter :: ID_ELF = 1, ID_GOB = 2
    integer, parameter :: INIT_HP = 200, DEFAULT_ATTACK_POWER = 3

    type, public :: board_t
      character(len=1), allocatable :: b(:,:) ! row, column
      type(unit_t), allocatable :: units(:)
      integer :: nunits(2) = 0
      integer :: ap(2)
      integer :: round = 0
    contains
      procedure :: display => board_display
      procedure :: move => board_move
      procedure :: attack => board_attack
      procedure :: oneround => board_oneround
      procedure :: final_score => board_final_score
    end type

    interface board_t
      module procedure board_readfromfile
    end interface

    type, public :: unit_t
      integer :: id = -1
      integer :: pos(2)
      integer :: side
      integer :: hp = INIT_HP
    contains
      procedure :: dest_to_move => unit_dest_to_move
      procedure :: next_step => unit_next_step
      procedure :: isalive => unit_isalive
      procedure, private :: gt_unit
      procedure, private, pass(a) :: gt_unit_pos
      procedure, private, pass(b) :: gt_pos_unit
      generic :: operator(>) => gt_unit, gt_unit_pos, gt_pos_unit
    end type

    interface unit_t
      module procedure unit_init
    end interface

    interface array_display
      module procedure array_display_int, array_display_log
    end interface

    ! to identify what square is accessible
    abstract interface
        pure function accessible_i(ch) result(is_accessible)
          implicit none
          character(len=1), intent(in) :: ch
          logical :: is_accessible
        end function
    end interface

  contains

    function board_readfromfile(file,elf_ap) result(new)
      use parse_mod, only : string_t, read_strings
      character(len=*), intent(in) :: file
      integer, intent(in), optional :: elf_ap
      type(board_t) :: new
      integer :: i, j, rows, cols, nunits

      type(string_t), allocatable :: lines(:)

      lines = read_strings(file)
      rows = size(lines)
      cols = 0
      do i=1,rows
        if (len_trim(lines(i)%str) > cols) cols = len_trim(lines(i)%str)
      end do
      allocate(new%b(rows,cols))
      new%b = ' '
      allocate(new%units(0))
      nunits = 0
      do i=1,rows
        associate (lin=>lines(i)%str)
          do j=1,len_trim(lin)
            new%b(i,j)=lin(j:j)
            if (any(lin(j:j)==CH_UNIT)) then
              nunits = nunits+1
              new%units = [new%units, unit_t(nunits,i,j,lin(j:j))]
            end if
          end do
        end associate
      end do

      ! count number of elves / goblins
      new%nunits = 0
      do i=1,size(new%units)
        select case(new%units(i)%side)
        case(ID_ELF)
          new%nunits(ID_ELF) = new%nunits(ID_ELF) + 1
        case(ID_GOB)
          new%nunits(ID_GOB) = new%nunits(ID_GOB) + 1
        case default
          error stop 'unit_init - wrong side'
        end select
      end do
      print '("Board size ",i2,",",i2,"   Units E",i2,"   G",i2)', &
      & size(new%b,1), size(new%b,2), new%nunits(ID_ELF), new%nunits(ID_GOB)

      ! set attack power
      new%ap = DEFAULT_ATTACK_POWER
      if (present(elf_ap)) new%ap(ID_ELF) = elf_ap
    end function


    function unit_init(id,row,col,ch) result(new)
      integer, intent(in) :: id, row, col
      character(len=1), intent(in) :: ch
      type(unit_t) :: new
      new%id = id
      new%pos = [row, col]
      if (ch==CH_UNIT(ID_ELF)) then
        new%side = ID_ELF
      else if (ch==CH_UNIT(ID_GOB)) then
        new%side = ID_GOB
      else
        error stop 'unit_init - invalid unit type'
      end if
    end function


    logical function unit_isalive(this) result(isalive)
      class(unit_t), intent(in) :: this
      isalive = this%hp > 0
    end function


! ========================
! Round control and attack
! ========================


    subroutine board_oneround(this)
      class(board_t), intent(inout) :: this
      integer :: n, i, dest(2), next(2), attack_target
      logical :: round_finished
      
      round_finished = .true.
      n = sum(this%nunits)
      call unit_sort(this%units(1:n))
      do i=1, n
        if (.not. this%units(i)%isalive()) cycle

        ! mark unfinished if no more oponents remain
        if (any(this%nunits<=0)) round_finished = .false.

        ! moving phase
        dest = unit_dest_to_move(this%units(i), this%units(1:n), this%b)
        if (all(dest==0)) then
          print *, 'unit ',this%units(i)%id,' has no target avialable'
          cycle
        end if
        next = unit_next_step(this%units(i), dest, this%b)
        if (any(next/=0)) then
          call this%move(i, next)
        end if

        ! attack phase
        attack_target = unit_select_attack(this%units(i), this%units(1:n))
        if (attack_target /= 0) then
          call this%attack(i, attack_target)
        end if
      end do

      ! move removed units behind sight
      call unit_sort(this%units(1:n))

      ! update only if full round ended
      if (round_finished) this%round = this%round+1
    end subroutine board_oneround


    function board_final_score(this) result(score)
      class(board_t), intent(in) :: this
      integer :: score

      integer :: i
      if (all(this%nunits>0)) error stop 'score - battle not ended'
      score = 0
      do i=1,sum(this%nunits)
        score = score + this%units(i)%hp
      end do
    end function


    function unit_select_attack(this, others) result(target_index)
      class(unit_t), intent(in) :: this
      type(unit_t), intent(in) :: others(:)
      integer :: target_index

      integer :: i, best_hp, best_pos(2)

      target_index = 0
      best_hp = huge(best_hp)
      best_pos = huge(best_hp)

      do i=1,size(others)
        ! ignore itself, friendly units and dead units
        if (others(i)%side==this%side) cycle
        if (others(i)%id==this%id) cycle
        if (.not. others(i)%isalive()) cycle

        ! ignore distant enemies
        if (abs(others(i)%pos(1)-this%pos(1))+abs(others(i)%pos(2)-this%pos(2))/=1) cycle

        ! update target if lower on health or at lower position
        if (others(i)%hp < best_hp .or. &
           (others(i)%hp == best_hp .and. best_pos > others(i))) then
          target_index = i
          best_hp = others(i)%hp
          best_pos = others(i)%pos
        end if
      end do
    end function unit_select_attack



    subroutine board_attack(this,attind,defind)
      class(board_t), intent(inout) :: this
      integer, intent(in) :: attind, defind
      
      if (min(attind,defind) < 1 .or. max(attind,defind) > size(this%units)) &
        error stop 'attack - invalid indices'
      associate(attacker=>this%units(attind), defender=>this%units(defind))
        if (abs(attacker%pos(1)-defender%pos(1))+abs(attacker%pos(2)-defender%pos(2))/=1) &
          error stop 'attack - not within range'
        if (.not. (attacker%isalive() .and. defender%isalive())) &
          error stop 'attack - unit is not alive'

        ! Subtract HP
        !defender%hp = defender%hp - ATTACK_POWER
        defender%hp = defender%hp - this%ap(attacker%side)
        print '("Unit ",a1,i0," attacks ",a1,i0," leaving it with ",i0," HP")', &
        &  CH_UNIT(attacker%side), attacker%id, CH_UNIT(defender%side), defender%id, defender%hp

        ! Remove dead unit
        if (.not. defender%isalive()) then
          if (this%b(defender%pos(1),defender%pos(2))/=CH_UNIT(defender%side)) &
            error stop 'attack  - wrong character at the position'
          this%b(defender%pos(1),defender%pos(2)) = CH_EMPT
          ! position set outside, so it ends up at the end of sorting
          defender%pos(1)=size(this%b,1)+1
          defender%pos(2)=size(this%b,2)+1
          this%nunits(defender%side) = this%nunits(defender%side) - 1
        end if
      end associate
      if (any(this%nunits<=0)) then
        print *, 'Battle ended ', this%nunits
      end if
    end subroutine


! ============
! Moving units
! ============


    subroutine board_move(this, unit_index, next)
      class(board_t), intent(inout) :: this
      integer, intent(in) :: unit_index, next(2)

      if (next(1)<1 .or. next(2)<1 .or. next(1)>size(this%b,1) .or. next(2)>size(this%b,2)) &
        error stop 'move - invalid destination'
      associate(unit=>this%units(unit_index))
        if (.not. unit%isalive()) error stop 'move - moving dead unit'
        if (this%b(next(1),next(2)) /= CH_EMPT) &
          error stop 'move - destination not empty'
        if (abs(unit%pos(1)-next(1))+abs(unit%pos(2)-next(2))/=1) &
          error stop 'move - destination too far'
        if (this%b(unit%pos(1),unit%pos(2))/=CH_UNIT(unit%side)) &
          error stop 'move - wrong character at the origin'
        ! all assertions hold, let us move the unit
        this%b(unit%pos(1),unit%pos(2))=CH_EMPT
        unit%pos = next
        this%b(unit%pos(1),unit%pos(2))=CH_UNIT(unit%side)
        print '("Moved ",a1,i0," to ",i0,1x,i0)', &
        &  CH_UNIT(unit%side), unit%id, unit%pos
      end associate
    end subroutine board_move


    function unit_next_step(this, dest, board) result(next)
      class(unit_t), intent(in) :: this
      character(len=1), intent(in) :: board(:,:)
      integer, intent(in) :: dest(2)
      integer :: next(2)

      integer :: manh(size(board,1),size(board,2))
      logical :: map(size(board,1),size(board,2))

      ! do not move, if at the destination
      if (all(dest == this%pos)) then
        next = 0
        return
      end if
      manh = manhattan(dest, board, accessible_fun)
      map = free_ngb_squares(this%pos, board)
      where (map)
        manh = manh
      else where
        manh = -1
      end where
      next = find_pos(manh)
    end function


    function unit_dest_to_move(this, others, board) result(pos)
      class(unit_t), intent(in) :: this
      type(unit_t), intent(in) :: others(:)
      character(len=1), intent(in) :: board(:,:)
      integer :: pos(2)

      integer :: dest(size(board,1),size(board,2))
      dest = get_move_destinations(this, others, board)
      pos = find_pos(dest)
    end function


    function get_move_destinations(this, others, board) result(dest)
      class(unit_t), intent(in) :: this
      type(unit_t), intent(in) :: others(:)
      character(len=1), intent(in) :: board(:,:)
      integer :: dest(size(board,1),size(board,2))

      logical :: map(size(board,1),size(board,2))
      integer :: manh(size(board,1),size(board,2))
      integer :: iu

      map = .false.

      ! free squares of enemy units
      do iu=1,size(others)
        ! ignore itself and friendly units
        if (others(iu)%side == this%side) cycle
        if (others(iu)%id == this%id) cycle
        ! ignore non-active units
        if (.not. others(iu)%isalive()) cycle
        map = map .or. free_ngb_squares(others(iu)%pos, board, mypos=this%pos)
      end do

      ! remove not-accessible squares
      manh = manhattan(this%pos, board, accessible_fun)
      where (map .and. manh >= 0)
        dest = manh
      else where
        dest = -1
      end where
    end function get_move_destinations


! =================
! Helper procedures
! =================


    subroutine board_display(this)
      class(board_t), intent(in) :: this
      integer :: i,j
      do i=1,size(this%b,1)
        do j=1,size(this%b,2)
          write(*,'(a1)',advance='no') this%b(i,j)
        end do
        ! TODO - write actual hit points of units
        write(*,*)
      end do
      write(*,'("Round ",i4,"  Elves ",i2,"  Goblins ",i2)') &
      &  this%round, this%nunits(ID_ELF), this%nunits(ID_GOB)
      write(*,*)
    end subroutine board_display


    subroutine array_display_int(arr)
      integer, intent(in) :: arr(:,:)
      integer :: i
      do i=1,size(arr,dim=1)
        write(*,'(*(i2,1x))') arr(i,:)
      end do
    end subroutine

    subroutine array_display_log(arr)
      logical, intent(in) :: arr(:,:)
      integer :: i
      do i=1,size(arr,dim=1)
        write(*,'(*(l1,1x))') arr(i,:)
      end do
    end subroutine


    pure function find_pos(arr) result(pos)    
      integer, intent(in) :: arr(:,:)
      integer :: pos(2)

      integer :: i, j, minv
      pos = 0
      minv = huge(minv)
      do i=1,size(arr,1)
        do j=1,size(arr,2)
          if (arr(i,j) >= minv .or. arr(i,j) < 0) cycle
          pos = [i, j]
          minv = arr(i,j)
        end do
      end do
    end function


    pure function free_ngb_squares(pos, board, mypos) result(map)
      integer, intent(in) :: pos(2)
      character(len=1), intent(in) :: board(:,:)
      integer, intent(in), optional :: mypos(2)
      logical :: map(size(board,1),size(board,2))

      integer :: minpos(2), maxpos(2), i, j
      map = .false.
      do i=1,2
        maxpos(i) = min(size(map,dim=i),pos(i)+1)
        minpos(i) = max(1,pos(i)-1)
      end do
      do i=minpos(1),maxpos(1)
        do j=minpos(2),maxpos(2)
          ! ignore diagonal neighbours and itself
          if (abs(i-pos(1))+abs(j-pos(2))/=1) cycle
          ! ignore not accessible squares
          if (board(i,j)/=CH_EMPT) then
            ! mypos (if present) is assumed available to identify enemies in range
            if (present(mypos)) then
              if (i /= mypos(1) .or. j /= mypos(2)) cycle
            else
              cycle
            end if
          end if
          ! mark and add to the queue
          map(i,j) = .true.
        end do 
      end do
    end function


    function manhattan(pos_start, board, accessible) result(map)
      integer, intent(in) :: pos_start(2)
      character(len=1), intent(in) :: board(:,:)
      integer :: map(size(board,1),size(board,2))
      procedure(accessible_i) :: accessible
!
! TODO - i am using non-efficient allocation/deallocation of whole array
! I should use an queue / priority queue
!
      integer, allocatable :: xq(:), yq(:)

      map = -1
      map(pos_start(1),pos_start(2)) = 0
      allocate(xq(1),yq(1))
      xq(1) = pos_start(1)
      yq(1) = pos_start(2)
      do
        call manhattan_crawl(xq,yq,map,board,accessible)
        if (size(xq)==0) exit
      end do
    end function


    subroutine manhattan_crawl(xq,yq,map,board,accessible)
      integer, intent(inout), allocatable :: xq(:), yq(:)
      integer, intent(inout) :: map(:,:)
      character(len=1), intent(in) :: board(:,:)
      procedure(accessible_i) :: accessible

      integer :: pos(2), maxpos(2), minpos(2), i, j, d0
      logical :: was_swap

      ! accept first task from the queue
      pos = [xq(1), yq(1)]
      xq = xq(2:)
      yq = yq(2:)
      d0 = map(pos(1),pos(2))
      do i=1,2
        maxpos(i) = min(size(map,dim=i),pos(i)+1)
        minpos(i) = max(1,pos(i)-1)
      end do
      ! loop over all first-closest neighbours
      do i=minpos(1),maxpos(1)
        do j=minpos(2),maxpos(2)
          ! ignore diagonal neighbours and itself
          if (abs(i-pos(1))+abs(j-pos(2))/=1) cycle
          ! ignore already visited
          if (map(i,j)/=-1) cycle
          ! ignore not accessible squares
          !if (board(i,j)/=CH_EMPT) cycle
          if (.not. accessible(board(i,j))) cycle
          ! mark and add to the queue
          map(i,j) = d0+1
          xq = [xq, i]
          yq = [yq, j]
        end do
      end do

      ! sort the queue
      do
        was_swap = .false.
        do i=1,size(xq)-1
          if (map(xq(i),yq(i)) > map(xq(i+1),yq(i+1))) then
            call swap(i,i+1)
            was_swap = .true.
          end if
        end do
        if (.not. was_swap) exit
      end do
    contains
      subroutine swap(r,s)
        integer, intent(in) :: r, s
        integer :: tmp
        tmp = xq(r)
        xq(r) = xq(s)
        xq(s) = tmp
        tmp = yq(r)
        yq(r) = yq(s)
        yq(s) = tmp
      end subroutine
    end subroutine manhattan_crawl


    pure function accessible_fun(ch) result(is)
      character(len=1), intent(in) :: ch 
      logical :: is
      is = ch==CH_EMPT
    end function



    elemental function gt_unit(a,b) result(gt)
      class(unit_t), intent(in) :: a, b
      logical :: gt

      ! grweater if greater reading position
      if (a%pos(1) > b%pos(1)) then
        gt = .true.
      else if (a%pos(1) < b%pos(1)) then
        gt = .false.
      else 
        if (a%pos(2) > b%pos(2)) then
          gt = .true.
        else
          gt = .false. ! < or ==
        end if
      end if
    end function gt_unit


    pure function gt_unit_pos(a,bpos) result(gt)
      class(unit_t), intent(in) :: a
      integer, intent(in)       :: bpos(2)
      logical :: gt

      ! grweater if greater reading position
      if (a%pos(1) > bpos(1)) then
        gt = .true.
      else if (a%pos(1) < bpos(1)) then
        gt = .false.
      else 
        if (a%pos(2) > bpos(2)) then
          gt = .true.
        else
          gt = .false. ! < or ==
        end if
      end if
    end function gt_unit_pos


    pure function gt_pos_unit(apos,b) result(gt)
      integer, intent(in)       :: apos(2)
      class(unit_t), intent(in) :: b
      logical :: gt

      ! grweater if greater reading position
      if (apos(1) > b%pos(1)) then
        gt = .true.
      else if (apos(1) < b%pos(1)) then
        gt = .false.
      else 
        if (apos(2) > b%pos(2)) then
          gt = .true.
        else
          gt = .false. ! < or ==
        end if
      end if
    end function gt_pos_unit


    subroutine unit_sort(arr)
      type(unit_t), intent(inout) :: arr(:)
      integer :: i
      logical :: was_swap
      type(unit_t) :: tmp

      do 
        was_swap = .false.
        do i=1,size(arr)-1
          if (arr(i) > arr(i+1)) then
            was_swap = .true.
            tmp = arr(i)
            arr(i) = arr(i+1)
            arr(i+1) = tmp
          end if
        end do
        if (.not. was_swap) exit
      end do
    end subroutine

  end module day1815_mod