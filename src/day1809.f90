module day1809_mod
    use kinds_m, only : I8B
    implicit none

    type :: marble_t
        integer :: value
        type(marble_t), pointer :: cw  => null()
        type(marble_t), pointer :: ccw => null()
    end type

    type :: player_t
        integer(I8B) :: score = 0
    contains
        procedure :: play => player_play
    end type

contains

    subroutine main_play(nplayers, nmarbles, hiscore)
        integer, intent(in)  :: nplayers, nmarbles
        integer(I8B), intent(out) :: hiscore

        type(marble_t), pointer :: current, first
        type(player_t), allocatable :: players(:)
        integer :: ip, im

        allocate(players(nplayers))
        hiscore = 0_I8B
        current => marble_new(0)
        first => current
        ip = 1
        do im=1,nmarbles
!if (mod(im,100000)==0) print *, im, nmarbles, hiscore
!call print_all_marbles(first)
            call players(ip)%play(current, im)
            if (players(ip)%score > hiscore) hiscore=players(ip)%score
!print *, 'player ',ip,' score ', players(ip)%score, ' marble ',im
            ip = mod(ip,nplayers)+1
        end do
        call remove_all_marbles(current)
    end subroutine main_play


    subroutine player_play(this, current, value)
        class(player_t), intent(inout) :: this
        type(marble_t), intent(inout), pointer :: current
        integer, intent(in) :: value

        integer :: value_removed

        if (mod(value,23)==0) then
            ! remove marble
            call remove_marble(current, value_removed)
            this%score = this%score + int(value,kind=I8B) + int(value_removed,kind=I8B)
        else
            ! place marble
            call place_marble(current, value)
        end if
    end subroutine player_play


    function marble_new(value) result(new)
        integer, intent(in) :: value
        type(marble_t), pointer :: new
        allocate(new)
        new%value = value
        new%cw => new
        new%ccw => new
    end function


    subroutine place_marble(current, value)
        type(marble_t), pointer, intent(inout) :: current
        integer, intent(in) :: value
        type(marble_t), pointer :: added, cw1, cw2
        
        ! add new between 1st and 2nd clock-wise
        cw1 => current%cw
        cw2 => cw1%cw
        added => marble_new(value)
        added%ccw => cw1        
        added%cw => cw2
        cw1%cw => added
        cw2%ccw => added
        current => added
    end subroutine place_marble


    subroutine remove_marble(current, value)
        type(marble_t), pointer, intent(inout) :: current
        integer, intent(out) :: value

        integer :: i
        type(marble_t), pointer :: removed

        ! remove 7th counter-clockwise marble
        ! clockwise of removed is new current
        do i=1,7-1
            current => current%ccw
        end do
        removed => current%ccw
        value = removed%value
        removed%ccw%cw => removed%cw
        removed%cw%ccw => removed%ccw
!print *, 'removing....', removed%value
        deallocate(removed)
    end subroutine


    subroutine remove_all_marbles(current)
        type(marble_t), pointer, intent(inout) :: current
        type(marble_t), pointer :: first, removed

!print *, 'In remove_all_marbles'
!call print_all_marbles(current)
        first => current
        if (.not. associated(current)) return
        current => current%cw
        do            
            if (associated(current, first)) exit ! back to first marble
            removed => current
            current => current%cw
!print *, 'removing....', removed%value
            deallocate(removed)
        end do
!print *, 'removing....', current%value
        deallocate(current)
    end subroutine remove_all_marbles


    subroutine print_all_marbles(first)
        type(marble_t), pointer, intent(in) :: first
        type(marble_t), pointer :: current

        current => first
        do
            write(*,'(i2,1x)',advance = 'no') current%value
            current => current%cw
            if (associated(current, first)) exit
        end do
        write(*,*)
    end subroutine print_all_marbles

end module day1809_mod