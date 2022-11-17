module day1823_mod
    use iso_fortran_env, only : I8B => int64, WP => real64
    implicit none

    type nanobot_t
        integer(I8B) :: pos(3)
        integer(I8B) :: rad
    contains
        procedure :: isinrange => nanobot_isinrange
    end type nanobot_t
    interface nanobot_t
        module procedure nanobot_new
    end interface


contains
    pure type(nanobot_t) function nanobot_new(str) result(new)
        character(len=*), intent(in) :: str

        character(len=500) :: wrk, spos, rpos
        integer :: p
        wrk = trim(str)
        if (wrk(1:5) /= 'pos=<') error stop 'nanobot_new - expecting "pos=<"'
        wrk = wrk(6:)
        p = scan(wrk,'>')
        if (p==0) error stop 'nanobot_new - ">" not found'
        spos = wrk(:p-1)
        if (wrk(p+1:p+4)/=', r=') error stop 'nanobot_new - expecting ">, r="'
        rpos = wrk(p+5:)

        read(spos,*) new%pos
        read(rpos,*) new%rad
    end function



    pure function manhattan(a, b) result(man)
        integer(I8B), intent(in) :: a(:), b(:)
        integer(I8B) :: man

        integer :: i
        if (size(a)/=size(b)) error stop 'manhattan - vectors must be same'
        man = 0
        do i=1,size(a)
            man = man + abs(a(i)-b(i))
        end do
    end function



    pure logical function nanobot_isinrange(this,pos) result(isinrange)
        class(nanobot_t), intent(in) :: this
        integer(i8B), intent(in) :: pos(:)
        integer(I8B) :: man
        man = manhattan(this%pos, pos)
        isinrange = man <= this%rad
    end function



    pure integer function bots_in_range(pos,bots) result(nbots)
        integer(I8B), intent(in) :: pos(:)
        type(nanobot_t), intent(in) :: bots(:)

        integer :: i
        nbots = 0
        do i=1,size(bots)
            if (bots(i)%isinrange(pos)) nbots = nbots+1
        end do
    end function



    pure real(WP) function bots_field(pos,bots) result(f)
        integer(I8B), intent(in) :: pos(:)
        type(nanobot_t), intent(in) :: bots(:)

        integer :: i
        integer(I8B) :: d
        real(WP), parameter :: IN_RANGE = 1.0, K = 1.0e-8
        f = 0.0
        do i=1,size(bots)
            if (bots(i)%isinrange(pos)) then
                f = f + IN_RANGE
            else
                d = manhattan(pos,bots(i)%pos) - bots(i)%rad
                if (d <= 0) error stop 'bots_field - programming lapse'
                f = f + IN_RANGE*exp(-K*d)
            end if
        end do
    end function


    function best_dir(pos, bots) result(dir)
        integer(I8B), intent(in) :: pos(3)
        type(nanobot_t), intent(in) :: bots(:)
        integer(I8B) :: dir(3)

        integer(I8B) :: i, j, k, bir, maxbir
        real(WP) :: f, fbest

        dir = 0
        fbest = -1.0
        maxbir = 0
        do i=-1,1
        do j=-1,1
        do k=-1,1
            !if (abs(i)+abs(j)+abs(k) == 0) cycle
            bir = bots_in_range(pos+[i,j,k], bots)
            f = bots_field(pos+[i,j,k], bots)
            if (bir > maxbir) then
                maxbir = bir
                fbest = f
                dir = [i,j,k]
            elseif (bir == maxbir .and. f > fbest) then
                fbest = f
                dir = [i,j,k]
            else if (f == fbest .and. sum(abs(pos+[i,j,k])) < sum(abs(pos+dir))) then
                dir = [i,j,k]
            else if (f == fbest .and. sum(abs(pos+[i,j,k])) ==sum(abs(pos+dir))) then
                !print '(3(i0,1x)," --> ",i0,1x,g0)', i, j, k, bir, f
                print *, 'we have a tie'
                !error stop 'we have a tie'
            end if
            !print '(3(i0,1x)," --> ",i0,1x,g0)', i, j, k, bir, f
        end do
        end do
        end do
    end function best_dir



    function best_dir2(pos, bots) result(dir)
        integer(I8B), intent(in) :: pos(3)
        type(nanobot_t), intent(in) :: bots(:)
        integer(I8B) :: dir(3), dis, dismin
        integer :: bir, bir0, i, j, k

        bir0 = bots_in_range(pos,bots)
        dismin = manhattan(pos,int([0,0,0],I8B))
    print *, 'bir0, dis0', bir0, dismin
        dir = 0
        do i=-1,1
        do j=-1,1
        do k=-1,1
            bir = bots_in_range(pos+[i,j,k], bots)
            print *, i,j,k, bir
            if (bir>bir0) then
                print *, 'something bad best_dir2'
                bir0 = bir
                dismin = manhattan(pos+[i,j,k],int([0,0,0],I8B))
                dir = [i,j,k]
                cycle
            elseif (bir<bir0) then
                cycle
            endif
            dis = manhattan(pos+[i,j,k],int([0,0,0],I8B))
            if (dis<dismin) then
                dismin = dis
                dir = [i,j,k]
            end if
        end do
        end do
        end do
    end function best_dir2



    logical function is_better(new, old, bots) 
        integer(I8B), intent(in) :: new(3), old(3)
        type(nanobot_t), intent(in) :: bots(:)
        associate(birnew=>bots_in_range(new,bots), birold=>bots_in_range(old,bots))
        if (birnew > birold) then
            is_better = .true.
        elseif (birnew==birold) then
            associate(fnew=>bots_field(new,bots), fold=>bots_field(old,bots))
                if (fnew > fold) then
                    is_better = .true.
                elseif (fnew==fold) then
                    if (manhattan(new,int([0,0,0],I8B)) < manhattan(old,int([0,0,0],I8B))) then
                        is_better = .true.
                    else 
                        is_better = .false.
                    end if
                else
                    is_better = .false.
                end if
            end associate
        else
            is_better = .false.
        endif
        end associate
    end function

end module day1823_mod