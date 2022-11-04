module day1806_mod
    use parse_mod, only : string_t, read_strings
    implicit none

    type, public :: point_t
        integer :: x(2) = [-1, -1]
        integer :: area = 0
        logical :: is_border = .false.
    end type

    type, public :: area_t
        integer, allocatable :: a(:,:) ! index to the closest point
        integer, allocatable :: b(:,:) ! total Manhattan distance to all points
    contains
        procedure :: populate => area_populate
        procedure :: area_count
    end type

contains

    subroutine area_populate(this, points)
        class(area_t), intent(out) :: this
        type(point_t), intent(inout) :: points(:)

        integer :: xlim0(2), xlim1(2), i, ip(2), i1, i2

        ! allocate board of necessary size
        xlim0 =  huge(xlim0)
        xlim1 = -huge(xlim1)
        do i=1,size(points)
            associate(x=>points(i)%x)
                where(x > xlim1)
                    xlim1 = x
                end where
                where(x < xlim0)
                    xlim0 = x
                end where
            end associate
        end do
        allocate(this%a(xlim0(1)-1:xlim1(1)+1, xlim0(2)-1:xlim1(2)+1))
        allocate(this%b(xlim0(1)-1:xlim1(1)+1, xlim0(2)-1:xlim1(2)+1))
        this%a = -1
        this%b = 0
print *, 'lbounds =', lbound(this%a)
print *, 'ubounds =', ubound(this%a)

        ! for each square, calculate the closest point ...
        ! ... and also calculate the sum of Manhattan distances to all points
        do i1 = lbound(this%a,dim=1), ubound(this%a,dim=1)
        do i2 = lbound(this%a,dim=2), ubound(this%a,dim=2)
            ip = [i1, i2]
            this%a(ip(1),ip(2)) = closest_point(ip, points)

            associate(b=>this%b(ip(1),ip(2)))
                do i=1,size(points)
                    b = b + abs(ip(1)-points(i)%x(1)) + abs(ip(2)-points(i)%x(2))
                end do
            end associate
        end do
        end do
    end subroutine



    subroutine area_count(this, points, max_points_area)
        class(area_t), intent(in) :: this
        type(point_t), intent(inout) :: points(:)
        integer, intent(out) :: max_points_area

        integer :: i, i1, i2, ip(2)
        integer :: total_area, tie_area, points_area

        ! initialize points
        do i=1,size(points)
            points(i)%area = 0
            points(i)%is_border = .false.
        end do

        ! for each square, add to total count
        do i1 = lbound(this%a,dim=1), ubound(this%a,dim=1)
        do i2 = lbound(this%a,dim=2), ubound(this%a,dim=2)
            ip = [i1, i2]
            associate(index=>this%a(ip(1),ip(2)))
                if (index>0) then
                    points(index)%area = points(index)%area + 1
                    if (ip(1)==lbound(this%a,dim=1) .or. ip(1)==ubound(this%a,dim=2) .or. &
                    &   ip(2)==lbound(this%a,dim=2) .or. ip(2)==ubound(this%a,dim=2)) &
                    &       points(index)%is_border = .true.
                end if
            end associate
        end do
        end do

        ! validate & find maximum area
        total_area = size(this%a,dim=1) * size(this%a,dim=2)
        tie_area = count(this%a == 0)
        points_area = 0
        max_points_area = 0
        do i=1,size(points)
            points_area = points_area + points(i)%area
            if (points(i)%area > max_points_area .and. .not. points(i)%is_border) then
                max_points_area = points(i)%area
            end if
        end do

        print '("Area: total ",i0,"  tied ",i0,"  points ",i0)', total_area, tie_area, points_area
        if (total_area-tie_area-points_area /= 0 ) error stop 'area_count: inconsistency'
        print '("Max area: ",i0)', max_points_area
    end subroutine area_count



    function closest_point(x, points) result(index)
        integer, intent(in) :: x(2)
        type(point_t), intent(in) :: points(:)
        integer :: index !must be in range 1:size(points) or 0 (if there is a tie)

        logical :: is_tie
        integer :: closest_dist, actual_dist, i

        closest_dist = huge(closest_dist)
        index = 0 ! is tie is true

        do i=1,size(points)
            actual_dist = abs(x(1)-points(i)%x(1)) + abs(x(2)-points(i)%x(2))
            if (actual_dist < closest_dist) then
                index = i ! is tie is false
                closest_dist = actual_dist
            else if (actual_dist == closest_dist) then
                index = 0 ! is tie is true
            end if
        end do
    end function



    function read_points_from_file(file) result(points)
        character(len=*), intent(in) :: file
        type(point_t), allocatable :: points(:)

        type(string_t), allocatable :: lines(:)
        integer :: npoints, i

        lines = read_strings(file)
        npoints = size(lines)
        allocate(points(npoints))
        do i = 1, npoints
            read(lines(i)%str,*) points(i)%x(1), points(i)%x(2)
        end do
    end function

end module day1806_mod