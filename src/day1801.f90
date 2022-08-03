  module day1801_mod
    use abstract_container, only : ERR_CONT_OK, ERR_CONT_ISNOT
    use tree_m, only : rbtr_t, DAT_KIND, tree_mold
    implicit none

    type map_t
      integer :: i
    end type

  contains

    subroutine read_and_count(file, cur, ans1, ans2, tree)
      character(len=*), intent(in) :: file
      integer, intent(out)         :: ans1, ans2
      integer, intent(inout)       :: cur
      type(rbtr_t), intent(inout)  :: tree

      integer :: fid, ios, i

      ans1 = 0
      open(newunit=fid, file=file, status='old')
      do
        read(fid,*,iostat=ios) i
        if (ios /= 0) exit
        ans1 = ans1 + i
        cur = cur + i
        call add_val(cur, tree, ans2)
        if (ans2 /= 0) exit
      end do
      close(fid)

    end subroutine



    subroutine add_val(cur, tree, ans)
      integer, intent(in)                 :: cur
      integer, intent(out)                :: ans
      type(rbtr_t), intent(inout)         :: tree

      integer, allocatable :: wrk(:)
      integer :: i, ierr
      integer(DAT_KIND), allocatable :: handle(:)
      type(map_t) :: map

      ans = 0
      map%i = cur

      call tree % Find(transfer(map,tree_mold), handle, ierr)
      select case(ierr)
      case(ERR_CONT_OK)
        ans = cur
      case(ERR_CONT_ISNOT)
        call tree % Add(transfer(map,tree_mold))
      case default
        error stop 'unknown ierr'
      end select

    end subroutine


    pure integer function compare(a, b) result(res)
      integer(DAT_KIND), intent(in) :: a(:), b(:)

      type(map_t) :: amap, bmap
      amap = transfer(a, amap)
      bmap = transfer(b, bmap)

      if (amap%i == bmap%i) then
        res = 0
      else if (amap%i < bmap%i) then
        res = 1
      else
        res = -1
      end if
    end function

  end module day1801_mod
