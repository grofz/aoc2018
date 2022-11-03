  module day1803_mod
    implicit none

  contains

    subroutine make_claims(file, claims, id)
      character(len=*), intent(in) :: file
      integer, intent(out)         :: claims(:,:), id(:,:)

      integer :: fid, ios, i0, i1, i2, im, x0, y0, xsize, ysize, idclaim
      integer :: x, y
      character(len=100) :: line

      claims = 0
      id = 0
      open(newunit=fid, status='old', file=file)

      do
        read(fid,'(a)',iostat=ios) line
        if (ios /= 0) exit
        im = scan(line,'#')
        i0 = scan(line,'@')
        i1 = scan(line,':')
        i2 = scan(line,'x')
        read(line(im+1:i0-1),*) idclaim
        read(line(i0+1:i1-1),*) x0, y0
        read(line(i1+1:i2-1),*) xsize
        read(line(i2+1:),*) ysize
        if (x0+xsize > size(claims,1) .or. y0+ysize > size(claims,2) .or. &
            x0<0 .or. y0<0) error stop 'claims array too small'
        call addclaim(claims(x0+1:x0+xsize, y0+1:y0+ysize))
        if (count(claims(x0+1:x0+xsize, y0+1:y0+ysize)>1) ==0) then
          ! new claim does not overwrite any old claim
          id(x0+1:x0+xsize, y0+1:y0+ysize) = idclaim
        else
          ! claim overwrites existing claim, remove it
          do x=x0+1,x0+xsize
          do y=y0+1,y0+ysize
            if (id(x,y) == 0) cycle
            where(id == id(x,y)) id = 0
          end do
          end do
        end if
      end do

    end subroutine



    elemental subroutine addclaim(a)
      integer, intent(inout) :: a
      a = a + 1
    end subroutine

  end module day1803_mod
