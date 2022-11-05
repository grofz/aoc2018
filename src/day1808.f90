module day1808_mod
    implicit none
contains
    recursive subroutine sum_meta(ind, arr, sm, val)
        integer, intent(inout) :: ind
        integer, intent(in)    :: arr(:)
        integer, intent(out)   :: sm, val

        integer :: nchilds, nmeta, ich, im, sm0
        integer, allocatable :: vals(:)

        if (ind+1 > size(arr)) error stop 'sum_meta - out of array bounds'
        nchilds = arr(ind+0)
        nmeta = arr(ind+1)
        ind = ind+2
        sm = 0
        val = 0
        allocate(vals(nchilds))

        do ich = 1, nchilds
            call sum_meta(ind, arr, sm0, vals(ich))
            sm = sm + sm0
        end do
        
        do im = 1, nmeta
            if (ind > size(arr)) error stop 'sum_meta - out of bounds 2'
            sm = sm + arr(ind)
            if (nchilds==0) then
                val = val + arr(ind)
            else if (arr(ind)>0 .and. arr(ind)<=nchilds) then
                val = val + vals(arr(ind))
            end if
            ind = ind + 1
        end do
    end subroutine sum_meta
end module day1808_mod