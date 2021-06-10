      subroutine ncopy(i1, i2, n)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Copy integer arays
*----------------------------------------------------------------------*
      integer i1(*), i2(*), n, i
      do i = 1, n
        i2(i) = i1(i)
      enddo
      end
