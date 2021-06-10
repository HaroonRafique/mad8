      subroutine m66prt(amat, iunit)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print matrix.                                                      *
* Input:                                                               *
*   AMAT(6,6) (real)    Matrix to be printed.                          *
*   IUNIT     (integer) Logical unit number.                           *
*----------------------------------------------------------------------*
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,iunit,j
      double precision amat
      dimension         amat(6,6)
 
      write (iunit, 910) ((amat(i,j), j = 1, 6), i = 1, 6)
 
  910 format(' ',6f12.6)
 
      end
