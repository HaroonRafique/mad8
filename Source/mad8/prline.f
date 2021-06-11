      subroutine prline(iunit)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print a line of dashes.                                            *
* Input:                                                               *
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
      integer iunit
 
      write (iunit,910)
 
  910 format(' ',13('----------'))
 
      end
