      subroutine prpage(iunit)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print page header.                                                 *
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
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
      integer iunit
 
      write (iunit,910) ctitle, nvers, cdate, ctime
 
  910 format('1',a80,'  "MAD" Version: ',a8,'  Run: ',a8,'  ',a8)
 
      end
