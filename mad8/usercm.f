      subroutine usercm(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   This routine is called for all codes which are not predefined.     *
*   It can be replaced to hook in user commands.                       *
* Input:                                                               *
*   IPR       (integer) Process code.                                  *
*   ISP       (integer) Subprocess code.                               *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ipr,isp
      write (msg, 910) ipr, isp
      call rdwarn('USERCM', 1, msg)
  910 format('Unknown command, PR =',i5,', SP =',i5,'.')
      end
