      subroutine userdf(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   This routine is called for all codes which are not predefined.     *
*   It can be replaced to hook in user definitions.                    *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ipr,isp
 
      write (msg, 910) ipr, isp
  910 format('Unknown definition, PR =',i5,', SP =',i5,'.')
      call rdwarn('USERDF', 1, msg)
 
      end
