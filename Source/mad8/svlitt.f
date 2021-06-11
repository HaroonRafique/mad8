      subroutine svlitt(string)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Format an string for SAVE or VIEW command.                         *
* Input:                                                               *
*   STRING    (char)    String to be output.                           *
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
      integer len,ns
      character*(*)     string
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
 
      ns = len(string)
      if (isvbuf + ns .ge. 78) call svcont
      savbuf(isvbuf+1:isvbuf+ns) = string
      isvbuf = isvbuf + ns
 
      end
