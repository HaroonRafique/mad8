      subroutine svcont
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Output one line for SAVE or VIEW command, in view of continuation. *
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
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
 
      if (isvbuf .gt. 0) then
        isvbuf = isvbuf + 1
        savbuf(isvbuf:isvbuf) = '&'
        write (isave, '(A)') savbuf(1:isvbuf)
        isvbuf = 0
      endif
 
      end
