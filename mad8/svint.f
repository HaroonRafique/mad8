      subroutine svint(ival)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Format an integer value for SAVE of VIEW command.                  *
* Input:                                                               *
*   IVAL      (integer) Value to be written.                           *
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
      integer ival,js,ns
 
      character*10      string
 
*---- Encode integer.
      write (string, '(I10)') ival
 
*---- Save number string.
      do 10 js = 1, 10
        if (string(js:js) .ne. ' ') go to 20
   10 continue
   20 continue
      ns = 11 - js
      if (isvbuf + ns .gt. 78) call svcont
      savbuf(isvbuf+1:isvbuf+ns) = string(js:10)
      isvbuf = isvbuf + ns
 
      end
