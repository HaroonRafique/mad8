      subroutine svstrg(string)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Format a quoted string for SAVE of VIEW command.                   *
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
      integer ileng,istep,istrg,len
      character*(*)     string
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
 
      ileng = len(string)
      if (isvbuf + ileng .gt. 76) call svcont
      isvbuf = isvbuf + 1
      savbuf(isvbuf:isvbuf) = '"'
      istrg = 0
   10 if (istrg .lt. ileng) then
        istep = min(ileng - istrg, 80 - isvbuf)
        savbuf(isvbuf+1:isvbuf+istep) = string(istrg+1:istrg+istep)
        isvbuf = isvbuf + istep
        istrg = istrg + istep
        if (istrg .lt. ileng) then
          write (isave, '(A)') savbuf
          isvbuf = 0
        endif
        go to 10
      endif
      if (isvbuf .ge. 80) then
        write (isave, '(A)') savbuf
        isvbuf = 0
      endif
      isvbuf = isvbuf + 1
      savbuf(isvbuf:isvbuf) = '"'
 
      end
