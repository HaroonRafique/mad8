      subroutine svreal(rval)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Format a real value for SAVE or VIEW command.                      *
* Input:                                                               *
*   RVAL      (real)    Value to be written.                           *
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
      integer iexp,js,ls,ns
      double precision rval,ten
 
      character*25      string
      character*5       expo
      parameter         (ten = 10.0d0)
 
*---- Special case for zero.
      if (rval .eq. 0.0) then
        call svlitt('0.0')
 
*---- Is scaling required?
      else
        if (abs(rval) .lt. 100.0  .and.  abs(rval) .gt. 0.01) then
          iexp = 0
        else
          iexp = log10(abs(rval))
          rval  = rval * ten**(-iexp)
        endif
        write (string(1:20), '(F20.12)') rval
 
*---- Drop trailing zeros.
        do 10 ls = 20, 10, -1
          if (string(ls:ls) .ne. '0') go to 20
   10   continue
        ls = 9
   20   continue
 
*---- Append exponent, if non-zero.
        if (iexp .ne. 0) then
          write (expo, '(''E'',I4)') iexp
          string(ls+1:ls+5) = expo
          ls = ls + 5
        endif
 
*---- Save number string.
        ns = 0
        do 40 js = 1, ls
          if (string(js:js) .ne. ' ') then
            ns = ns + 1
            string(ns:ns) = string(js:js)
          endif
   40   continue
        if (isvbuf + ns .gt. 78) call svcont
        savbuf(isvbuf+1:isvbuf+ns) = string(1:ns)
        isvbuf = isvbuf + ns
      endif
 
      end
