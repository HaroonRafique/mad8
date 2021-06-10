      subroutine dcrept(nrept)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode repeat count:   "nrepeat \* data".                          *
*   Use \* to kill ambiguity with * in expressions.                    *
* Output:                                                              *
*   NREPT    (integer)  Repeat count.                                  *
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
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer ichar,idig,jtmp,nrep,nrept
 
      character*1       bslash
      parameter         (bslash = '\')
 
      jtmp = jtok
      nrep = 0
 
*---- Try to read repeat count.
   10 continue
        idig = ichtyp(ichar(token(jtmp)))
        if (idig .ge. 10) go to 20
        nrep = 10 * nrep + idig - 1
        jtmp = jtmp + 1
      go to 10
   20 continue
 
*---- Expect "number \*" or "number ~" if value is repeat count.
      if (jtmp .le. jtok) then
        nrept = 1
      else if (token(jtmp).eq.bslash .and. token(jtmp+1).eq.'*') then
        jtok = jtmp + 2
        nrept = nrep
      else if (token(jtmp) .eq. '~') then
        jtok = jtmp + 1
        nrept = nrep
      else
        nrept = 1
      endif
 
      end
