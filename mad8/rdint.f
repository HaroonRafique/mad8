      subroutine rdint(ivalue, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read and decode integer.                                           *
* Return values:                                                       *
*   IVALUE   (integer)  Value decoded.                                 *
*   EFLAG    (logical)  Error flag.                                    *
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
      integer ichar,idig,index,ival,ivalue
      logical           eflag
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
      logical           minus
 
      eflag = .true.
      minus = .false.
      if (token(jtok) .eq. '+') then
        jtok = jtok + 1
      else if (token(jtok) .eq. '-') then
        jtok = jtok + 1
        minus = .true.
      endif
      ival = 0
   10 continue
      idig = ichtyp(ichar(token(jtok)))
      if (idig .le. 9) then
        ival = 10 * ival + idig
        eflag = .false.
        jtok = jtok + 1
        go to 10
      endif
      if (minus) ival = - ival
      if (eflag) then
        call rdfail('RDINT', 1, 'Integer expected.')
        ivalue = 0
      else if (index('.DE', token(jtok)) .ne. 0) then
        call rdskip('0123456789.E')
        call rdfail('RDINT', 1, 'Real value not allowed.')
        eflag = .true.
        ivalue = 0
      else
        ivalue = ival
      endif
 
      end
