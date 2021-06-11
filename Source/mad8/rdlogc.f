      subroutine rdlogc(lval, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read and decode logical value.                                     *
* Return values:                                                       *
*   LVAL   (logical)    Value decoded.                                 *
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
      integer lword
      logical           lval, eflag
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      logical           dot
      character*8       word
 
      eflag = .false.
      dot = .false.
      if (token(jtok) .eq. '.') then
        dot = .true.
        jtok = jtok + 1
      endif
 
*---- Read token for logical.
      call rdword(word, lword)
      if (lword .eq. 0) go to 800
 
*---- Do dots match?
      if (dot) then
        if (word(lword:lword) .ne. '.') go to 800
        word = word(1:lword-1)
      endif
 
*---- Look for valid values.
      if      (word .eq. 'F'  .or.
     +         word .eq. 'N'  .or.
     +         word .eq. 'FALSE'  .or.
     +         word .eq. 'NO'  .or.
     +         word .eq. 'OFF') then
        lval = .false.
        go to 9999
      else if (word .eq. 'T'  .or.
     +         word .eq. 'Y'  .or.
     +         word .eq. 'TRUE'  .or.
     +         word .eq. 'YES'  .or.
     +         word .eq. 'ON') then
        lval = .true.
        go to 9999
      endif
 
*---- Invalid value.
  800 continue
      call rdfail('RDLOGC', 1, 'Logical value expected.')
 
 9999 end
