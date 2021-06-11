      subroutine dcindx(index1, index2, index3, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode subscript list.                                             *
* Output:                                                              *
*   INDEX1, INDEX2, INDEX3 (integer)   Decoded subscripts (default=1). *
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
      integer index1,index2,index3
      logical           eflag
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      eflag = .false.
      index1 = 1
      index2 = 1
      index3 = 1
      if (token(jtok) .eq. '(') then
        jtok = jtok + 1
        call rdint(index1, eflag)
        if (eflag) go to 9999
        if (token(jtok) .eq. ',') then
          jtok = jtok + 1
          call rdint(index2, eflag)
          if (eflag) go to 9999
          if (token(jtok) .eq. ',') then
            jtok = jtok + 1
            call rdint(index3, eflag)
            if (eflag) go to 9999
          endif
        endif
        if (token(jtok) .eq. ',') then
          call rdfail('DCINDX', 1, 'At most 3 dimensions allowed.')
          eflag = .true.
        else if (token(jtok) .ne. ')') then
          call rdfail('DCINDX', 1, '"," or ")" expected.')
          eflag = .true.
        else
          jtok = jtok + 1
        endif
      endif
 
 9999 end
