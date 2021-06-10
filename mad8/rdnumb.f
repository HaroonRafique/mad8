      subroutine rdnumb(rval, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read and decode real value.                                        *
* Output:                                                              *
*   RVAL   (real)       Value decoded.                                 *
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
      integer ichar,idig,index,ise,ive,npl
      double precision one,rval,sig,ten,val,zero
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
 
      logical           dig,   pnt
      parameter         (zero = 0.0d0)
      parameter         (one =  1.0d0)
      parameter         (ten = 10.0d0)
 
      eflag = .false.
      rval = zero
 
*---- Any numeric character?
      if (ichtyp(ichar(token(jtok))) .le. 9  .or.
     +    index('+-.',token(jtok)) .ne. 0) then
        val = zero
        sig = one
        ive = 0
        ise = 1
        npl = 0
        dig = .false.
        pnt = .false.
 
*---- Sign.
        if (token(jtok) .eq. '+') then
          jtok = jtok + 1
        else if (token(jtok) .eq. '-') then
          sig = - one
          jtok = jtok + 1
        endif
 
*---- Digit or decimal point?
   10   continue
        idig = ichtyp(ichar(token(jtok)))
        if (idig .le. 9) then
          val = ten * val + float(idig)
          dig = .true.
          if (pnt) npl = npl + 1
          jtok = jtok + 1
          go to 10
        else if (token(jtok) .eq. '.') then
          if (pnt) eflag = .true.
          pnt = .true.
          jtok = jtok + 1
          go to 10
        endif
        eflag = eflag  .or.  (.not. dig)
 
*---- Exponent?
        if (token(jtok) .eq. 'D'  .or.  token(jtok) .eq. 'E'  .or.
     +      token(jtok) .eq. 'd'  .or.  token(jtok) .eq. 'e') then
          jtok = jtok + 1
          dig = .false.
          if (token(jtok) .eq. '+') then
            jtok = jtok + 1
          else if (token(jtok) .eq. '-') then
            ise = -1
            jtok = jtok + 1
          endif
   20     continue
          idig = ichtyp(ichar(token(jtok)))
          if (idig .le. 9) then
            ive = 10 * ive + idig
            dig = .true.
            jtok = jtok + 1
            go to 20
          endif
          if (.not. dig) then
            eflag = .true.
 
*---- Expect a separator after a number group.
          else if (ichtyp(ichar(token(jtok))) .le. 9  .or.
     +             index('.DE', token(jtok)) .ne. 0) then
            call rdskip('0123456789.DE')
            eflag = .true.
          endif
        endif
 
*---- Return value
        if (eflag) then
          call rdfail('RDNUMB', 1, 'Incorrect real value.')
        else
          rval = sig * val * ten ** (ise * ive - npl)
        endif
      else
        call rdfail('RDNUMB', 1, 'Real value expected.')
        eflag = .true.
      endif
 
      end
