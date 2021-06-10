      subroutine tbdata(iform, ival, rval, sval)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode one table field according to format code and length.        *
* Input:                                                               *
*   IFORM     (integer) Zebra format code, with extensions:            *
*                       11 = character, 12 = octal, 13 = hexadecimal.  *
* Output:                                                              *
*   IVAL      (integer) Decoded value, if integer.                     *
*   RVAL      (real)    Decoded value, if real.                        *
*   SVAL      (char)    Decoded value, if string or name.              *
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
      integer ichar,idigit,iform,index,ival
      double precision rval
      character*(*)     sval
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*1       ch
      logical           eflag
 
*---- Skip leading blanks.
   10 if (jtok .le. ntok  .and.  token(jtok) .eq. ' ') then
        jtok = jtok + 1
        go to 10
      endif
 
*==== Unset value.
      if (token(jtok) .eq. '~') then
        jtok = jtok + 1
 
*---- Bit string or integer.
        if (iform .le. 2) then
          ival = intmax
 
*---- Single or double precision real.
        else if (iform .le. 4) then
          rval = fltmax
 
*---- String.
        else if (iform .eq. 5) then
          sval = '~'
 
*---- Character: becomes string.
        else if (iform .eq. 11) then
          iform = 5
          sval = '~'
 
*---- Octal number: becomes bit string.
        else if (iform .eq. 12) then
          iform = 1
          ival = intmax
 
*---- Hexadecimal number: becomes bit string.
        else if (iform .eq. 13) then
          iform = 1
          ival = intmax
        endif
 
*==== Ordinary value.
*---- Integer or bit string.
      else
        if (iform .le. 2) then
          call rdint(ival, eflag)
 
*---- Real: Always returned in double precision.
        else if (iform .le. 4) then
          call rdnumb(rval, eflag)
 
*---- String.
        else if (iform .eq. 5) then
          call tbname(sval)
 
*---- Character: becomes string.
        else if (iform .eq. 11) then
          iform = 5
          sval = token(jtok)
          jtok = jtok + 1
 
*---- Octal number: becomes bit string.
        else if (iform .eq. 12) then
          iform = 1
          ival = 0
   20     idigit = index('01234567',token(jtok)) - 1
          if (idigit .ge. 0) then
            ival = 8 * ival + idigit
            jtok = jtok + 1
            go to 20
          endif
 
*---- Hexadecimal number: becomes bit string.
        else if (iform .eq. 13) then
          iform = 1
          ival = 0
   30     ch = ch2upp(ichar(token(jtok)))
          idigit = index('0123456789ABCDEF',ch) - 1
          if (idigit .ge. 0) then
            ival = 16 * ival + idigit
            jtok = jtok + 1
            go to 30
          endif
        endif
      endif
 
      end
