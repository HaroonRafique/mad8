      subroutine tbname(sval)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode a string in TFS system notation.                            *
* Output:                                                              *
*   SVAL      (char)    String read.                                   *
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
      integer ilen,len
      character*(*)     sval
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
*---- Skip leading blanks.
      sval = ' '
      ilen = 0
   10 if (jtok .le. ntok) then
         if (token(jtok) .eq. ' ') then
           jtok = jtok + 1
           go to 10
         endif
 
*---- If quoted string, go to RDSTRG.
        if (token(jtok) .eq. '"') then
          call rdstrg(sval, ilen)
 
*---- Otherwise, string is up to next blank.
        else
   20     continue
            if (ilen .lt. len(sval)) then
              ilen = ilen + 1
              sval(ilen:ilen) = token(jtok)
            endif
            jtok = jtok + 1
          if (token(jtok) .ne. ' ') go to 20
        endif
      endif
 
      end
