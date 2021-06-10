      subroutine rdword(word, number)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read and decode a possibly quoted identifier.                      *
* Output:                                                              *
*   WORD     (char*(*)) Decoded value.                                 *
*   NUMBER   (integer)  Number of characters actually read.            *
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
      integer ichar,index,len,length,number
      character*(*)     word
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
 
      length = len(word)
      number = 0
      word = ' '
 
*---- If quoted, call string decoder.
      if (token(jtok) .eq. '"'  .or.  token(jtok) .eq. '''') then
        call rdstrg(word, number)
 
*---- Expect a letter to start word.
      else if (ichtyp(ichar(token(jtok))) .eq. 10) then
 
*---- Pack character to word.
   10   continue
          if (number .lt. length) then
            number = number + 1
            word(number:number) = token(jtok)
          endif
          jtok = jtok + 1
        if (ichtyp(ichar(token(jtok))) .le. 10) go to 10
        if (index('.''_', token(jtok)) .ne. 0) go to 10
      endif
 
      end
