      subroutine rdstrg(string, number)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read and decode a quoted string.                                   *
* Output:                                                              *
*   STRING   (char)     Value decoded.                                 *
*   NUMBER   (integer)  Characters actually read.                      *
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
      integer len,length,number
      character*(*)     string
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*1       q
      character*1       bslash
      parameter         (bslash = '\')
 
*---- Expect quote.
      q = token(jtok)
 
      jtok = jtok + 1
      length = len(string)
      number = 0
      string = ' '
 
*---- Loop over characters.
   10 continue
 
*---- Remove escapes, "\". Take following character even if special.
        if (token(jtok) .eq. bslash) then
          jtok = jtok + 1
 
*---- End of string?
        else if (token(jtok) .eq. q) then
          jtok = jtok + 1
          if (token(jtok) .ne. q) go to 9999
        endif
        if (number .lt. length) then
          number = number + 1
          string(number:number) = token(jtok)
        endif
        jtok = jtok + 1
      go to 10
 
 9999 end
