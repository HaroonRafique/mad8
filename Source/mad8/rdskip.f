      subroutine rdskip(string)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Skip any character(s) occurring in "STRING".                       *
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
      integer index
      character*(*)     string
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
   10 if (jtok .le. ntok  .and.  index(string,token(jtok)) .ne. 0) then
        jtok = jtok + ntok
        if(jtok .lt. ntok) go to 10
      endif
 
      end
