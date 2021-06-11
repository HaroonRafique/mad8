      subroutine rdtest(string, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Verify that next character is one of those in "STRING".            *
* Input:                                                               *
*   STRING   (char)     Character(s) expected.                         *
* Output:                                                              *
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
      integer index,len
      character*(*)     string
      logical          eflag
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
 
      eflag = .false.
      if (index(string, token(jtok)) .eq. 0) then
        if (len(string) .eq. 1) then
          msg(1) = '"' // string // '" expected.'
          call rdfail('RDTEST', 1, msg)
        else
          msg(1) = 'One of "' // string // '" expected.'
          call rdfail('RDTEST', 1, msg)
        endif
        eflag = .true.
      endif
 
      end
