      subroutine rdfail(rout, lines, text)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print fatal error message and display offending statement.         *
* Input:                                                               *
*   ROUT      (char)    Calling routine name.                          *
*   LINES     (integer) Number of lines in message.                    *
*   TEXT      (char)    Message.                                       *
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
      integer i,leng,lines
      character*(*)     rout, text(lines)
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      call utleng(text(1), leng)
      write (iqlog, 910) rout, text(1)(1:leng)
      do 90 i = 2, lines
        call utleng(text(i), leng)
        write (iqlog, 920) text(i)(1:leng)
   90 continue
      call rdmark
      nfail = nfail + 1
      error = .true.
 
  910 format(' '/' ',a,'.',t11,'*** Error *** ',a)
  920 format(t11,a)
 
      end
