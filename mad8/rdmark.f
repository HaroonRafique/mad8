      subroutine rdmark
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print offending statement after an error message.                  *
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
      integer j
 
      write (iqlog, 910) lintok
      write (iqlog, 920) (token(j), j = 1, jtok - 1), ('?', j = 1, 3),
     +                   (token(j), j = jtok, ntok)
      write (iqlog, 930)
 
  910 format(t11,'Statement beginning at line',i6,':')
  920 format(t11,70a1)
  930 format(' ')
 
      end
