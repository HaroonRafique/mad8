      subroutine rdinit
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize input buffers.                                          *
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
      integer jtext,lintxt,ltext,ntext
 
*---- Input line buffer.
      parameter         (ltext = 80)
      common /lnbufc/   text(ltext)
      common /lnbufi/   lintxt, jtext, ntext
      save              /lnbufc/, /lnbufi/
      character*1       text
      character*(ltext) txtlin
      equivalence       (txtlin, text(1))
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
*---- Line buffer.
      lintxt = 0
      jtext = ltext + 1
      ntext = ltext
      txtlin = ' '
 
*---- Statement buffer.
      lintok = 0
      jtok = 0
      ntok = 0
 
      end
