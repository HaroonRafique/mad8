      subroutine rdline
      implicit none
*----------------------------------------------------------------------*
* Purpose:
*   Read input line, skip blank lines and comments.
*   Special DOOM version: read lines from buffer if flag
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer jtext,lintxt,ltext,ntext
 
*---- Input line buffer.
      parameter         (ltext = 80)
      common /lnbufc/   text(ltext)
      common /lnbufi/   lintxt, jtext, ntext
      save              /lnbufc/, /lnbufi/
      character*1       text
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      character*(ltext) txtlin
      equivalence       (txtlin, text(1))
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
*---- Give prompt if reading from console.
  100 continue
        if (prompt) write (iqtype, 910)
        read (iqread, 920, end=110) txtlin
        lintxt = lintxt + 1
        go to 120
 
*---- End of file?
  110   continue
        txtlin = 'END_FILE! *** Generated line ***'
 
*---- Write input echo.
  120   continue
        if (echo) then
          if (mod(lintxt, 5) .eq. 0) then
            write (iqlog, 940) lintxt, txtlin
          else
            write (iqlog, 950) txtlin
          endif
        endif
 
*---- Skip comment lines and blank lines.
      if (text(1) .eq. '!'  .or.  txtlin .eq. ' ') go to 100
      jtext = 1
      ntext = ltext
 
  910 format(' M: ==>')
  920 format(a80)
  930 format('Read error on unit',i4,', line number',i6,'.')
  940 format(' ',i9,5x,a80)
  950 format(15x,a80)
 
      end
