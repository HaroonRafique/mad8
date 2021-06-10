      subroutine flcall(iflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   CALL and RETURN commands.                                          *
* Input:                                                               *
*   IFLAG     (integer) Case distinction: 1 = CALL, 2 = RETURN.        *
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
      integer lcali,lcatt,lccls,lccmd,lccom,lcdef,lcelm,lcexp,lcfld,
     +lckey,lcseq,lcspl,lcsrc,lcvar,ldbnk,ldkey,lref1,lref2,lsali,lscom,
     +lsdir,lsfld,lsflg,lsnum,lsspl,lbeam,lconsm,ldummy
 
*---- Global reference links.
      common /refer/    lref1,
     +                  lcali, lcatt, lccls, lccmd, lccom, lcdef, lcelm,
     +                  lcexp, lcfld, lckey, lcseq, lcspl, lcsrc, lcvar,
     +                  lbeam, lconsm, ldbnk(4), ldkey(4), ldummy(10),
     +                  lsali, lscom, lsdir, lsfld, lsflg, lsnum, lsspl,
     +                  lref2
      save              /refer/
      integer liftseq, currseq
      common /seqinfi/ liftseq, currseq
      character * (mcnam) sequnam, seqnames
      common /seqinfc/ sequnam, seqnames(mttact)
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
      integer icall,iflag,level,lines,maxcal
 
      parameter         (maxcal = 10)
      integer           ifilst(maxcal), ilinst(maxcal)
      real              time1(maxcal), time2
      save              ifilst, ilinst, level, time1
      character*(mcfil) filnam
      logical           intrac
 
      data level        / 0 /
 
*---- Input line should be empty.
      if (jtext .lt. ntext) then
        call aawarn('FLCALL', 1,
     +  'Text after CALL or RETURN command skipped.')
        jtext = ntext
      endif
 
*---- Distinguish CALL/RETURN.
      if (iflag .eq. 1) then
 
*---- Test for CALL stack overflow.
        if (level .ge. maxcal) then
          write (msg, 910) maxcal
  910     format('CALL''s cannot be nested more than ',i2,
     +           ' levels deep --- CALL ignored.')
          call aafail('FLCALL', 1, msg)
 
*---- Open call file.
        else
          filnam = 'call'
          call utgstr(lccmd, 1, 1, filnam)
 
*---- Call to terminal input.
          if (filnam(1:4) .eq. 'TERM') then
            if (iqread .eq. iqttin) then
              call rdwarn('FLCALL', 1,
     +        'Already reading standard input --- CALL ignored.')
            else if (inter) then
              level = level + 1
              ifilst(level) = iqread
              ilinst(level) = lintxt
              call timex(time1(level))
              iqread = iqttin
              lintxt = 0
              call aainfo('FLCALL', 1, 'Now reading standard input.')
              prompt = intrac()
            else
              call rdwarn('FLCALL', 1,
     +        'Not running interactively --- cannot CALL terminal.')
            endif
 
*---- Stack old input file and switch to new one.
          else
            call flopen(filnam, 'SRFD', 0, 0, icall, error)
            if (.not. error) then
              level = level + 1
              ifilst(level) = iqread
              ilinst(level) = lintxt
              call timex(time1(level))
              iqread = icall
              lintxt = 0
              call flname(icall, filnam)
              msg(1) = 'Now reading file: ' // filnam
              call aainfo('FLCALL', 1, msg)
              prompt = .false.
            endif
          endif
        endif
 
*---- Perform RETURN command.
      else
 
*---- In case of CALL stack underflow, perform STOP.
        if (level .le. 0) then
          call aawarn('FLCALL', 1,
     +    'RETURN on main input file --- Replaced by STOP.')
          call zend
          stop
 
*---- Close call file and switch to previous input file.
        else
          if (iqread .ne. iqttin) call flclos(iqread, error)
          iqread = ifilst(level)
          lintxt = ilinst(level)
          call flname(iqread, filnam)
          if (trace) then
            lines = 2
            call timex(time2)
            write (msg, 930) time2 - time1(level)
  930       format(f12.3,' seconds used to read file.')
          else
            lines = 1
          endif
          msg(lines) = 'Now reading file: ' // filnam
          call aainfo('FLCALL', lines, msg)
          level = level - 1
 
*---- If new input file is terminal, turn off scanning mode.
          if (level .eq. 0) then
            prompt = intrac()
            if (scan  .and.  inter  .and.  iqread .eq. iqttin) then
              call aainfo('FLCALL', 1, 'Leaving scanning mode.')
              scan = .false.
            endif
          endif
        endif
      endif
 
      end
