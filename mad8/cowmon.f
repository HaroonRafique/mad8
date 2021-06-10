      subroutine cowmon
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find closed orbit and write a TFS table of monitor readings.       *
* PUTORBIT command:                                                    *
*   FILENAME  (string)  TFS file name to be written.                   *
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
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      integer lcobuf,lcocor,lcoelm,lcomon,lcotab
 
*---- Links for closed orbit correction module.
      common /colink/   lcotab, lcobuf, lcocor, lcomon, lcoelm
      save              /colink/
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
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
      integer idir,iunit,jbit,jrow,mcol,ncol,nrow
      double precision zero
 
      parameter         (mcol = 4, zero = 0.0d0)
      character*(mcfil) filnam
      character*(mcnam) monnam, tabnam, cnam(mcol)
      integer           icfrm(mcol)
 
      data cnam         / 'PUNAME', 'X', 'Y', 'STATUS' /
      data tabnam       / '*ORBIT TABLE*' /
 
*---- Check main beam line.
      call lnchck('PUTORBIT', error)
      if (error) go to 9999
 
*---- Set up corrector and monitor table.
      call cotble(error)
      if (error) go to 9999
 
*---- Find closed orbit and monitor readings.
      call tmturn(lcseq, zero, error)
      if (error) go to 9999
 
*---- Fill in monitor readings and dispersion.
      call cofill
      if (error) go to 9999
 
*---- Retrieve file name.
      filnam = 'orbit'
      call utgstr(lccmd, 1, 1, filnam)
      if (filnam .eq. ' ') go to 9999
 
*---- Build internal table.
      nrow  = 0
      lccom = lq(lcseq-msmon)
   10 if (lccom .ne. 0) then
        nrow  = nrow + 1
        lccom = lq(lccom-1)
        go to 10
      endif
      lccom = lq(lcseq-msmon)
      ncol = mcol
      icfrm(1) = 5
      icfrm(2) = 3
      if (double) icfrm(2) = mreal
      icfrm(3) = icfrm(2)
      icfrm(4) = 1
      call tbcrea(tabnam, 1, nrow, ncol, cnam, icfrm, 1, lcotab)
      call tbpdsc(lcotab, 'TYPE', 5, 0, zero, 'ORBIT')
      do 90 jrow = 1, nrow
        call tbset(lcotab, jrow, 3, lcobuf)
        call ucopy(q(lccom+1), xcm, iq(lccom-1))
        xcm = xcm * 1000.0
        ycm = ycm * 1000.0
        idir = iq(lq(lcseq-msdir)+iq(lccom-5))
        call diname(ldbnk, idir, monnam)
        call uctoh(monnam, iq(lcobuf+1), mcwrd, mcnam)
        if (jbit(iq(lccom),3) .eq. 0) xcm = 0.0
        if (jbit(iq(lccom),4) .eq. 0) ycm = 0.0
        if (double) then
          call ucopy(xcm, q(lcobuf+mwnam+1), 2*mwflt)
          iq(lcobuf+mwnam+2*mwflt+1) = 0
        else
          q(lcobuf+mwnam+1) = xcm
          q(lcobuf+mwnam+2) = ycm
          iq(lcobuf+mwnam+3) = 0
        endif
        lccom = lq(lccom-1)
   90 continue
      call tbclos(lcotab)
 
*---- Write table in TFS format.
      call flopen(filnam, 'SWFD', 0, 0, iunit, error)
      if (.not. error) then
        call tbwtfs(tabnam, iunit)
        call flname(iunit, filnam)
        call flclos(iunit, error)
        if (.not. error) then
          msg(1) = 'Monitor readings written on file: ' // filnam
          call aainfo('COWMON', 1, msg)
        endif
      endif
      call tbopen(tabnam, 0, lcotab)
      call tbdrop(lcotab)
 
 9999 end
