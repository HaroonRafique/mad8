      subroutine aaexec(label, key)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Main switch routine (process code) for command execution.          *
*   Uses pointers to current command and to current keyword.           *
* Input:                                                               *
*   LABEL     (char)    Name of command bank.                          *
*   KEY       (char)    Name of keyword bank.                          *
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
      integer ipr,isp
      character*(mcnam) label, key
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer ietflg,ipnflg, liftbeam,currbeam
      double precision alfa,amass,arad,beta,betas,bunch,bxmax,bymax,
     +charge,cosmux,cosmuy,currnt,deltas,deltat,dtbyds,dxmax,dymax,
     +et,ex,exn,ey,eyn,freq0,gamma,gammas,gamtr,parnum,pc,pdamp,
     +qs,qx,qy,sigdx,sigdy,sige,sigt,sigx,sigxco,sigy,sigyco,sinmux,
     +sinmuy,u0,xcomax,xix,xiy,ycomax,en0,beambv,elkfact,elmfact
 
*---- Particles, emittances and sigmas.
      integer mfact, mbmult
      parameter (mfact = 50, mbmult = 20)
      common /beanam/   prtnam, bsequnam,
     +                  prtnames(mttact), bseqnames(mttact)
      common /beaflt/   amass, charge, en0, pc, gamma,
     +                  ex, exn, ey, eyn, et, sigt, sige,
     +                  bunch, parnum, currnt
      common /beaaux/   sigx, qx, xix, cosmux, sinmux, bxmax, dxmax,
     +                  xcomax, sigxco, sigdx,
     +                  sigy, qy, xiy, cosmuy, sinmuy, bymax, dymax,
     +                  ycomax, sigyco, sigdy,
     +                  qs, alfa, gamtr, deltas, dtbyds, deltat,
     +                  freq0, beta, u0, arad, beambv, pdamp(3),
     +                  gammas, betas,
     +                  elkfact(mfact), elmfact(0:mbmult)
      common /beaint/   ietflg, ipnflg, liftbeam, currbeam
      save   /beaint/
      common /bealog/   fbch, frad
      save              /beanam/, /beaflt/, /beaaux/, /bealog/
      logical           fbch, frad
      character*(mcnam) prtnam, bsequnam, prtnames, bseqnames
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
 
      real              time1, time2, dtime
 
*---- Skip in case of error.
      if (error .or. scan) return
 
*---- Fill in variable references.
      call exfill
      if (.not. error) then
 
*---- Order and evaluate expressions.
        call exordr
        if (.not. error) then
          call exupdt
        endif
      endif
 
*---- Propagate change flags in data structure.
      call aapmod
 
*---- Load in BEAM common from BEAM bank.
      call enget
*---  Save if default
      if (liftbeam .eq. 0)  call enput
*---- Command TRACE option.
      call timex(time1)
      if (trace) then
        write (msg, 910) time1, label, key
  910   format(f12.3,' begin "',a,':',a,'"')
        call aainfo('AAEXEC', 1, msg)
      endif
 
*---- COMMAND dump option.
      if (icmdfl .eq. 1  .or.  icmdfl .eq. 3) then
        call aadump(lccmd)
      endif
      if (icmdfl .eq. 2  .or.  icmdfl .eq. 3) then
        call dzshow('keyword', 0, lckey, 'V', 0, 0, 0, 0)
        call dzshow('command', 0, lccmd, 'V', 0, 0, 0, 0)
      endif
 
*---- Switch on process code.
      ipr = iq(lccmd+mbpr)
      isp = iq(lccmd+mbsp)
 
*---- User-defined process codes.
      if (ipr .le. 0  .or.  ipr .gt. 30) then
        call usercm(ipr, isp)
 
*---- IPR = MPSRV, Pool services section.
      else if (ipr .eq. mpsrv) then
        call aaserv(ipr, isp)
 
*---- IPR = MPFIL, File services section.
      else if (ipr .eq. mpfil) then
        call flmain(ipr, isp)
 
*---- IPR = MPENV, Environment section.
      else if (ipr .eq. mpenv) then
        call enmain(ipr, isp)
 
*---- IPR = MPPLT, Plotting section.
      else if (ipr .eq. mpplt) then
        call plmain(ipr, isp)
 
*---- IPR = MPSUR, Survey section.
      else if (ipr .eq. mpsur) then
        call sumain(ipr, isp)
 
*---- IPR = MPTWS, Twiss section.
      else if (ipr .eq. mptws) then
        call twmain(ipr, isp)
 
*---- IPR = MPMAT, Matching section.
      else if (ipr .eq. mpmat) then
        call mtmain(ipr, isp)
 
*---- IPR = MPTRK, Tracking section.
      else if (ipr .eq. mptrk) then
        call trmain(ipr, isp)
 
*---- IPR = MPHAR, HARMON section.
      else if (ipr .eq. mphar) then
        call hamain(ipr, isp)
 
*---- IPR = MPERR, Error section.
      else if (ipr .eq. mperr) then
        call ermain(ipr, isp)
 
*---- IPR = MPCOR, Correction section.
      else if (ipr .eq. mpcor) then
        call comain(ipr, isp)
 
*---- IPR = MPLIE, Lie algebraic analysis.
      else if (ipr .eq. mplie) then
        call lamain(ipr, isp)
 
*---- IPR = MPEDI, Sequence editor commands.
      else if (ipr .eq. mpedi) then
        call lnedit(ipr, isp)
 
*---- IPR = MPPOL, Polarization commands.
      else if (ipr .eq. mppol) then
        call spmain(ipr, isp)
      endif
 
*---- Command trace option.
      if (trace) then
        call timex(time2)
        dtime = time2 - time1
        write (msg, 920) time2, label, key, dtime
  920   format(f12.3,' end   "',a,':',a,'",',f12.3,' seconds used.')
        call aainfo('AAEXEC', 1, msg)
      endif
 
      end
