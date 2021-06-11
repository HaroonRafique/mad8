      subroutine aainit
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize data structures.                                        *
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer bbd_max, bbd_cnt, bbd_pos, bbd_loc, bbd_flag
      parameter (bbd_max = 200)
      common / bbcommi / bbd_cnt, bbd_pos, bbd_flag, bbd_loc(bbd_max)
      double precision bb_kick
      common / bbcommr / bb_kick(2, bbd_max)
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer laabnk,laacur,laadef,laakey,laanew,laaold,laaprc,laasrc,
     +laastk,laatar
 
*---- Local links for control module.
      common /aalink/   laabnk, laadef, laasrc, laatar, laakey, laacur,
     +                  laaprc, laastk, laanew, laaold
      save              /aalink/
      double precision coest,cotol
 
*---- Estimate for closed orbit search.
      common /coesti/ coest(6), cotol
      save            /coesti/
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
      integer mmaxel,mwind,ndccnt,ndflag,nditer,ndocc,ndpos,ndtype,
     +nlpos
      double precision admatr,adorbt,adsuml,adtol,orbkpt,reforb,skpt
*--- common block for threader variables
      parameter (mwind = 500, mmaxel = 20000)
      common/thrcml/adthfl, adwofl, adcofl
      logical adthfl, adwofl, adcofl
      common/thrcmi/ndccnt, ndocc, nlpos, nditer,
     +ndpos(mwind), ndtype(mwind), ndflag(mmaxel)
      common/thrcmr/adtol(6), reforb(6), adsuml(mwind),
     +adorbt(6,mwind), admatr(6,6,mwind), orbkpt(6,mmaxel),
     +skpt(mmaxel)
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer l,mbi,mki,mxi
      double precision onem6
*---- Initial directory sizes.
      parameter         (mki = 1000, mbi = 10000, mxi = 1000)
      parameter         (onem6 = 1.0d-6)
      character*(mcnam) label
 
*---- Initialize status flags.
      error  = .false.
      scan   = .false.
      nwarn  = 0
      nfail  = 0
      imodul = 0
      iplflg = 0
      liftseq = 0
      currseq = 0
      liftbeam = 0
      currbeam = 0
      bbd_flag = 0
      bbd_cnt = 0
      sequnam = ' '
      adthfl = .false.
      adwofl = .false.
      adcofl = .false.
      inval  = .false.
      stabx  = .false.
      staby  = .false.
      stabt  = .false.
      newcor = .false.
      newmap = .false.
      optflg(20) = .false.
 
*---- Initialize dynamic store and global links.
      call mzstor(0, '/ /', '.', fence, lroot, iq, iq,
     +            iq(mwflt*memmin), iq(mwflt*memlen))
*     Lift the "Great Master Bank".
      call mzbook(2, lroot, lroot, 1, 'ROOT', mlr, mls, md, 2, 0)
*     Lift the bank for active (USEd) beam lines.
      call mzbook(2, l, lroot, -mcseq, 'ACTS', mttact, mttact, 0, 0, 0)
*     Lift the bank for active (defined) beam commands.
      call mzbook(2, l, lroot, -mbeam, 'BACT', mttact, mttact, 0, 0, 0)
*     Lift the constraint master bank
      call mzbook(2, l, lroot, -mconsm, 'CMAS', mttact, mttact, 0, 0, 0)
*     Global reference links.
      call mzlink(0, '/REFER/', lref1, lref1, lref2)
*     Local links for control module.
      call mzlink(0, '/AALINK/', laabnk, laabnk, laaold)
 
*---- Initialize other modules.
      call uzero(coest, 1, 6*mwflt)
      cotol = onem6
      call dcinit
      call exinit
      call lninit
 
*---- Bank name directories.
      call dimake(mki, mdkey, ldkey)
      call dimake(mbi, mdbnk, ldbnk)
*---- Build KEYWORD mother bank.
*     Lift bank.
      call mzbook(2, lckey, lroot, -mrkey, 'KEYW', 3, 3, mbat, 7, 0)
*     Link to directory.
      label = 'KEYWORD'
      call didefi(ldkey, label, lckey)
*     Store bank description.
      iq(lckey+mbfrm) = 16 * 5 + 2
      iq(lckey+mbnam) = 0
      iq(lckey+mbln) = 0
      iq(lckey+mbpr) = mpkey
      iq(lckey+mbsp) = 1
      iq(lckey+mbat) = 0
 
*---- Build KEYEDIT keyword.
      label = 'KEYEDIT'
      call kwmake(label, 1, 2, 0)
 
*---- Expression and variable reference directories.
*     Link L will not be used, it can be local to AAINIT.
      call mzbook(2, l, lroot, -mdexp, 'EDIR', mxi, 0, 2, 2, 0)
      call mzbook(2, l, lroot, -mdvar, 'VDIR', mxi, 0, 1, 2, 0)
*---- Random generator.
      call init55(123456789)
 
*---- Clear working space.
      iwork = 0
      nwork = 0
      call mzwork(0, dq(1), dq(1), - 1)
 
	call painit(6) 
      end
