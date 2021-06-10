      subroutine emdamp(em1, em2, orb1, orb2, re)
      implicit none
*---------------------------------------------------------------------*
* Purpose:                                                            *
*   Deal with radiation damping in an element.                        *
* Input:                                                              *
*   EM1(6,6)  (real)    Matrix of eigenvectors at entrance.           *
*   EM2(6,6)  (real)    Matrix of eigenvectors at exit.               *
*   ORB1(6)   (real)    Orbit position at entrance.                   *
*   ORB2(6)   (real)    Orbit position at exit.                       *
* Input/output:                                                       *
*   RE(6,6)   (real)    Transfer matrix for the element; changed on   *
*                       output to contain damping.                    *
*---------------------------------------------------------------------*
* Modified: 11-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Included FINTX attribute in RBEND, SBEND, and GBEND (NOTE: FINTX   *
*   is assumed to have same value as FINT if it is not set or is       *
*   negative as the default in the dict file)                          *
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
      integer i,ii,j,iord,ir,isp,n,na,nd,ne,nord
      double precision an,ang,bi2gi2,corr,ct,data,di,dr,drt,e1,e2,e5sq1,
     +e5sq2,e5sqs1,e5sqs2,edg1,edg2,ek0,el,em1,em2,f1,f1s,f2,f2s,fact1,
     +fact1x,fact2,fact2x,ferror,fh,fh1,fh2,field,fint,four,h,h1,
     +h2,half,hbi,hcb,hcb1,hcb2,hcbs1,hcbs2,hgap,hx,hxx,hxy,hy,hyx,hyy,
     +o1,o2,one,orb1,orb2,pt1,pt2,px1,px2,py1,py2,r1sq,r2sq,re,rfac,
     +rfac1,rfac1x,rfac1y,rfac2,rfac2x,rfac2y,rfacx,rfacy,rff,rffrq,rfl,
     +rflag,rfv,rfvlt,rw,six,sk1,sk2,sk3,sks,st,str,t1,t2,tedg1,tedg2,
     +ten3m,ten6p,three,tilt,time,tw,twelve,two,twon,val,x,x1,x2,xkick,
     +y,y1,y2,ykick,zero
      dimension         em1(6,6), em2(6,6), orb1(6), orb2(6), re(6,6)
      double precision an2
      double precision fintx
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer meangb,meangg,meangr,mechg,mee1b,mee1g,mee2b,mee2g,meflde,
     +mefrqc,megapb,megapg,meh1b,meh1g,meh2b,meh2g,mehrmc,meintb,meintg,
     +mek1b,mek1g,mek1q,mek2b,mek2s,mek3b,mek3o,mekick,meklm,meksg,
     +mekss,melagc,melen,mesigx,mesigy,metltb,metlte,metltg,metltm,
     +metlto,metltq,metlts,metyp,mevltc,mexcol,mexma,meycol,meyma
      integer meintbx,meintgx,meapro,mek0lm,met0m,mek1lm,met1m,
     +mek2lm,met2m,mek3lm,met3m,meaprm,meapss,melosc,meaprc,mee0l,
     +medel,mephil,mefrql,melosl,mevoll,melagl,meaprl
 
*---- Bias for element attribute values.
*     These statements MUST be consistent with the command dictionary.
*     Routines using this group must also include BANKHEAD and CMDGROUP.
*     Common to all elements: TYPE and L attributes.
      parameter    (metyp  = mbat   + mcval, melen  = metyp  + mcsiz)
*     Common to RBEND and SBEND.
      parameter    (meangb = melen  + mcsiz, mek1b  = meangb + mcsiz,
     +              mee1b  = mek1b  + mcsiz, mee2b  = mee1b  + mcsiz,
     +              metltb = mee2b  + mcsiz, mek2b  = metltb + mcsiz,
     +              meh1b  = mek2b  + mcsiz, meh2b  = meh1b  + mcsiz,
     +              megapb = meh2b  + mcsiz, meintb = megapb + mcsiz)
      parameter (meintbx = meintb + mcsiz, mek3b  = meintbx + mcsiz)
*     QUADRUPO.
      parameter    (mek1q  = melen  + mcsiz, metltq = mek1q  + mcsiz)
      integer meaprq
      parameter    (meaprq = metltq + mcsiz)
*     SEXTUPOL.
      parameter    (mek2s  = melen  + mcsiz, metlts = mek2s  + mcsiz)
      integer meaprs
      parameter    (meaprs = metlts + mcsiz)
*     OCTUPOLE.
      parameter    (mek3o  = melen  + mcsiz, metlto = mek3o  + mcsiz)
      parameter    (meapro = metlto + mcsiz)
*     MULTIPOL.
      parameter    (mek0lm = melen  + mcsiz, met0m  = mek0lm + mcsiz,
     +              mek1lm = met0m  + mcsiz, met1m  = mek1lm + mcsiz,
     +              mek2lm = met1m  + mcsiz, met2m  = mek2lm + mcsiz,
     +              mek3lm = met2m  + mcsiz, met3m  = mek3lm + mcsiz,
     +              meaprm = melen  + 21*mcsiz)
*     MULTIPOL.
      parameter    (meklm  = melen  + mcsiz, metltm = meklm  + mcsiz)
*     SOLENOID.
      parameter    (mekss  = melen  + mcsiz, meapss = mekss  + mcsiz)
*     RFCAVITY.
      parameter    (mevltc = melen  + mcsiz, melagc = mevltc + mcsiz,
     +              mefrqc = melagc + mcsiz, mehrmc = mefrqc + mcsiz)
      parameter    (melosc = mehrmc + 5*mcsiz,
     +              meaprc = melosc + 3*mcsiz)
*     ELSEPARA.
      parameter    (meflde = melen  + mcsiz, metlte = meflde + mcsiz)
*     Common to SROT and YROT.
      parameter    (meangr = melen  + mcsiz)
*     Common to KICK, HKICK, and VKICK.
      parameter    (mekick = melen  + mcsiz)
*     Common to ECOLLIMA and RCOLLIMA.
      parameter    (mexcol = melen  + mcsiz, meycol = mexcol + mcsiz)
*     BEAMBEAM.
      parameter    (mesigx = melen  + mcsiz, mesigy = mesigx + mcsiz,
     +              mexma  = mesigy + mcsiz, meyma  = mexma  + mcsiz,
     +              mechg  = meyma  + mcsiz)
*     GBEND.
      parameter    (meangg = melen  + mcsiz, mek1g  = meangg + mcsiz,
     +              mee1g  = mek1g  + mcsiz, mee2g  = mee1g  + mcsiz,
     +              metltg = mee2g  + mcsiz, meksg  = metltg + mcsiz,
     +              meh1g  = meksg  + mcsiz, meh2g  = meh1g  + mcsiz,
     +              megapg = meh2g  + mcsiz, meintg = megapg + mcsiz)
*     lcavity.
      parameter    (mee0l  = melen  + mcsiz, medel  = mee0l  + mcsiz,
     +              mephil = medel  + mcsiz, mefrql = mephil + mcsiz,
     +              melosl = mefrql + mcsiz, mevoll = melosl + mcsiz,
     +              melagl = mevoll + mcsiz, meaprl = melagl + mcsiz)
      parameter (meintgx = meintg + mcsiz)
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      double precision cg,sum,sumu0
 
*---- Communication area for radiation damping.
      common /emdata/   cg, sum(3), sumu0
      save   /emdata/
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
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
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
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
      integer maxmul
 
*---- Maximum order of multipoles.
      parameter         (maxmul = 20)
      double precision amu0,asube,asubp,clight,elamda,emass,eps0,erad,
     +falfa,hbar,plamda,pmass,qelect,mumass
 
*---- Universal physical constants.
*     Velocity of light [m/s]:
      parameter         (clight = 2.997 924 58 d+08)
*     Permeability of vacuum [V*s/A*m]:
      parameter         (amu0   = 1.256 637 061d-06)
*     Permittivity of vaccum [A*S/V*m]:
      parameter         (eps0   = 8.854 187 817d-12)
*     Reduced Plack's constant [GeV*s]:
      parameter         (hbar   = 6.58211889d-25)
 
*---- Electromagnetic constants.
*     Elementary charge [A*s]:
      parameter         (qelect = 1.602176462d-19)
*     Fine structure constant [1]:
      parameter         (falfa  = 7.297 353 08 d-03)
 
*---- Electron.
*     Rest mass [GeV]:
      parameter         (emass  = 0.510998902d-3)
*     Classical radius [m]:
      parameter         (erad   = 2.817940285d-15)
*     Reduced Compton wavelength [m]:
      parameter         (elamda = 3.861 593 23 d-13)
*     Magnetic moment anomaly [1]:
      parameter         (asube  = 1.159 652 193d-03)
 
*---- Proton.
*     Rest mass [GeV]:
      parameter         (pmass  = 0.938271998d+00)
*     Reduced Compton wavelength [m]:
      parameter         (plamda = 2.103 089 37 d-16)
*     Magnetic moment anomaly [1]:
      parameter         (asubp  = 1.792 847 386d+00)
 
*---- Muon.
*     Rest mass [GeV]:
      parameter         (mumass  = 0.1056583568d+00)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (ten3m = 1.0d-3, ten6p = 1.0d+6)
      parameter         (zero  = 0.0d0,  half  = 0.5d0)
      parameter         (one   = 1.0d0,  two   = 2.0d0)
      parameter         (three = 3.0d0,  twelve = 12.d0)
      parameter         (four  = 4.0d0,  six   = 6.0d0)
 
      dimension         ek0(6), rw(6,6), tw(6,6,6), ferror(2)
      dimension         data(2,0:maxmul), field(2,0:maxmul)
      dimension         o1(6), e1(6,6), o2(6), e2(6,6)
      logical bvflag
      integer bvpos
      parameter         (bvpos = 24)
      equivalence       (x1, o1(1)), (px1, o1(2))
      equivalence       (y1, o1(3)), (py1, o1(4))
      equivalence       (t1, o1(3)), (pt1, o1(4))
      equivalence       (x2, o2(1)), (px2, o2(2))
      equivalence       (y2, o2(3)), (py2, o2(4))
      equivalence       (t2, o2(3)), (pt2, o2(4))
 
      bvflag = .false.
*---- Switch on element type.
      isp = iq(lcelm+mbsp)
      go to (500,  20,  30, 500,  50,  60,  70,  80, 500, 100,
     +       500, 500, 500, 140, 150, 160, 500, 500, 500, 500,
     +       500, 500, 500, 500, 500, 260, 500, 500, 500, 500,
     +       500, 500, 500, 500, 500, 500, 500, 500, 500, 500), isp
      go to 500
 
*---- Dipole.
   20 continue
   30 continue
 
*---- Prepare data.
        call ucopy(q(lcelm+melen), el, mwflt)
        if (el .eq. zero) go to 500
        call ucopy(q(lcelm+meangb), an,   mwflt)
        call ucopy(q(lcelm+metltb), tilt, mwflt)
        call ucopy(q(lcelm+mek1b), sk1, mwflt)
        call ucopy(q(lcelm+mee1b), edg1, mwflt)
        call ucopy(q(lcelm+mee2b), edg2, mwflt)
        call ucopy(q(lcelm+mek2b), sk2, mwflt)
        call ucopy(q(lcelm+megapb), hgap, mwflt)
        call ucopy(q(lcelm+meintb), fint, mwflt)
        call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
        call ucopy(q(lcelm+meintbx), fintx, mwflt)
       sks = 0.0
      go to 35
 
*---- General bend (dipole, quadrupole, and skew quadrupole).
  260 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        if (el .eq. zero) go to 500
        call ucopy(q(lcelm+meangg), an,   mwflt)
        call ucopy(q(lcelm+metltg), tilt, mwflt)
        call ucopy(q(lcelm+mek1g), sk1, mwflt)
        call ucopy(q(lcelm+mee1g), edg1, mwflt)
        call ucopy(q(lcelm+mee2g), edg2, mwflt)
        call ucopy(q(lcelm+meksg), sks, mwflt)
        call ucopy(q(lcelm+megapg), hgap, mwflt)
        call ucopy(q(lcelm+meintg), fint, mwflt)
        call ucopy(q(lcelm+meintgx), fintx, mwflt)
        call ucopy(q(lcelm+meintgx+3*mcsiz), bvflag, 1)
        sk2 = 0.0
 
*---- Edge focusing angles.
   35 continue
*--- HG000915 use bv flag to possibly invert angle
        if (bvflag) an = beambv * an
*--- apply inversion and scaling
        sk1 = sk1 * elmfact(1)
        sks = sks * elmfact(1)
        sk2 = sk2 * elmfact(2)
        if (iq(lcelm+mbsp) .eq. 2) then
*--- HG001026: arc length to rectangular bend
          an2  = an / two
          if (an2 .ne. 0.d0 .and. rbarc)  el = el * an2 / sin(an2)
          edg1 = edg1 + an2
          edg2 = edg2 + an2
        endif
        h = an / el
 
*---- Refer orbit and eigenvectors to magnet midplane.
        ct = cos(tilt)
        st = sin(tilt)
        o1(1) =   ct * orb1(1) + st * orb1(3)
        o1(2) =   ct * orb1(2) + st * orb1(4)
        o1(3) = - st * orb1(1) + ct * orb1(3)
        o1(4) = - st * orb1(2) + ct * orb1(4)
        o1(5) = orb1(5)
        o1(6) = orb1(6)
        o2(1) =   ct * orb2(1) + st * orb2(3)
        o2(2) =   ct * orb2(2) + st * orb2(4)
        o2(3) = - st * orb2(1) + ct * orb2(3)
        o2(4) = - st * orb2(2) + ct * orb2(4)
        o2(5) = orb2(5)
        o2(6) = orb2(6)
        do 10 i = 1, 6
          e1(1,i) =   ct * em1(1,i) + st * em1(3,i)
          e1(2,i) =   ct * em1(2,i) + st * em1(4,i)
          e1(3,i) = - st * em1(1,i) + ct * em1(3,i)
          e1(4,i) = - st * em1(2,i) + ct * em1(4,i)
          e1(5,i) = em1(5,i)
          e1(6,i) = em1(6,i)
          e2(1,i) =   ct * em2(1,i) + st * em2(3,i)
          e2(2,i) =   ct * em2(2,i) + st * em2(4,i)
          e2(3,i) = - st * em2(1,i) + ct * em2(3,i)
          e2(4,i) = - st * em2(2,i) + ct * em2(4,i)
          e2(5,i) = em2(5,i)
          e2(6,i) = em2(6,i)
   10   continue
 
*---- Move through orbit through fringing field;
*     Requested components of eigenvectors are not affected.
        corr = (h + h) * hgap * fint
        call tmfrng(.false.,h,sk1,edg1,zero,+one,corr,ek0,rw,tw)
        call m66byv(rw,o1,o1)
*---- Tor: use FINTX if set
        if (fintx .gt. 0) then
          corr = (h + h) * hgap * fintx
        else
          corr = (h + h) * hgap * fint
        endif
        call tmfrng(.false.,h,sk1,edg2,zero,-one,corr,ek0,rw,tw)
        call m66inv(rw,rw)
        call m66byv(rw,o2,o2)
 
*---- Local curvature and its derivatives,
*     Coefficients for damping matrix.
        hx = sk1*x1 + sks*y1 + h + half*sk2 * (x1**2 - y1**2)
        hy = sks*x1 - sk1*y1 - sk2*x1*y1
        hxx = sk1 + sk2*x1
        hxy = sks - sk2*y1
        hyx = hxy
        hyy = - hxx
        h1 = sqrt(hx**2 + hy**2)
        hcb1 = h1**3
        hcbs1 = three*h1 *
     +    (hx * (hxx*px1 + hxy*py1) + hy * (hxy*px1 + hyy*py1))
 
        tedg1  = tan(edg1)
        fact1  = (one + h*x1) * (one - tedg1*x1)
        fact1x = h - tedg1 - 2.0*h*tedg1*x1
        rfac1  = cg*el*h1**2*fact1
        rfac1x = cg*el * (two*(hx*hxx+hy*hyx)*fact1 + h1**2*fact1x)
        rfac1y = cg*el *  two*(hx*hxy+hy*hyy)*fact1
 
        hx = sk1*x2 + sks*y2 + h + half*sk2 * (x2**2 - y2**2)
        hy = sks*x2 - sk1*y2 - sk2*x2*y2
        hxx = sk1 + sk2*x2
        hxy = sks - sk2*y2
        hyx = hxy
        hyy = - hxx
        h2 = sqrt(hx**2 + hy**2)
        hcb2 = h2**3
        hcbs2 = three*h2 *
     +    (hx * (hxx*px2 + hxy*py2) + hy * (hxy*px2 + hyy*py2))
 
        tedg2  = tan(edg2)
        fact2  = (one + h*x2) * (one - tedg2*x2)
        fact2x = h - tedg2 - 2.0*h*tedg2*x2
        rfac2  = cg*el*h2**2*fact2
        rfac2x = cg*el * (two*(hx*hxx+hy*hyx)*fact2 + h2**2*fact2x)
        rfac2y = cg*el *  two*(hx*hxy+hy*hyy)*fact2
 
*---- Cubic integration over h**3 * E(i,5) * conjg(E(i,5)).
        bi2gi2 = one / (betas * gammas)**2
        hbi = h / betas
        do 40 i = 1, 3
          ir = 2 * i - 1
          ii = 2 * i
 
*---- E(i,5) * conjg(E(i,5)) and its derivative w.r.t. S.
          e5sq1 = e1(5,ir)**2 + e1(5,ii)**2
          e5sq2 = e2(5,ir)**2 + e2(5,ii)**2
          e5sqs1 = two * (e1(5,ir) * (bi2gi2*e1(6,ir) - hbi*e1(1,ir))
     +                  + e1(5,ii) * (bi2gi2*e1(6,ii) - hbi*e1(1,ii)))
          e5sqs2 = two * (e2(5,ir) * (bi2gi2*e2(6,ir) - hbi*e2(1,ir))
     +                  + e2(5,ii) * (bi2gi2*e2(6,ii) - hbi*e2(1,ii)))
 
*---- Integrand and its derivative w.r.t. S.
          f1 = hcb1 * e5sq1
          f2 = hcb2 * e5sq2
          f1s = hcbs1 * e5sq1 + hcb1 * e5sqs1
          f2s = hcbs2 * e5sq2 + hcb2 * e5sqs2
 
*---- Actual integration.
          sum(i) = sum(i) + half * el * (f1 + f2) -
     +             el**2 * (f2s - f1s) / twelve
   40   continue
      go to 77
 
*---- Quadrupole.
   50 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek1q), sk1, mwflt)
        str  = sk1
        n    = 1
        twon = two
        go to 75
 
*---- Sextupole.
   60 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek1q), sk2, mwflt)
        str  = sk2 / two
        n    = 2
        twon = four
        go to 75
 
*---- Octupole.
   70 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek1q), sk3, mwflt)
        str  = sk3 / six
        n    = 3
        twon = six
 
*---- Common to all pure multipoles.
   75   if (el .eq. zero) go to 500
        call ucopy(orb1, o1, 6*mwflt)
        call ucopy(orb2, o2, 6*mwflt)
        call ucopy(em1, e1, 36*mwflt)
        call ucopy(em2, e2, 36*mwflt)
 
*---- Local curvature.
        r1sq = orb1(1)**2 + orb1(3)**2
        r2sq = orb2(1)**2 + orb2(3)**2
        h1 = abs(str) * sqrt(r1sq)**n
        h2 = abs(str) * sqrt(r2sq)**n
        rfac = cg * str**2 * el
        rfac1 = rfac * r1sq**n
        rfac2 = rfac * r2sq**n
        rfac1x = twon * rfac * r1sq**(n-1) * x1
        rfac2x = twon * rfac * r1sq**(n-1) * x2
        rfac1y = twon * rfac * r1sq**(n-1) * y1
        rfac2y = twon * rfac * r1sq**(n-1) * y2
 
*---- Trapezoidal integration over h**3 * E(k,5) * conjg(E(k,5)).
        fh1 = half * el * h1**3
        fh2 = half * el * h2**3
        sum(1) = sum(1) + fh1 * (e1(5,1)**2 + e1(5,2)**2)
     +                  + fh2 * (e2(5,1)**2 + e2(5,2)**2)
        sum(2) = sum(2) + fh1 * (e1(5,3)**2 + e1(5,4)**2)
     +                  + fh2 * (e2(5,3)**2 + e2(5,4)**2)
        sum(3) = sum(3) + fh1 * (e1(5,5)**2 + e1(5,6)**2)
     +                  + fh2 * (e2(5,5)**2 + e2(5,6)**2)
 
*---- Damping matrices.
*     Code common to bending magnet and pure multipoles.
   77   call m66one(rw)
        rw(2,1) =     - rfac1x * (one + pt1) * px1
        rw(2,2) = one - rfac1  * (one + pt1)
        rw(2,3) =     - rfac1y * (one + pt1) * px1
        rw(2,6) =     - rfac1                * px1
        rw(4,1) =     - rfac1x * (one + pt1) * py1
        rw(4,3) =     - rfac1y * (one + pt1) * py1
        rw(4,4) = one - rfac1  * (one + pt1)
        rw(4,6) =     - rfac1                * py1
        rw(6,1) =     - rfac1x * (one + pt1)**2
        rw(6,3) =     - rfac1y * (one + pt1)**2
        rw(6,6) = one - two * rfac1 * (one + pt1)
        call m66mpy(re, rw, re)
 
        call m66one(rw)
        rw(2,1) =     - rfac2x * (one + pt2) * px2
        rw(2,2) = one - rfac2  * (one + pt2)
        rw(2,3) =     - rfac2y * (one + pt2) * px2
        rw(2,6) =     - rfac2                * px2
        rw(4,1) =     - rfac2x * (one + pt2) * py2
        rw(4,3) =     - rfac2y * (one + pt2) * py2
        rw(4,4) = one - rfac2  * (one + pt2)
        rw(4,6) =     - rfac2                * py2
        rw(6,1) =     - rfac2x * (one + pt2)**2
        rw(6,3) =     - rfac2y * (one + pt2)**2
        rw(6,6) = one - two * rfac2 * (one + pt2)
        call m66mpy(rw, re, re)
      go to 500
 
*---- Thin multipoles, EL is the fictitious length for radiation.
   80 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        if (el .ne. zero) then
 
*---- Multipole components.
          nd = 2 * mwflt * (maxmul + 1)
          call uzero(data, 1, nd)
          na = min(2 * maxmul + 4, iq(lcelm+mbat))
          call utgflt(lcelm, 3, na, data)
          call utglog(lcelm, bvpos, bvpos, bvflag)
 
          call uzero(field, 1, nd)
          if (lcfld .ne. 0) then
            ne = min(iq(lcfld-1), nd)
            call ucopy(q(lcfld+1), field, ne)
          endif
 
          nord = 0
          do 81 iord = 0, maxmul
            val =   data(1,iord)
            ang = - data(2,iord) * float(iord+1)
            field(1,iord) = val * cos(ang) + field(1,iord)
            field(2,iord) = val * sin(ang) + field(2,iord)
            if (field(1,iord).ne.zero .or. field(2,iord).ne.zero) then
              nord = iord
            endif
   81     continue
 
*--- HG000915 use bv flag to possibly invert angle
          if (bvflag) then
            field(1,0) = beambv * field(1,0)
            field(2,0) = beambv * field(2,0)
          endif
*--- apply inversion and scaling
          do i = 1, nord
            do j = 1, 2
              field(j,i) = field(j,i) * elmfact(i)
            enddo
          enddo
 
*---- Track orbit.
          x = orb1(1)
          y = orb1(3)
 
*---- Multipole kick.
          dr = zero
          di = zero
          do 82 iord = nord, 0, -1
            drt = (dr * x - di * y) / float(iord+1) + field(1,iord)
            di  = (dr * y + di * x) / float(iord+1) + field(2,iord)
            dr  = drt
   82     continue
 
*---- H is local "curvature" due to multipole kick.
          h  = sqrt(dr**2 + di**2) / el
          hcb = half * el * h**3
          sum(1)  = sum(1) + hcb *
     +      (em1(5,1)**2 + em1(5,2)**2 + em2(5,1)**2 + em2(5,2)**2)
          sum(2)  = sum(2) + hcb *
     +      (em1(5,3)**2 + em1(5,4)**2 + em2(5,3)**2 + em2(5,4)**2)
          sum(3)  = sum(3) + hcb *
     +      (em1(5,5)**2 + em1(5,6)**2 + em2(5,5)**2 + em2(5,6)**2)
 
*---- Damping matrix, is the same at both ends.
          rfac  = cg * (dr**2 + di**2) / el
          rfacx = cg * (- dr * re(2,1) + di * re(4,1)) / el
          rfacy = cg * (- dr * re(2,3) + di * re(4,3)) / el
 
          call m66one(rw)
          rw(2,1) = - rfacx * (one + orb1(6)) * orb1(2)
          rw(2,2) = one - rfac * (one + orb1(6))
          rw(2,3) = - rfacy * (one + orb1(6)) * orb1(2)
          rw(2,6) = - rfac * orb1(2)
          rw(4,1) = - rfacx * (one + orb1(6)) * orb1(4)
          rw(4,3) = - rfacy * (one + orb1(6)) * orb1(4)
          rw(4,4) = one - rfac * (one + orb1(6))
          rw(4,6) = - rfac * orb1(4)
          rw(6,1) = - rfacx * (one + orb1(6))
          rw(6,3) = - rfacy * (one + orb1(6))
          rw(6,6) = one - two * rfac * (one + orb1(6))
          call m66mpy(re, rw, re)
          call m66mpy(rw, re, re)
        endif
      go to 500
 
*---- RF cavities.
  100 continue
        call ucopy(q(lcelm+mevltc), rfv, mwflt)
        call ucopy(q(lcelm+mefrqc), rff, mwflt)
        call ucopy(q(lcelm+melagc), rfl, mwflt)
        rfvlt = ten3m * rfv
        rffrq = rff * (ten6p * two * pi / clight)
        rflag = two * pi * rfl
        time = half * (orb1(5) + orb2(5))
        sumu0 = sumu0 + rfvlt * sin(rflag - rffrq * time)
      go to 500
 
*---- Orbit correctors.
  140 continue
  150 continue
  160 continue
        call ucopy(q(lcelm+melen), el, mwflt)
 
*---- Original setting.
        if (el .ne. zero) then
          isp = iq(lcelm+mbsp)
          if (isp .eq. 14) then
            call ucopy(q(lcelm+mekick), xkick, mwflt)
            ykick = zero
          else if (isp .eq. 16) then
            xkick = zero
            call ucopy(q(lcelm+mekick), ykick, mwflt)
          else
            call ucopy(q(lcelm+mekick), xkick, mwflt)
            call ucopy(q(lcelm+mekick+mcsiz), ykick, mwflt)
          endif
 
*---- Correction from C.O. correction algorithm.
          if (dokick  .and.  lccom .ne. 0) then
            call ucopy(q(lccom+1), xcm, 2*mwflt)
          else
            xcm = zero
            ycm = zero
          endif
 
*---- Field errors.
          if (lcfld .ne. 0) then
            call ucopy(q(lcfld+1), ferror, 2*mwflt)
          else
            ferror(1) = zero
            ferror(2) = zero
          endif
 
*---- Local curvature.
          hx = abs(xcm + xkick + ferror(1)) / el
          hy = abs(ycm + ykick + ferror(2)) / el
          rfac = cg * (hx**2 + hx**2) * el
 
*---- Trapezoidal integration over h**3*E(k,5)*E*(k,5).
          fh = half * el * sqrt(hx**2 + hy**2)**3
          sum(1) = sum(1) + fh *
     +      (em1(5,1)**2 + em1(5,2)**2 + em2(5,1)**2 + em2(5,2)**2)
          sum(2) = sum(2) + fh *
     +      (em1(5,3)**2 + em1(5,4)**2 + em2(5,3)**2 + em2(5,4)**2)
          sum(3) = sum(3) + fh *
     +      (em1(5,5)**2 + em1(5,6)**2 + em2(5,5)**2 + em2(5,6)**2)
 
*---- Damping matrices.
          call m66one(rw)
          rw(2,2) = one - rfac * (one + orb1(6))
          rw(2,6) = - rfac * orb1(2)
          rw(4,4) = one - rfac * (one + orb1(6))
          rw(4,6) = - rfac * orb1(4)
          rw(6,6) = one - two * rfac * (one + orb1(6))
          call m66mpy(re, rw, re)
 
          call m66one(rw)
          rw(2,2) = one - rfac * (one + orb2(6))
          rw(2,6) = - rfac * orb2(2)
          rw(4,4) = one - rfac * (one + orb2(6))
          rw(4,6) = - rfac * orb2(4)
          rw(6,6) = one - two * rfac * (one + orb2(6))
          call m66mpy(rw, re, re)
        endif
      go to 500
 
  500 continue
 
 9999 end
