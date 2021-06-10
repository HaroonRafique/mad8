      subroutine twopsv(iflag, kflag, ctab, fract)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build OPTICS table.                                                *
* Input:                                                               *
*   IFLAG     (integer) Operation desired.                             *
*                       1: Create new table.                           *
*                       2: Save one line.                              *
*                       4: Close table.                                *
*   KFLAG     (integer) Type of output requested:                      *
*                       1: No extrapolation, no element data.          *
*                       2: No extrapolation, but element data.         *
*                       3: Extrapolation and element data.             *
*   CTAB(*)   (char)    For IFLAG = 1: Columns selected.               *
*                       For IFLAG = 2: Character info to be output.    *
*   FRACT     (real)    Fraction of element for interpolation.         *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   LCAV handling at statement number 270 HOWEVER attributes are not   *
*   correct; modified optics tracking similar to that in TWBTTK        *
* Modified: 11-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Included FINTX attribute in RBEND, SBEND, and GBEND (NOTE: FINTX   *
*   is assumed to have same value as FINT if it is not set or is       *
*   negative as is the default in the dictionary file) (NOTE: FINTX    *
*   parameter does NOT work properly here!!!); added warning about     *
*   incorrect operation with FINTX                                     *
* Modified: 25-MAR-1999, M. Woodley (SLAC)                             *
*   Fix pointer to FINTX parameter                                     *
* Modified: 10-SEP-1999, M. Woodley (SLAC)                             *
*   Use the determinant of the longitudinal 2x2 part of the R-matrix   *
*   instead of R(6,6) for the energy scaling.                          *
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
      integer i,idata,iflag,ileng,ipos,isp,j,jbias,jform,jtab,k,kflag,
     +ktab,ltemp,maxcol,mnorm,mskew,mtwrow,nb,nc,nd,ne,nr,ns,ntab
      double precision ampl,angl,aux,auxp,ax1,ax2,ay1,ay2,bx1,bx2,by1,
     +by2,corr,ddpx,ddpy,ddx,ddy,delt,dh,dl,dmxx,dmyy,dpx,dpy,dx,dy,e1,
     +e2,efld,ek,el,elag,elrd,ferror,fint,fract,freq,h,h1,h2,harm,hgap,
     +hkik,one,phxx,phyy,proxim,pxco,pyco,radl,re,rep,rw,sk0l,sk1,sk1l,
     +sk2,sk2l,sk3,sk3l,sk4l,sk5l,sk6l,sk7l,sk8l,sk9l,sks,sksl,spos,
     +ss0l,ss1l,ss2l,ss3l,ss4l,ss5l,ss6l,ss7l,ss8l,ss9l,t,t2,ta,table,
     +tb,te,temp,tg,tilt,tw,utwopi,vkik,volt,wxx,wyy,x,xalf,xbet,xco,
     +xkick,xmu,y,yalf,ybet,yco,ykick,ymu,zero,two
      character*(mcnam) ctab(*)
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer mtbact,mtbbky,mtbbuf,mtbcfm,mtbcnm,mtbcol,mtbcps,mtbdsc,
     +mtbf1,mtbf2,mtbfst,mtblst,mtbmod,mtbnam,mtbrow,mtbseg,mtbsiz,
     +mtbsky,mtbwid
 
*---- Parameters for table manager bank structure.
      parameter         (mtbact = 1, mtbbuf = 2, mtbmod = 1)
      parameter         (mtbf1  = 1,
     +                   mtbseg = 2, mtbrow = 3, mtbcol = 4, mtbwid = 5,
     +                   mtbf2  = 6,
     +                   mtbnam = 7, mtbsiz = mtbnam + mwnam - 1)
      parameter         (mtbsky = 2, mtbbky = 3, mtbcnm = 4,
     +                   mtbcfm = 5, mtbcps = 6, mtbdsc = 7,
     +                   mtbfst = 8, mtblst = 9)
      integer ndelta
 
*---- Common for Twiss module.
      common /twchar/   funnam, optnam, sumnam, betnam
      common /twdata/   ndelta, chrom, couple
      save              /twdata/, /twchar/
      character*(mcnam) funnam, optnam, sumnam, betnam
      logical           chrom, couple
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
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
      double precision detl, f, fintx
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0d0 / (2.0d0 * pi))
      parameter         (nd = 20 * mwflt, mtwrow = 500)
      parameter         (zero = 0.0d0, one = 1.0d0, two = 2.d0)
      parameter         (maxcol = 70)
 
      logical           fmap
      dimension         ek(6), re(6,6), rep(6,6), rw(6,6), temp(6)
      double precision  fre(6,6), frep(6,6)
      double precision an2
      dimension         aux(6), auxp(6)
      dimension         te(6,6,6), tw(6,6,6)
      dimension         ferror(2,0:9), table(maxcol)
      character*(mcnam) madnam(maxcol), word
 
      integer           itab(50), iform(50)
      save              ipos, itab, iform, ntab
      integer bvpos
      parameter         (bvpos = 24)
      logical bvflag
      equivalence
     +    (xbet, table( 5)), (xalf, table( 6)), (xmu,  table( 7)),
     +    (ybet, table( 8)), (yalf, table( 9)), (ymu,  table(10)),
     +    (xco,  table(11)), (pxco, table(12)),
     +    (yco,  table(13)), (pyco, table(14)),
     +    (dx,   table(15)), (dpx,  table(16)),
     +    (dy,   table(17)), (dpy,  table(18)),
     +    (wxx,  table(19)), (phxx, table(20)), (dmxx, table(21)),
     +    (wyy,  table(22)), (phyy, table(23)), (dmyy, table(24)),
     +    (ddx,  table(25)), (ddpx, table(26)),
     +    (ddy,  table(27)), (ddpy, table(28)), (elrd, table(29))
      equivalence
     +    (delt, table(31)), (spos, table(32)), (radl, table(33)),
     +    (el,   table(34)), (volt, table(35)), (elag, table(36)),
     +    (freq, table(37)), (harm, table(38)), (tilt, table(39)),
     +    (sksl, table(40)), (hkik, table(41)), (vkik, table(42)),
     +    (e1,   table(43)), (e2,   table(44)), (h1,   table(45)),
     +    (h2,   table(46)), (efld, table(47))
      parameter (mnorm = 51, mskew = 61)
      equivalence
     +    (sk0l, table(51)), (sk1l, table(52)), (sk2l, table(53)),
     +    (sk3l, table(54)), (sk4l, table(55)), (sk5l, table(56)),
     +    (sk6l, table(57)), (sk7l, table(58)), (sk8l, table(59)),
     +    (sk9l, table(60)),
     +    (ss0l, table(61)), (ss1l, table(62)), (ss2l, table(63)),
     +    (ss3l, table(64)), (ss4l, table(65)), (ss5l, table(66)),
     +    (ss6l, table(67)), (ss7l, table(68)), (ss8l, table(69)),
     +    (ss9l, table(70))
 
      data (madnam(i), i = 1, 30)
     +    / 'NAME',    'KEYWORD', 'CLASS',   'TYPE',
     +      'BETX',    'ALFX',    'MUX',
     +      'BETY',    'ALFY',    'MUY',
     +      'X',       'PX',      'Y',       'PY',
     +      'DX',      'DPX',     'DY',      'DPY',
     +      'WX',      'PHIX',    'DMUX',
     +      'WY',      'PHIY',    'DMUY',
     +      'DDX',     'DDPX',    'DDY',     'DDPY',
     +      'LRAD',    ' ' /
      data (madnam(i), i = 31, 50)
     +    / 'DP',      'S',       'RADLOSS', 'L',
     +      'VOLT',    'LAG',     'FREQ',    'HARMON',
     +      'TILT',    'KS',      'HKICK',   'VKICK',
     +      'E1',      'E2',      'H1',      'H2',      'EFIELD',
     +      ' ',       ' ',       ' ' /
      data (madnam(i), i = 51, 70)
     +    / 'K0L',     'K1L',     'K2L',     'K3L',     'K4L',
     +      'K5L',     'K6L',     'K7L',     'K8L',     'K9L',
     +      'KS0L',    'KS1L',    'KS2L',    'KS3L',    'KS4L',
     +      'KS5L',    'KS6L',    'KS7L',    'KS8L',    'KS9L' /
 
      proxim(x, y) = (x * utwopi) + anint((y - x) * utwopi)
 
      bvflag = .false.
*==== Create table.
      if (iflag .eq. 1) then
        ntab = 0
        do 10 jtab = 1, 50
          if (ctab(jtab) .ne. ' ') then
            call utleng(ctab(jtab), ileng)
            call utlook(ctab(jtab)(1:ileng), madnam, maxcol, ktab)
            if (ktab .eq. 0) then
              msg(1) = 'Unknown column name "' // ctab(jtab)(1:ileng)
     +        // '" ignored.'
              call aawarn('TWOPSV', 1, msg)
            else
              ntab = ntab + 1
              ctab(ntab) = madnam(ktab)
              itab(ntab) = ktab
              iform(ntab) = 3
              if (double) iform(ntab) = 4
              if (ktab .le. 4) iform(ntab) = 5
            endif
          endif
   10   continue
 
*---- Create the table.
        ns = ndelta
        nr = mtwrow
        nc = ntab
        nb = 1
        call tbcrea(optnam, ns, nr, nc, ctab, iform, nb, ltwopt)
        call tbpdsc(ltwopt, 'TYPE', 5, 0, zero, 'OPTICS')
        ipos = 0
*==== Save one line.
      else if (ltwopt .ne. 0) then
        if (iflag .eq. 2) then
 
*---- Set up unit map and clear table.
          dl = 0.0
          call uzero(ek, 1, 6*mwflt)
          call m66one(re)
          call uzero(te, 1, 216*mwflt)
          call uzero(table, 1, maxcol*mwflt)
 
*---- Extract field errors.
          call uzero(ferror, 1, nd)
          if (lcfld .ne. 0) then
            ne = min(iq(lcfld-1), nd)
            call ucopy(q(lcfld+1), ferror, ne)
          endif
 
*---- Skip extrapolation for beam line entry or exit.
          if (kflag .ne. 1) then
 
*---- Get length, as well as extrapolation length.
            call ucopy(q(lcelm+melen), el, mwflt)
            dl = fract * el
            isp = iq(lcelm+mbsp)
 
*---- Select element type.
            go to (110,120,130,140,150,160,170,180,190,200,
     +             210,220,230,240,250,260,270,280,290,300,
     +             310,320,330,340,350,360,370,380,390,400,
     +             410,410,410,410,410,410,410,410,410,410),isp
 
*---- Drift, Monitor, Collimator, Beam instrument.
  110       continue
 
*---- LCAV cavity.
  270       continue
              call ucopy(q(lcelm+mevltc), volt, mwflt)
              call ucopy(q(lcelm+mefrqc), freq, mwflt)
              call ucopy(q(lcelm+melagc), elag, mwflt)
              harm = iq(lcelm+mehrmc)
              if (kflag .eq. 3) then
                call tmdrf(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
  280       continue
  290       continue
  300       continue
  310       continue
  340       continue
            if (kflag .eq. 3) then
              call tmdrf(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
            endif
            go to 500
 
*---- Bending magnets, RBEND or SBEND.
  120       continue
  130       continue
              call ucopy(q(lcelm+meangb), sk0l, mwflt)
              call ucopy(q(lcelm+mek1b), sk1, mwflt)
              call ucopy(q(lcelm+mek2b), sk2, mwflt)
              call ucopy(q(lcelm+mek3b), sk3, mwflt)
              call ucopy(q(lcelm+mee1b), e1, mwflt)
              call ucopy(q(lcelm+mee2b), e2, mwflt)
              call ucopy(q(lcelm+metltb), tilt, mwflt)
              call ucopy(q(lcelm+meh1b), h1, mwflt)
              call ucopy(q(lcelm+meh2b), h2, mwflt)
              call ucopy(q(lcelm+megapb), hgap, mwflt)
              call ucopy(q(lcelm+meintb), fint, mwflt)
              call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
              call ucopy(q(lcelm+meintbx), fintx, mwflt)
              sk0l = sk0l + ferror(1,0)
              sk1l = elmfact(1)*(sk1*el + ferror(1,1))
              sk2l = elmfact(2)*(sk2*el + ferror(1,2))
              sk3l = elmfact(3)*(sk3*el + ferror(1,3))
              sksl = 0.0
*--- HG000915 use bv flag to possibly invert angle
              if (bvflag) sk0l = beambv * sk0l
            go to 365
 
*---- General bend (dipole, quadrupole, and skew quadrupole).
  360       continue
              call ucopy(q(lcelm+meangg), sk0l, mwflt)
              call ucopy(q(lcelm+mek1g), sk1, mwflt)
              call ucopy(q(lcelm+meksg), sks, mwflt)
              call ucopy(q(lcelm+mee1g), e1, mwflt)
              call ucopy(q(lcelm+mee2g), e2, mwflt)
              call ucopy(q(lcelm+metltg), tilt, mwflt)
              call ucopy(q(lcelm+meh1g), h1, mwflt)
              call ucopy(q(lcelm+meh2g), h2, mwflt)
              call ucopy(q(lcelm+megapg), hgap, mwflt)
              call ucopy(q(lcelm+meintg), fint, mwflt)
              call ucopy(q(lcelm+meintgx), fintx, mwflt)
              call ucopy(q(lcelm+meintgx+3*mcsiz), bvflag, 1)
              sk0l = sk0l + ferror(1,0)
              sk1l = elmfact(1) * (sk1*el + ferror(1,1))
              sk2l = 0.0
              sk3l = 0.0
              ss1l = elmfact(1) * (sks*el + ferror(2,1))
*--- HG000915 use bv flag to possibly invert angle
              if (bvflag) sk0l = beambv * sk0l
 
*---- Extrapolate to desired position.
  365       continue
              if (kflag .eq. 3  .and.  el .ne. 0.0) then
                if (isp .eq. 2) then
*--- HG001026: arc length to rectangular bend
                  an2 = sk0l / 2.d0
                  if (an2 .ne. 0.d0 .and. rbarc)
     +            el = el * an2 / sin(an2)
                  e1 = e1 + an2
                endif
                h = sk0l / el
                dh = ferror(1,0) / el
                sk1 = sk1l / el
                sk2 = sk2l / el
                sk3 = sk3l / el
                sks = sksl / el
 
                corr = (h + h) * hgap * fint
*---- Tor: warn user that TWOPSV and TWOPGO do not handle FINTX
*     correctly.
                if (fintx .gt. zero) then
                  msg(1) = 'OPTICS command does not handle FINTX '
     +                     //'attribute correctly'
                  call aawarn ('TWOPSV', 1, msg)
                endif
                call tmfrng(.true.,h,sk1,e1,h1,one,corr,ek,re,te)
                if (isp .ne. 26) then
                  call tmsect(.true.,dl,h,dh,sk1,sk2,ek,rw,tw)
                else
                  call tmgsec(.true.,dl,h,dh,sk1,sks,ek,rw,tw)
                endif
                call tmcat(.true.,rw,tw,re,te,re,te)
                if (tilt .ne. 0.0) call tmtilt(.true.,tilt,ek,re,te)
              endif
              if (el .ne. 0.0) then
                radl = - 2.0 * arad * charge * gammas**3 * sk0l**2 /
     +          (3.0 * el)
              endif
            go to 500
 
*---- Quadrupole.
  150       continue
              call ucopy(q(lcelm+mek1q), sk1, mwflt)
              call ucopy(q(lcelm+metltq), tilt, mwflt)
              sk1l = elkfact(5) * (sk1*el + ferror(1,1))
              if (kflag .eq. 3) then
                call tmquad(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
 
*---- Sextupole.
  160       continue
              call ucopy(q(lcelm+mek2s), sk2, mwflt)
              call ucopy(q(lcelm+metlts), tilt, mwflt)
              sk2l = elkfact(6) * (sk2*el + ferror(1,2))
 
              if (kflag .eq. 3) then
                call tmsext(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
 
*---- Octupole.
  170       continue
              call ucopy(q(lcelm+mek3o), sk3, mwflt)
              call ucopy(q(lcelm+metlts), tilt, mwflt)
              sk3l = elkfact(7) * (sk3*el + ferror(1,3))
 
              if (kflag .eq. 3) then
                call tmoct(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
 
*---- Multipole.
  180       continue
              idata = meklm
              call utglog(lcelm, bvpos, bvpos, bvflag)
              call ucopy(q(lcelm+idata), ampl, mwflt)
              idata = idata + mcsiz
              call ucopy(q(lcelm+idata), angl, mwflt)
              idata = idata + mcsiz
              table(mnorm) = ferror(1,0) + ampl * cos(angl)
              table(mskew) = ferror(2,0) + ampl * sin(angl)
              if (bvflag) then
                table(mnorm) = beambv * table(mnorm)
                table(mskew) = beambv * table(mskew)
              endif
              do i = 1, 9
                call ucopy(q(lcelm+idata), ampl, mwflt)
                idata = idata + mcsiz
                call ucopy(q(lcelm+idata), angl, mwflt)
                idata = idata + mcsiz
                angl = real(i+1) * angl
                table(i+mnorm) = ferror(1,i) + ampl * cos(angl)
                table(i+mnorm) = elmfact(i) * table(i+mnorm)
                table(i+mskew) = ferror(2,i) + ampl * sin(angl)
                table(i+mskew) = elmfact(i) * table(i+mskew)
              enddo
 
*---- Radiation loss.
              if (el .ne. 0.0) then
                radl = - 2.0 * arad * charge * gammas**3 *
     +            (sk0l**2 + ss0l**2) / (3.0 * el)
              endif
              el = 0.0
              call ucopy(q(lcelm+melen), elrd, mwflt)
            go to 500
 
*---- Solenoid.
  190       continue
              call ucopy(q(lcelm+mekss), sks, mwflt)
              sksl = sks * el
              if (kflag .eq. 3) then
                call tmsol(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
 
*---- RF cavity.
  200       continue
              call ucopy(q(lcelm+mevltc), volt, mwflt)
              call ucopy(q(lcelm+mefrqc), freq, mwflt)
              call ucopy(q(lcelm+melagc), elag, mwflt)
              harm = iq(lcelm+mehrmc)
              if (kflag .eq. 3) then
                call tmdrf(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
 
*---- Electrostatic separator.
  210       continue
              call ucopy(q(lcelm+meflde), efld, mwflt)
              call ucopy(q(lcelm+metlte), tilt, mwflt)
              if (kflag .eq. 3) then
                call tmsep(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
 
*---- No extrapolation for the following:
*       Arbitrary matrix, rotations, markers, beam-beam, lump, reserved.
  140       continue
  220       continue
  230       continue
  320       continue
  330       continue
  350       continue
  370       continue
  380       continue
  390       continue
  400       continue
  410       continue
            el = 0.0
            go to 500
 
*---- Corrector.
  240       continue
  250       continue
  260       continue
 
*---- Original setting.
              if (isp .eq. 14) then
                call ucopy(q(lcelm+mekick), xkick, mwflt)
                call ucopy(q(lcelm+mekick+mcsiz), bvflag, 1)
                ykick = 0.0
              else if (isp .eq. 16) then
                xkick = 0.0
                call ucopy(q(lcelm+mekick), ykick, mwflt)
                call ucopy(q(lcelm+mekick+mcsiz), bvflag, 1)
              else
                call ucopy(q(lcelm+mekick), xkick, mwflt)
                call ucopy(q(lcelm+mekick+mcsiz), ykick, mwflt)
                call ucopy(q(lcelm+mekick+2*mcsiz), bvflag, 1)
              endif
 
*---- Correction from C.O. correction algorithm.
              if (lccom .ne. 0) then
                call ucopy(q(lccom+1), xcm, 2*mwflt)
              else
                xcm = 0.0
                ycm = 0.0
              endif
 
*---- Store sum of original setting and correction.
              hkik = xcm + xkick + ferror(1,0)
              vkik = ycm + ykick + ferror(2,0)
*--- HG000915 use bv flag to possibly invert angle
              if (bvflag) then
                hkik = beambv * hkik
                vkik = beambv * vkik
              endif
 
*---- Extrapolate.
              if (kflag .eq. 3) then
                call tmcorr(.true.,.false.,fract,orbit,fmap,el,ek,re,te)
              endif
            go to 500
 
  500       continue
          endif
 
*---- Track orbit to desired position.
          if (fmap) then
            call tmtrak(ek, re, te, orbit, temp)
          else
            call ucopy(orbit, temp, 6*mwflt)
          endif
 
*---- Longitudinal position and energy error.
          spos = suml + fract * el
          delt = orbit(6)
 
*---- Dispersion.
          do 530 i = 1, 6
            aux(i) = 0.0
            auxp(i) = 0.0
            do 520 k = 1, 6
              t = 0.0
              do 510 j = 1, 6
                t = t + te(i,j,k) * disp(j)
  510         continue
              aux(i) = aux(i) + re(i,k) * disp(k)
              auxp(i) = auxp(i) + t * disp(k) + re(i,k) * ddisp(k)
              rep(i,k) = 2.0 * t
  520       continue
  530     continue
          call ucopy(aux, dx, 4*mwflt)
          call ucopy(auxp, ddx, 4*mwflt)
 
*---- Tor/MDW: scale by square root of the determinant of the
*     longitudinal 2x2 part of the R-matrix
          detl = re(5,5)*re(6,6) - re(5,6)*re(6,5)
          f = one / sqrt(detl)
          call m66scl(f, re, fre)
          call m66scl(f, rep, frep)
 
*---- Horizontal lattice functions including energy scaling.
          if (stabx) then
            tb = fre(1,1)*betx - fre(1,2)*alfx
            ta = fre(2,1)*betx - fre(2,2)*alfx
            t2 = tb**2 + fre(1,2)**2
            tg = fre(1,1)*alfx - fre(1,2)*(one + alfx**2) / betx
 
*---- Linear functions.
            xalf = - (tb*ta + fre(1,2)*fre(2,2)) / betx
            xbet = t2 / betx
            xmu = (amux + atan2(fre(1,2), tb)) * utwopi
            if (re(1,2) .lt. zero) xmu = xmu + one
 
*---- Chromatic functions.
            bx1 = wx*cos(phix)
            ax1 = wx*sin(phix)
            bx2 = ((tb**2 - fre(1,2)**2)*bx1
     +          - two*tb*fre(1,2)*ax1) / t2
     +          + two*(tb*frep(1,1) - tg*frep(1,2)) / xbet
            ax2 = ((tb**2 - fre(1,2)**2)*ax1
     +            + two*tb*fre(1,2)*bx1) / t2
     +          - (tb*(frep(1,1)*xalf + frep(2,1)*xbet)
     +          - tg*(frep(1,2)*xalf + frep(2,2)*xbet)
     +          + fre(1,1)*frep(1,2) - fre(1,2)*frep(1,1)) / xbet
            wxx = sqrt(ax2**2 + bx2**2)
            if (wxx .gt. 1.0d-8) phxx = proxim(atan2(ax2, bx2), phix)
            dmxx = (dmux + fre(1,2)*(fre(1,2)*ax1 - tb*bx1) / t2
     +           + (fre(1,1)*frep(1,2) - fre(1,2)*frep(1,1)) / xbet)
     +           * utwopi
          else
            xalf = zero
            xbet = zero
            xmu = zero
            wxx = zero
            phxx = zero
            dmxx = zero
          endif
 
*---- Vertical lattice functions including energy scaling.
          if (staby) then
            tb = fre(3,3)*bety - fre(3,4)*alfy
            ta = fre(4,3)*bety - fre(4,4)*alfy
            t2 = tb**2 + fre(3,4)**2
            tg = fre(3,3)*alfy - fre(3,4)*(one + alfy**2) / bety
 
*---- Linear functions.
            yalf = - (tb*ta + fre(3,4)*fre(4,4)) / bety
            ybet = t2 / bety
            ymu = (amuy + atan2(fre(3,4), tb)) * utwopi
            if (re(3,4) .lt. zero) ymu = ymu + one
 
*---- Chromatic functions.
            by1 = wy*cos(phiy)
            ay1 = wy*sin(phiy)
            by2 = ((tb**2 - fre(3,4)**2)*by1
     +            - two*tb*fre(3,4)*ay1) / t2
     +          + two*(tb*frep(3,3) - tg*frep(3,4)) / ybet
            ay2 = ((tb**2 - fre(3,4)**2)*ay1
     +            + two*tb*fre(3,4)*by1) / t2
     +          - (tb*(frep(3,3)*yalf + frep(4,3)*ybet)
     +          - tg*(frep(3,4)*yalf + frep(4,4)*ybet)
     +          + fre(3,3)*frep(3,4) - fre(3,4)*frep(3,3)) / ybet
            wyy = sqrt(ay2**2 + by2**2)
            if (wyy .gt. 1.0d-8) phyy = proxim(atan2(ay2, by2), phiy)
            dmyy = (dmuy + fre(3,4)*(fre(3,4)*ay1 - tb*by1) / t2
     +           + (fre(3,3)*frep(3,4) - fre(3,4)*frep(3,3)) / ybet)
     +           * utwopi
          else
            yalf = zero
            ybet = zero
            ymu = zero
            wyy = zero
            phyy = zero
            dmyy = zero
          endif
*---- Closed orbit.
          xco  = 1000.0 * temp(1)
          pxco = 1000.0 * temp(2)
          yco  = 1000.0 * temp(3)
          pyco = 1000.0 * temp(4)
 
*---- Increase table size, if so required.
          ipos = ipos + 1
          if (ipos .gt. iq(ltwopt+mtbrow)) then
            ltemp = lq(ltwopt-mtbbky)
            call mzpush(0, ltemp, mtwrow, mtwrow, 'I')
            iq(ltwopt+mtbrow) = iq(ltwopt+mtbrow) + mtwrow
          endif
 
*---- Store table line.
          call tbset(ltwopt, ipos, 3, ltwbuf)
          do 610 jtab = 1, ntab
            jform = iq(lq(ltwopt-mtbcfm)+jtab)
            jbias = iq(lq(ltwopt-mtbcps)+jtab)
            if (jform .eq. 3) then
              q(ltwbuf+jbias+1) = table(itab(jtab))
            else if (jform .eq. 4) then
              call ucopy(table(itab(jtab)), q(ltwbuf+jbias+1), mwflt)
            else
              word = ctab(itab(jtab))
              call uctoh(word, iq(ltwbuf+jbias+1), mcwrd, mcnam)
            endif
  610     continue
*==== Close table.
        else if (iflag .eq. 4) then
          call tbclos(ltwopt)
        endif
      endif
 
      end
