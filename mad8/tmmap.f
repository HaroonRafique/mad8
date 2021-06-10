      subroutine tmmap(fsec, ftrk, orbit, fmap, el, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for a complete element.                              *
*   Optionally, follow orbit.                                          *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   FTRK      (logical) If true, track orbit.                          *
* Input/output:                                                        *
*   ORBIT(6)  (real)    Closed orbit.                                  *
* Output:                                                              *
*   FMAP      (logical) If true, element has a map.                    *
*   EL        (real)    Element length.                                *
*   EK(6)     (real)    Kick due to element.                           *
*   RE(6,6)   (real)    Transfer matrix.                               *
*   TE(6,6,6) (real)    Second-order terms.                            *
* Important common data:                                               *
*   LCELM     /REFER/   Current element bank.                          *
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
      integer isp,lmap
      double precision ek,el,one,orbit,parvec,re,te,zero
      logical           fsec, ftrk, fmap
      dimension         orbit(6), ek(6), re(6,6), te(6,6,6),
     +                  parvec(mbbparam)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
 
      parameter         (one = 1.0d0, zero = 0.0d0)
 
*---- Select element type.
 1000 continue
      el = 0.0
      isp = iq(lcelm+mbsp)
      go to ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +       210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     +       310, 310, 310, 310, 310, 310, 310, 310, 310, 310), isp
 
*---- Drift space, monitor, collimator, or beam instrument.
   10 continue
  170 continue
  180 continue
  190 continue
  200 continue
  210 continue
  240 continue
        call tmdrf(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Bending magnet.
   20 continue
   30 continue
        call tmbend(fsec, ftrk, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Arbitrary matrix.
   40 continue
        call tmarb(fsec, ftrk, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Quadrupole.
   50 continue
        call tmquad(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Sextupole.
   60 continue
        call tmsext(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Octupole.
   70 continue
        call tmoct(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Multipole.
   80 continue
        call tmmult(fsec, ftrk, orbit, fmap, ek, re, te)
      go to 500
 
*---- Solenoid.
   90 continue
        call tmsol(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- RF cavity.
  100 continue
        call tmrf(fsec, ftrk, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Electrostatic separator.
  110 continue
        call tmsep(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Rotation around s-axis.
  120 continue
        call tmsrot(fsec, ftrk, orbit, fmap, ek, re, te)
      go to 500
 
*---- Rotation around y-axis.
  130 continue
        call tmyrot(fsec, ftrk, orbit, fmap, ek, re, te)
      go to 500
 
*---- Correctors.
  140 continue
  150 continue
  160 continue
        call tmcorr(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- Beam-beam.
*     (Particles/bunch taken for the opposite beam).
  220 continue
      call ucopy(q(lcelm+mesigx), parvec(1), mwflt)
      if (parvec(1) .eq. 0.d0) parvec(1) = 1.d0
      call ucopy(q(lcelm+mesigy), parvec(2), mwflt)
      if (parvec(2) .eq. 0.d0) parvec(2) = 1.d0
      call ucopy(q(lcelm+mexma), parvec(3), mwflt)
      call ucopy(q(lcelm+meyma), parvec(4), mwflt)
      parvec(5) = arad
      call ucopy(q(lcelm+mechg), parvec(6), mwflt)
      parvec(6) = parvec(6) * charge * parnum
      parvec(7) = gammas
      parvec(8) = ex
      parvec(9) = ey
      call tmbb(fsec, ftrk, parvec, orbit, fmap, ek, re, te)
      go to 500
 
*---- Lump.
  230 continue
        lmap = lq(lcelm-iq(lcelm+mbat)-mbemap)
        fmap = .true.
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lmap+1), ek, 6*mwflt)
        call ucopy(q(lmap+6*mwflt+1), re, 36*mwflt)
        call ucopy(q(lmap+42*mwflt+1), te, 216*mwflt)
        call tmtrak(ek,re,te,orbit,orbit)
      go to 500
 
*---- Marker.
  250 continue
        call uzero(ek, 1, 6*mwflt)
        call m66one(re)
        if (fsec) call uzero(te, 1, 216*mwflt)
      go to 500
 
*---- General bend (dipole, quadrupole, and skew quadrupole).
  260 continue
        call tmbend(fsec, ftrk, orbit, fmap, el, ek, re, te)
      go to 500
 
*---- LCAV cavity.
  270 continue
        call tmlcav(fsec, ftrk, orbit, fmap, el, ek, re, te)
      go to 500
*---- Reserved.
  280 continue
  290 continue
  300 continue
        call uzero(ek, 1, 6*mwflt)
        call m66one(re)
        if (fsec) call uzero(te, 1, 216*mwflt)
      go to 500
 
*---- User-defined elements.
  310 continue
        call tmuser(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
 
*---- End of element calculation; check for LUMP.
  500 continue
 
      end
