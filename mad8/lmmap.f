      subroutine lmmap(nord, el, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Return Lie algebraic map for an element.                          *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
* Output:                                                              *
*   EL        (real)    Element length.                                *
*   FP, FM    (map)     Element map.                                   *
* Local links:                                                         *
*   LMAP                Transfer map for a lump.                       *
*   LSEQ                Beam lines sequence for a lump.                *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Added LCAVITY element at ISP 27 ... calls routine LMLCAV           *
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
      integer i,ileng,iord,isp,lmap,nd,ne,nord
      double precision efield,el,field,fm,fp,one,phi,psi,rff,rfl,rfv,
     +sk1,sk2,sk3,sks,tilt,xcm,xkick,ycm,ykick,zero
      dimension         fp(*), fm(6,6)
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
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
 
      parameter         (nd = 8 * mwflt)
      parameter         (zero = 0.0d0, one = 1.0d0)
 
      character*(mcnam) elmnam
      dimension         field(2,0:3)
      logical bvflag
 
      bvflag = .false.
*---- Field error data.
 1000 continue
      call uzero(field, 1, nd)
      if (lcfld .ne. 0) then
        ne = min(iq(lcfld-1),nd)
        call ucopy(q(lcfld+1), field, ne)
      endif
 
*---- Switch for element type.
      el = 0.0
      isp = iq(lcelm+mbsp)
      go to ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +       210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     +       310, 310, 310, 310, 310, 310, 310, 310, 310, 310), isp
 
*---- Drift space.
   10 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call lmdrf(6, el, fp, fm)
      go to 500
 
*---- All bending magnets RBEND, SBEND, GBEND.
   20 continue
   30 continue
  260 continue
        call lmbend(6, el, fp, fm)
      go to 500
 
*---- Arbitrary matrix.
   40 continue
        call lmarb(6, fp, fm)
      go to 500
 
*---- Quadrupole.
   50 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek1q), sk1, mwflt)
        sk1 = (sk1 + field(1,1)/el) / (one + deltas)
*--- apply inversion and scaling
        sk1 = sk1 * elkfact(5)
        call ucopy(q(lcelm+metltq), tilt, mwflt)
        call lmquad(6, el, sk1, tilt, fp, fm)
      go to 500
 
*---- Sextupole.
   60 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek2s), sk2, mwflt)
        sk2 = (sk2 + field(1,2)/el) / (one + deltas)
*--- apply inversion and scaling
        sk2 = sk2 * elkfact(6)
        call ucopy(q(lcelm+metlts), tilt, mwflt)
        call lmsext(6, el, sk2, tilt, fp, fm)
      go to 500
 
*---- Octupole.
   70 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek3o), sk3, mwflt)
        sk3 = (sk3 + field(1,3)/el) / (one + deltas)
*--- apply inversion and scaling
        sk3 = sk3 * elkfact(7)
        call ucopy(q(lcelm+metlto), tilt, mwflt)
        call lmoct(6, el, sk3, tilt, fp, fm)
      go to 500
 
*---- Multipole.
   80 continue
        call lmmult(3, fp, fm)
      go to 500
 
*---- Solenoid.
   90 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mekss), sks, mwflt)
        sks = sks / (one + deltas)
*--- apply inversion and scaling
        sks = sks * elkfact(9)
        call lmsol(6, el, sks, fp, fm)
      go to 500
 
*---- RF Cavity.
  100 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mevltc), rfv, mwflt)
        call ucopy(q(lcelm+melagc), rfl, mwflt)
        call ucopy(q(lcelm+mefrqc), rff, mwflt)
        call lmrf(6, el, rfv, rfl, rff, fp, fm)
      go to 500
 
*---- Electrostatic separator.
  110 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+meflde), efield, mwflt)
        call ucopy(q(lcelm+metlte), tilt, mwflt)
        call lmsep(6, el, efield, tilt, fp, fm)
      go to 500
 
*---- S-Rotation.
  120 continue
        call ucopy(q(lcelm+meangr), psi, mwflt)
        call lmsrot(6, psi, fp, fm)
      go to 500
 
*---- Y-Rotation.
  130 continue
        call ucopy(q(lcelm+meangr), phi, mwflt)
        call lmyrot(6, phi, fp, fm)
      go to 500
 
*---- Correctors.
  140 continue
  150 continue
  160 continue
        call ucopy(q(lcelm+melen), el, mwflt)
 
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
 
*---- Apply sum of original setting, correction, and field error.
        xcm = xcm + xkick + field(1,0)
        ycm = ycm + ykick + field(2,0)
*--- HG000915 use bv flag to possibly invert angle
        if (bvflag) then
          xcm = beambv * xcm
          ycm = beambv * ycm
        endif
        call lmcorr(6, el, xcm, ycm, fp, fm)
      go to 500
 
*---- Monitors.
  170 continue
  180 continue
  190 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call lmdrf(6, el, fp, fm)
      go to 500
 
*---- Apertures.
  200 continue
  210 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call lmdrf(6, el, fp, fm)
      go to 500
 
*---- Beam-Beam: not allowed.
  220 continue
        call diname(ldbnk, iq(lcelm+mbnam), elmnam)
        call utleng(elmnam, ileng)
        msg(1) = 'Lie-algebraic map for beam-beam element is not known,'
        msg(2) = 'identity used for "' // elmnam(1:ileng) // '".'
        call aawarn('LMMAP', 2, msg)
        call lmone(6, fp, fm)
      go to 500
 
*---- Lump.
  230 continue
        iord = 4
        call utgint(lcelm, 3, 3, iord)
        iord = min(iord, 6)
        lmap = lq(lcelm-iq(lcelm+mbat)-mbelie)
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lmap+1), fm, 36*mwflt)
        call ucopy(q(lmap+36*mwflt+1), fp, itop6(iord)*mwflt)
        do 235 i = ibot6(iord+1), itop6(nord)
          fp(i) = 0.0
 235    continue
      go to 500
 
*---- Beam instrument.
  240 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call lmdrf(6, el, fp, fm)
      go to 500
 
*---- Marker.
  250 continue
        call lmone(6, fp, fm)
      go to 500
 
*---- LCAV Cavity.
  270 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mevltc), rfv, mwflt)
        call ucopy(q(lcelm+melagc), rfl, mwflt)
        call ucopy(q(lcelm+mefrqc), rff, mwflt)
        call lmlcav(6, el, rfv, rfl, rff, fp, fm)
      go to 500
  280 continue
  290 continue
  300 continue
        call lmone(6, fp, fm)
      go to 500
 
*---- User-defined elements.
  310 continue
        call lmuser(6, isp, el, fp, fm)
 
*---- End of element calculation; check for LUMP.
  500 continue
 
      end
