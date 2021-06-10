      subroutine lmbend(nord, el, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a bending magnet.                            *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
* Output:                                                              *
*   EL        (real)    Bend length.                                   *
*   FP, FM    (map)     Bend map.                                      *
* Local link:                                                          *
*   LMAP                Transfer map for bend.                         *
*----------------------------------------------------------------------*
* Modified: 11-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Included FINTX attribute in RBEND, SBEND, and GBEND (NOTE: FINTX   *
*   is assumed to have same value as FINT if it is not set or is       *
*   negative as is the default in the dictionary file)                 *
* Modified: 25-MAR-1999, M. Woodley (SLAC)                             *
*   Fix pointer to FINTX parameter for SBEND and RBEND                 *
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
      integer i,j,ifm,ifp,igm,igp,ilink,isave,isp,itop,lmap,nd,nord
      double precision an,corr,dh,e1x,e1y,e2x,e2y,el,field,fint,fm,fp,h,
     +h1,h2,hgap,one,sk1,sk2,sk3,sks,tilt
      dimension         fp(*), fm(6,6)
      double precision an2
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
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
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
      double precision cohelp
 
*---- Communication area for TM module.
*     Positive sign means deflection to negative coordinates.
      common /tmcomm/   cohelp
      save              /tmcomm/
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      double precision fintx
      logical bvflag
 
      parameter         (one = 1.0d0)
      dimension         field(2,0:3)
 
      bvflag = .false.
*---- Allocate working space.
      isave = iwork
      ifm   = iwork
      igm   = ifm + 36
      ifp   = igm + 36
      igp   = ifp + 209
      iwork = igp + 209
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Test for presence of bend.
      call ucopy(q(lcelm+melen), el, mwflt)
      if (el .eq. 0.0) then
        call lmone(nord, fp, fm)
 
*---- Can we use precomputed map?
      else
        ilink = iq(lcelm+mbat) + mbelie
        lmap = lq(lcelm-ilink)
        if (lmap .ne. 0  .and.  lcfld .eq. 0) then
          itop = itop6(min(nord,4))
          call ucopy(q(lmap+1), fm, 36 * mwflt)
          call ucopy(q(lmap+36*mwflt+1), fp, mwflt * itop)
 
*---- Fetch bend data.
        else
          isp = iq(lcelm+mbsp)
 
*---- RBEND or SBEND.
          if (isp .ne. 26) then
            call ucopy(q(lcelm+meangb), an, mwflt)
            call ucopy(q(lcelm+mek1b), sk1, mwflt)
            call ucopy(q(lcelm+mee1b), e1x, mwflt)
            call ucopy(q(lcelm+mee2b), e2x, mwflt)
            call ucopy(q(lcelm+metltb), tilt, mwflt)
            call ucopy(q(lcelm+mek2b), sk2, mwflt)
            call ucopy(q(lcelm+meh1b), h1, mwflt)
            call ucopy(q(lcelm+meh2b), h2, mwflt)
            call ucopy(q(lcelm+megapb), hgap, mwflt)
            call ucopy(q(lcelm+meintb), fint, mwflt)
            call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
            call ucopy(q(lcelm+meintbx), fintx, mwflt)
            call ucopy(q(lcelm+mek3b), sk3, mwflt)
            sks = 0.0
 
*---- GBEND.
          else
            call ucopy(q(lcelm+meangg), an, mwflt)
            call ucopy(q(lcelm+mek1g), sk1, mwflt)
            call ucopy(q(lcelm+mee1g), e1x, mwflt)
            call ucopy(q(lcelm+mee2g), e2x, mwflt)
            call ucopy(q(lcelm+metltg), tilt, mwflt)
            call ucopy(q(lcelm+meksg), sks, mwflt)
            call ucopy(q(lcelm+meh1g), h1, mwflt)
            call ucopy(q(lcelm+meh2g), h2, mwflt)
            call ucopy(q(lcelm+megapg), hgap, mwflt)
            call ucopy(q(lcelm+meintg), fint, mwflt)
            call ucopy(q(lcelm+meintgx), fintx, mwflt)
            call ucopy(q(lcelm+meintgx+3*mcsiz), bvflag, 1)
            sk2 = 0.0
            sk3 = 0.0
          endif
 
*--- HG000915 use bv flag to possibly invert angle
        if (bvflag) an = beambv * an
          if (isp .eq. 2) then
*--- HG001026: arc length to rectangular bend
            an2 = an / 2.d0
            if (an2 .ne. 0.d0 .and. rbarc)  el = el * an2 / sin(an2)
            e1x = e1x + an2
            e2x = e2x + an2
          endif
          h = an / el
          corr = (h + h) * hgap * fint
          e1y = e1x - corr * (1.0 + sin(e1x)**2) / cos(e1x)
*---- Tor: use FINTX if set
          if (fintx .gt. 0) then
            corr = (h + h) * hgap * fintx
            else
            corr = (h + h) * hgap * fint
          endif
          e2y = e2x - corr * (1.0 + sin(e2x)**2) / cos(e2x)
 
*---- Fetch field errors, if any.
          if (lcfld .ne. 0) then
            nd = min(8*mwflt,iq(lcfld-1))
            call uzero(field, 1, 8*mwflt)
            call ucopy(q(lcfld+1), field, nd)
*--- HG000915 use bv flag to possibly invert angle
            if (bvflag) field(1,0) = beambv * field(1,0)
            dh = (- h * deltas + field(1,0) / el) / (one + deltas)
            sk1 = (sk1 + field(1,1)/el) / (one + deltas)
            sks = (sks + field(1,1)/el) / (one + deltas)
            sk2 = (sk2 + field(1,2)/el) / (one + deltas) * cohelp
            sk3 = (sk3 + field(1,3)/el) / (one + deltas)
          else
            dh = - h * deltas / (one + deltas)
            sk1 = sk1 / (one + deltas)
            sks = sks / (one + deltas)
            sk2 = sk2 / (one + deltas) * cohelp
            sk3 = sk3 / (one + deltas)
          endif
*--- apply inversion and scaling
          sk1 = sk1 * elmfact(1)
          sks = sks * elmfact(1)
          sk2 = sk2 * elmfact(2)
 
*---- Body of dipole.
          if (isp .ne. 26) then
            call lmsect(4,el,h,dh,sk1,sk2,sk3,dq(ifp+1),dq(ifm+1))
          else
            call tmgsec(.false.,el,h,dh,sk1,sks,ek,dq(ifm+1),te)
            call uzero(dq(ifm+1), 1, 210*mwflt)
            dq(ifp+1) =   ek(2)
            dq(ifp+2) = - ek(1)
            dq(ifp+3) =   ek(4)
            dq(ifp+4) = - ek(3)
            dq(ifp+5) =   ek(6)
            dq(ifp+6) = - ek(5)
          endif
 
*---- Fringe fields.
          call lmfrg1(4, h, e1x, e1y, sk1, h1, dq(igp+1), dq(igm+1))
          call lmcat(4, dq(igp+1), dq(igm+1), dq(ifp+1), dq(ifm+1),
     +               dq(ifp+1), dq(ifm+1))
          call lmfrg2(4, h, e2x, e2y, sk1, h2, dq(igp+1), dq(igm+1))
          call lmcat(4, dq(ifp+1), dq(ifm+1), dq(igp+1), dq(igm+1),
     +               dq(ifp+1), dq(ifm+1))
          call lmtilt(4, tilt, dq(ifp+1), dq(ifm+1))
          call lmcopy(min(nord,4), dq(ifp+1), dq(ifm+1), fp, fm)
 
*---- Store map for later re-use.
          if (lcfld .eq. 0) then
            nd = mwflt * (36 + 209)
            call mzbook(2, lmap, lcelm, -ilink, 'TMAP', 0, 0, nd,
     +                  mreal,0)
            call ucopy(dq(ifm+1), q(lmap+1), 36 * mwflt)
            call ucopy(dq(ifp+1), q(lmap+36*mwflt+1), 209 * mwflt)
          endif
        endif
 
*---- Clear terms of higher orders.
        do 10 i = 210, itop6(nord)
          fp(i) = 0.0
   10   continue
      endif
 
*---- Drop working storage.
      iwork = isave
 
      end
