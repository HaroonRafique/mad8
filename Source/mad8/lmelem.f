      subroutine lmelem(iturn, nord, isup, ipos, suml,
     +                  track, number, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track through an element by Lie-algebraic method.                  *
* Input:                                                               *
*   ITURN     (integer) Turn number.                                   *
*   NORD      (integer) Order desired for tracking.                    *
*   ISUP      (integer) Superperiod number.                            *
*   IPOS      (integer) Position counter.                              *
* Input/output:                                                        *
*   SUML      (real)    Accumulated length.                            *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track                        *
*   KTRACK    (integer) number of surviving tracks.                    *
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
      integer ifm,ifp,iord,ipos,isave,isp,isup,iturn,nd,ne,nord,ktrack
      double precision efield,el,field,one,parvec,rff,rfl,rfv,sk1,sk2,
     +sk3,sks,suml,tilt,track,xcm,xkick,ycm,ykick
      dimension         track(6,*)
      integer           number(*)
      integer i,j
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      parameter         (nd = 8 * mwflt, one = 1.0d0)
      dimension         field(2,0:3)
      dimension parvec(mbbparam)
      logical bvflag
 
      bvflag = .false.
*---- Initialize.
      isp = iq(lcelm+mbsp)
      el = 0.0
      iord = nord
      if (isp .eq. 23) iord = iq(lcelm+mbat+2*mcsiz+mcval)
 
*---- Allocate working space.
      isave = iwork
      ifm   = iwork
      ifp   = ifm + 36
      iwork = ifp + itop6(iord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Field error data.
      call uzero(field, 1, nd)
      if (lcfld .ne. 0) then
        ne = min(iq(lcfld-1), nd)
        call ucopy(q(lcfld+1), field, ne)
      endif
 
*---- Switch on element type.
      go to ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +       210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     +       310, 310, 310, 310, 310, 310, 310, 310, 310, 310), isp
 
*---- Drift space, treated by TRANSPORT method for speed.
   10 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ttdrf(el, track, ktrack)
      go to 500
 
*---- All Bending magnets, RBEND, SBEND, GBEND.
   20 continue
   30 continue
  260 continue
        call lmbend(iord, el, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Arbitrary map.
   40 continue
        call lmarb(iord, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Quadrupole.
   50 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek1q), sk1, mwflt)
        sk1 = (sk1 + field(1,1)) / (one + deltas)
*--- apply inversion and scaling
        sk1 = sk1 * elkfact(5)
        call ucopy(q(lcelm+metltq), tilt, mwflt)
        call lmquad(iord, el, sk1, tilt, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Sextupole.
   60 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek2s), sk2, mwflt)
        sk2 = (sk2 + field(1,2)) / (one + deltas)
*--- apply inversion and scaling
        sk2 = sk2 * elkfact(6)
        call ucopy(q(lcelm+metlts), tilt, mwflt)
        call lmsext(iord, el, sk2, tilt, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Octupole.
   70 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek3o), sk3, mwflt)
        sk3 = (sk3 + field(1,3)) / (one + deltas)
*--- apply inversion and scaling
        sk3 = sk3 * elkfact(7)
        call ucopy(q(lcelm+metlto), tilt, mwflt)
        call lmoct(iord, el, sk3, tilt, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Multipole, treated by TRANSPORT method for speed.
   80 continue
        call ttmult(track, ktrack)
      go to 500
 
*---- Solenoid.
   90 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mekss), sks, mwflt)
        sks = sks / (one + deltas)
*--- apply inversion and scaling
        sks = sks * elkfact(9)
        call lmsol(iord, el, sks, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- RF cavity.
  100 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mevltc), rfv, mwflt)
        call ucopy(q(lcelm+melagc), rfl, mwflt)
        call ucopy(q(lcelm+mefrqc), rff, mwflt)
        call lmrf(iord, el, rfv, rfl, rff, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Electrostatic separator.
  110 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+meflde), efield, mwflt)
        call ucopy(q(lcelm+metlte), tilt, mwflt)
        call lmsep(iord, el, efield, tilt, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Rotation around s-axis.
  120 continue
        call ttsrot(track, ktrack)
      go to 500
 
*---- Rotation around y-axis.
  130 continue
        call ttyrot(track, ktrack)
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
        call lmcorr(iord, el, xcm, ycm, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Monitor, "HMONITOR", "VMONITOR", "MONITOR".
  170 continue
  180 continue
  190 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ttdrf(el, track, ktrack)
      go to 500
 
*---- Elliptic collimator.
  200 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call trkill
     +    (1, iturn, isup, ipos, suml, el, track, number, ktrack)
      go to 500
 
*---- Rectangular collimator.
  210 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call trkill
     +    (2, iturn, isup, ipos, suml, el, track, number, ktrack)
      go to 500
 
*---- Beam-beam.
  220 continue
        if (parnum .ne. 0.0) then
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
          j = mechg
          do i = 1, 17
            j = j + mcsiz
            call ucopy(q(lcelm+j), parvec(i+9), mwflt)
          enddo
          if (parvec(23) .eq. 0.d0) then
*--- standard 4D
            call ttbb(parvec, track, ktrack)
          else
*--- Hirata 6D (no Lie map yet)
*            call beamint(parvec, track, ktrack)
          endif
        endif
      go to 500
 
*---- Lump.
  230 continue
        call lmmap(iord, el, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
 
*---- Beam instrument.
  240 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ttdrf(el, track, ktrack)
      go to 500
 
*---- Marker.
  250 continue
      go to 500
*---- LCAV cavity.
  270 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mevltc), rfv, mwflt)
        call ucopy(q(lcelm+melagc), rfl, mwflt)
        call ucopy(q(lcelm+mefrqc), rff, mwflt)
        call lmlcav(iord, el, rfv, rfl, rff, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
  280 continue
  290 continue
  300 continue
      go to 500
 
*---- User-defined elements.
  310 continue
        call lmuser(iord, isp, el, dq(ifp+1), dq(ifm+1))
        call lmtrak(iord, dq(ifp+1), dq(ifm+1), track, ktrack)
      go to 500
  500 continue
      suml = suml + el
 
*---- Drop working storage.
      iwork = isave
 
      end
