      subroutine suelem(elmlen, arclen, ve, we)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute Displacement and rotation for one element.                 *
* Output:                                                              *
*   ELMLEN    (real)    Nominal element length.                        *
*   ARCLEN    (real)    Element length along design orbit.             *
*   VE(3)     (real)    Displacement of exit w.r.t. entry.             *
*   WE(3,3)   (real)    Rotation of exit w.r.t. entry.                 *
* Reference pointer used:                                              *
*   LCELM     /REFER/   Current element bank.                          *
* Local links:                                                         *
*   LSEQ                Beam lines sequence for a lump.                *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Added LCAVITY element at ISP 27                                    *
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
      integer ienum,iflag,ileng,ilm,ils,ilv,iocc,isp,jbit,lseq
      double precision angle,arclen,cospsi,costhe,ds,dx,elmlen,sinpsi,
     +sinthe,tilt,ve,we,zero
      dimension         ve(3), we(3,3)
 
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
 
      parameter         (zero = 0.0d0)
 
      character*(mcnam) elmnam
      integer bvpos
      parameter         (bvpos = 24)
      logical bvflag
 
*---- Define default values.
      bvflag = .false.
      llump = 0
 1000 continue
      elmlen = 0.0
      arclen = 0.0
      angle = 0.0
      tilt = 0.0
 
*---- Branch on subprocess code.
      call suiden(ve, we)
      isp = iq(lcelm+mbsp)
      go to ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +       210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     +       310, 310, 310, 310, 310, 310, 310, 310, 310, 310), isp
 
*---- Drift space.
   10 continue
 
*---- Arbitrary matrix.
   40 continue
 
*---- Quadrupole.
   50 continue
 
*---- Sextupole.
   60 continue
 
*---- Octupole.
   70 continue
 
*---- Solenoid.
   90 continue
 
*---- RF cavity.
  100 continue
 
*---- Electrostatic separator.
  110 continue
 
*---- Kickers.
  140 continue
  150 continue
  160 continue
 
*---- Monitors.
  170 continue
  180 continue
  190 continue
 
*---- Apertures.
  200 continue
  210 continue
 
*---- Marker.
  250 continue
 
*---- Beam-beam.
  220 continue
 
*---- lcavity
  270 continue
*---- Reserved.
  280 continue
  290 continue
  300 continue
 
*---- Beam instrument.
  240 continue
        call ucopy(q(lcelm+melen), elmlen, mwflt)
        arclen = elmlen
        ve(3) = elmlen
      go to 500
 
*---- Rectangular bend.
   20 continue
        call ucopy(q(lcelm+melen), elmlen, mwflt)
        call ucopy(q(lcelm+meangb), angle, mwflt)
        call ucopy(q(lcelm+metltb), tilt, mwflt)
        call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
        if (bvflag)  angle = beambv * angle
        if (angle .eq. 0.0) then
          dx = 0.0
          ds = elmlen
          arclen = elmlen
        else
          dx = - elmlen * sin(angle / 2.0)
          ds = + elmlen * cos(angle / 2.0)
          arclen = elmlen * angle / (2.0 * sin(angle / 2.0))
        endif
      go to 490
 
*---- Sector bend.
   30 continue
        call ucopy(q(lcelm+melen), elmlen, mwflt)
        call ucopy(q(lcelm+meangb), angle, mwflt)
        call ucopy(q(lcelm+metltb), tilt, mwflt)
        call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
 
        if (bvflag)  angle = beambv * angle
        if (angle .eq. 0.0) then
          dx = 0.0
          ds = elmlen
          arclen = elmlen
        else
          dx = (elmlen / angle) * (cos(angle) - 1.0)
          ds = (elmlen / angle) * sin(angle)
          arclen = elmlen
        endif
      go to 490
 
*---- Multipoles.
   80 continue
        call ucopy(q(lcelm+meklm), angle, mwflt)
        call ucopy(q(lcelm+metltm), tilt, mwflt)
        call utglog(lcelm, bvpos, bvpos, bvflag)
*--- HG000915 use bv flag to possibly invert angle
        if (bvflag) angle = beambv * angle
        dx = 0.0
        ds = 0.0
      go to 490
 
*---- General bend (dipole, quadrupole, and skew quadrupole).
  260 continue
        call ucopy(q(lcelm+melen), elmlen, mwflt)
        call ucopy(q(lcelm+meangg), angle, mwflt)
        call ucopy(q(lcelm+metltg), tilt, mwflt)
        call ucopy(q(lcelm+meintgx+3*mcsiz), bvflag, 1)
 
        if (bvflag)  angle = beambv * angle
        if (angle .eq. 0.0) then
          dx = 0.0
          ds = elmlen
          arclen = elmlen
        else
          dx = - elmlen * sin(angle / 2.0)
          ds = + elmlen * cos(angle / 2.0)
          arclen = elmlen * angle / (2.0 * sin(angle / 2.0))
        endif
      go to 490
 
*---- Rotation around S-axis.
  120 continue
        call ucopy(q(lcelm+meangr), angle, mwflt)
        we(1,1) = cos(angle)
        we(2,1) = sin(angle)
        we(1,2) = - we(2,1)
        we(2,2) = + we(1,1)
      go to 500
 
*---- Rotation around Y-axis.
  130 continue
        call ucopy(q(lcelm+meangr), angle, mwflt)
        we(1,1) = cos(angle)
        we(3,1) = sin(angle)
        we(1,3) = - we(3,1)
        we(3,3) = + we(1,1)
      go to 500
 
*---- User-defined elements.
  310 continue
        call suuser(elmlen, arclen, ve, we)
      go to 500
 
*---- Lump.
  230 continue
 
*---- Avoid recursive call.
        if (jbit(iq(lcelm),mxlmp) .ne. 0) then
          call diname(ldbnk, iq(lcelm+mbnam), elmnam)
          call utleng(elmnam, ileng)
          msg(1) =
     +    'LUMP "' // elmnam(1:ileng) // '" has a recursive definition,'
          msg(2) = 'identity transformation used.'
          call aawarn('SUELEM', 2, msg)
          go to 500
        endif
        call sbit1(iq(lcelm), mxlmp)
 
*---- Stack data for element.
        call mzbook(2, llump, llump, 1, 'LSTK', 2, 1, 4, 2, 0)
        lq(llump-2) = lcelm
 
*---- Expand the element sequence for this lump.
*     Avoid dropping of a previous sequence.
        call lnrefe(lcelm, 4, lseq, llump, -1)
        iq(llump+1) = iq(lseq+msr1)
        iq(llump+2) = iq(lseq+msr2)
        iq(llump+3) = iq(lseq+msr1) - 1
 
*---- Assign working space for accumulation of transform.
        iq(llump+4) = iwork
        ils = iwork
        ilv = iwork + 2
        ilm = iwork + 5
        iwork = iwork + 14
        if (iwork .gt. nwork) then
          call mzwork(0, dq(1), dq(iwork+1), 2)
          nwork = iwork
        endif
        dq(ils+1) = 0.0
        dq(ils+2) = 0.0
        call suiden(dq(ilv+1), dq(ilm+1))
 
*---- Find first element and compute its transform.
        go to 510
 
*---- Common for bends and multipoles: Displacement and rotation matrix.
  490 continue
      cospsi = cos(tilt)
      sinpsi = sin(tilt)
      costhe = cos(angle)
      sinthe = sin(angle)
      ve(1) = dx * cospsi
      ve(2) = dx * sinpsi
      ve(3) = ds
      we(1,1) = costhe * cospsi**2 + sinpsi**2
      we(2,1) = (costhe - 1.0) * cospsi * sinpsi
      we(3,1) = sinthe * cospsi
      we(1,2) = we(2,1)
      we(2,2) = costhe * sinpsi**2 + cospsi**2
      we(3,2) = sinthe * sinpsi
      we(1,3) = - we(3,1)
      we(2,3) = - we(3,2)
      we(3,3) = costhe
 
*---- End of element calculation; check for LUMP.
  500 continue
      if (llump .eq. 0) go to 550
 
*---- Accumulate transform for lump.
      ils = iq(llump+4)
      ilv = ils + 2
      ilm = ils + 5
      dq(ils+1) = dq(ils+1) + arclen
      dq(ils+2) = dq(ils+2) + elmlen
      call sutrak(dq(ilv+1), dq(ilm+1), ve, we)
 
*---- Test for end of sequence.
  510 iq(llump+3) = iq(llump+3) + 1
      if (iq(llump+3) .le. iq(llump+2)) then
        lseq = lq(llump-1)
        call utelem(lseq, iq(llump+3), iflag, elmnam, iocc, ienum)
        if (iq(lcelm+mbpr) .eq. mpelm) go to 1000
        go to 510
      endif
 
*---- Clear call bit.
      lcelm = lq(llump-2)
      call sbit0(iq(lcelm), mxlmp)
 
*---- Store length for current lump.
      call ucopy(dq(ils+2), q(lcelm+melen), mwflt)
 
*---- Return transform.
      arclen = dq(ils+1)
      elmlen = dq(ils+2)
      call ucopy(dq(ilv+1), ve, 3 * mwflt)
      call ucopy(dq(ilm+1), we, 9 * mwflt)
 
*---- Release working space and unstack.
      iwork = iq(llump+4)
      call lndrop(lq(llump-1))
      call mzdrop(0, llump, '.')
      if (llump .ne. 0) go to 500
  550 continue
 
      end
