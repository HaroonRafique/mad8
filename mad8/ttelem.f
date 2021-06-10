      subroutine ttelem(iturn, iorder, isup, ipos, sum,
     +                  track, number, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track through an element by TRANSPORT method (Switch routine).     *
* Output:                                                              *
*   ITURN     (integer) Turn number.                                   *
*   IORDER    (integer) Order for Lie algebraic tracking, unused.      *
*   ISUP      (integer) Superperiod number.                            *
*   IPOS      (integer) Position counter.                              *
* Input/output:                                                        *
*   SUM       (real)    Accumulated length.                            *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track.                       *
*   KTRACK    (integer) number of surviving tracks.                    *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Added LCAVITY element at ISP 27 ... calls routine TTLCAV           *
* Modified: 21-AUG-1999, T. Raubenheimer (SLAC)                        *
*   Added TTCSR to bend calculations                                   *
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
      integer iorder,ipos,isp,isup,iturn,nd,ktrack
      double precision el,one,parvec,sum,track,an2
      dimension         track(6,*)
      integer           number(*)
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      double precision cohelp
 
*---- Communication area for TM module.
*     Positive sign means deflection to negative coordinates.
      common /tmcomm/   cohelp
      save              /tmcomm/
      dimension parvec(mbbparam)
      parameter         (nd = 8 * mwflt)
      parameter         (one = 1.0d0)
 
      integer i,j
      logical           fsec, ftrk, fmap
      double precision ang, f_csr
      fsec = .true.
      ftrk = dorad  .and.  .not. dodamp
      isp = iq(lcelm+mbsp)
      el = 0.0
*-- switch on element type
      go to ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +       210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     +       310, 310, 310, 310, 310, 310, 310, 310, 310, 310), isp
 
*---- Drift space, monitors, beam instrument.
   10 continue
  170 continue
  180 continue
  190 continue
  240 continue
        call ttdrf(el, track, ktrack)
      go to 500
 
*---- Bending magnet.
   20 continue
   30 continue
*---- Get length, angle, and CSR factor (parameters #2, #3, and #15).
        call ucopy(q(lcelm+melen+0*mcsiz), el, mwflt)
        call ucopy(q(lcelm+melen+1*mcsiz), ang, mwflt)
        call ucopy(q(lcelm+melen+13*mcsiz), f_csr, mwflt)
*--- HG001026: arc length to rectangular bend
        if (isp .eq. 2)  then
          an2  = ang / 2
          if (an2 .ne. 0.d0 .and. rbarc)  el = el * an2 / sin(an2)
        endif
        if (f_csr .ne. 0.0d0) then
          call ttcsr(f_csr/2, el, ang, track, ktrack)
        endif
        call ttbend(el, track, ktrack)
        if (f_csr .ne. 0.0d0) then
          call ttcsr(f_csr/2, el, ang, track, ktrack)
        endif
      go to 500
 
*---- Arbitrary matrix.
   40 continue
        call tmarb(fsec, ftrk, orbit, fmap, el, ek, re, te)
        call tttrak(ek, re, te, track, ktrack)
      go to 500
 
*---- Quadrupole.
   50 continue
        call ttquad(el, track, ktrack)
      go to 500
 
*---- Sextupole.
   60 continue
        call ttsext(el, track, ktrack)
      go to 500
 
*---- Octupole.
   70 continue
        call ttoct(el, track, ktrack)
      go to 500
 
*---- Multipole.
   80 continue
        call ttmult(track, ktrack)
      go to 500
 
*---- Solenoid.
   90 continue
        call ttsol(el, track, ktrack)
      go to 500
 
*---- RF cavity.
  100 continue
        call ttrf(el, track, ktrack)
      go to 500
 
*---- Electrostatic separator.
  110 continue
        call ttsep(el, track, ktrack)
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
        call ttcorr(el, track, ktrack)
      go to 500
 
*---- Elliptic aperture.
  200 continue
        call trkill(1,iturn,isup,ipos,sum,el,track,number,ktrack)
      go to 500
 
*---- Rectangular aperture.
  210 continue
        call trkill(2,iturn,isup,ipos,sum,el,track,number,ktrack)
      go to 500
 
*---- Beam-beam.
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
      j = mechg
      do i = 1, 17
        j = j + mcsiz
        call ucopy(q(lcelm+j), parvec(i+9), mwflt)
      enddo
      if (parvec(23) .eq. 0.d0) then
*--- standard 4D
        call ttbb(parvec, track, ktrack)
      else
*--- Hirata 6D
        call beamint(parvec, track, ktrack)
      endif
      go to 500
 
*---- Lump.
  230 continue
        call tmlump(fsec,.false.,orbit,fmap,el,ek,re,te)
        call tttrak(ek,re,te,track,ktrack)
      go to 500
 
*---- Marker.
  250 continue
      go to 500
 
*---- General bend (dipole, quadrupole, and skew quadrupole).
  260 continue
*---- Get length, angle, and CSR factor (parameters #2, #3, and #15).
        call ucopy(q(lcelm+melen+0*mcsiz), el, mwflt)
        call ucopy(q(lcelm+melen+1*mcsiz), ang, mwflt)
        call ucopy(q(lcelm+melen+13*mcsiz), f_csr, mwflt)
        if (f_csr .ne. 0.0d0) then
          call ttcsr(f_csr/2, el, ang, track, ktrack)
        endif
        call ttbend(el, track, ktrack)
        if (f_csr .ne. 0.0d0) then
          call ttcsr(f_csr/2, el, ang, track, ktrack)
        endif
      go to 500
 
*---- LCAV cavity.
  270 continue
        call ttlcav(el, track, ktrack)
      go to 500
*---- Reserved.
  280 continue
  290 continue
  300 continue
      go to 500
 
*---- User-defined elements.
  310 continue
        call tmuser(fsec, ftrk, one, orbit, fmap, el, ek, re, te)
        call tttrak(ek, re, te, track, ktrack)
      go to 500
 
*---- Accumulate length.
  500 continue
      sum = sum + el
 
      end
