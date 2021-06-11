      subroutine ttlcav(el, track, ktrack)
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track a set of trajectories through a thin accelerating structure  *
*   (zero length); the structure is sandwiched between two drift       *
*   spaces of half the structure length.                               *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   KTRACK    (integer) Number of surviving tracks.                    *
* Output:                                                              *
*   EL        (real)    Length of structure.                           *
*----------------------------------------------------------------------*
* Created:  06-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Routine copied and modified from TTRF; modified to allow wakefield *
*   tracking -- use error attributes; note ELOSS is NOT subtracted     *
*   from average energy gain if wakefields are set                     *
* Modified: 21-SEP-1999, M. Woodley (SLAC)                             *
*   Convert to standard MAD units (electric voltage in MV, frequency   *
*   in MHz, phase angles in multiples of 2pi); ELOSS attribute units   *
*   are V/C; use DODEFL flag to turn on/off edge focusing              *
*----------------------------------------------------------------------*
 
      implicit none
      integer ktrack
      double precision el, track(6,*)
      integer nbin, lstr, itrack
      double precision deltae, phi0, freq, eloss, volt_err, alag_err,
     +binmax, el1, vrf, omega, phirf, etas, bi2gi2, denergy, dl, px,
     +py, pt, etmp, ttt, beti
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      double precision twopi,ten3m,ten3p,ten6m,ten6p,zero,half,
     +one,two,three
      parameter         (twopi = 2.0d0 * pi)
      parameter         (ten3m = 1.0d-3, ten3p = 1.0d+3)
      parameter         (ten6m = 1.0d-6, ten6p = 1.0d+6)
      parameter         (zero = 0.0d0, half = 0.5d0)
      parameter         (one = 1.0d0, two = 2.0d0, three = 3.0d0)
 
      logical           error, lwake
      character*80      lfile, tfile
 
      logical           dodefl
      data              dodefl /.true./
*---------+---------+---------+---------+---------+---------+---------++
*---- Fetch data from pool.
      call ucopy(q(lcelm+melen), el, mwflt)
      call ucopy(q(lcelm+melen+2*mcsiz), deltae, mwflt)
      call ucopy(q(lcelm+melen+3*mcsiz), phi0, mwflt)
      call ucopy(q(lcelm+melen+4*mcsiz), freq, mwflt)
 
*---- Get energy loss if it was set - otherwise it is zero.
      call ucopy(q(lcelm+melen+5*mcsiz), eloss, mwflt)
      eloss = (ten6m * eloss) * (qelect * parnum)
 
*---- Get errors (parameters #8 and #9).
      call ucopy(q(lcelm+melen+6*mcsiz), volt_err, mwflt)
      call ucopy(q(lcelm+melen+7*mcsiz), alag_err, mwflt)
 
*---- Get wakefield codes (parameters #11 and #12).
      nbin = iq(lcelm+melen+9*mcsiz)
      call ucopy(q(lcelm+melen+10*mcsiz), binmax, mwflt)
 
*---- Get the longitudinal wakefield filename (parameter #13).
      if (iq(lcelm+melen+11*mcsiz-2) .eq. 61) then
        lstr = iq(lcelm+melen+11*mcsiz)
        call uhtoc(iq(lq(lcelm-13)+1), mcwrd, lfile, 80)
      else
        lfile = " "
      endif
 
*---- Get the transverse wakefield filename (parameter #14).
      if (iq(lcelm+melen+12*mcsiz-2) .eq. 61) then
        lstr = iq(lcelm+melen+12*mcsiz)
        call uhtoc(iq(lq(lcelm-14)+1), mcwrd, tfile, 80)
      else
        tfile = " "
      endif
 
*---- If there are wakefields split the structure.
      if (lfile .ne. " " .or. tfile .ne. " ") then
        el1 = el / two
        deltae = deltae / two
        volt_err = volt_err / two
        eloss = eloss / two
        lwake = .true.
      else
        el1 = el
        lwake = .false.
      endif
 
*---- Set up.
*--- vrf, volt_err, eloss GeV
      vrf = ten3m * deltae
      volt_err = ten3m * volt_err
      eloss = ten3m * eloss
*--- omega radians/m
      omega = twopi * (ten6p * freq) / clight
*--- phirf, alag_err radians
      phirf = twopi * phi0
      alag_err = twopi * alag_err
 
*--- HG001121 start
      error = .false.
      if (ereset) then
*---- If incoming energy is not set, default it to 1 GeV ... warn user
*     later.
        if (ener1 .eq. zero) then
          ener1 = pc
          error = .true.
        endif
        if (ener1 .eq. zero) then
          ener1 = one
        endif
 
*---- Update relativistic terms at entrance to structure.
        en0 = ener1
      else
        ener1 = en0
      endif
*--- HG001121 end
*---- Update relativistic terms at entrance to structure.
      pc = sqrt(abs(en0**2 - amass**2))
      gamma = en0 / amass
      beta = pc / en0
      etas   = beta * gamma * (one + deltas)
      gammas = sqrt(one + etas**2)
      betas  = etas / gammas
      bi2gi2 = one / etas ** 2
      dtbyds = deltas * etas / betas
      beti = one / betas
 
*---- Change to the centroid energy value.
*     (NOTE: phase is referenced to the crest, NOT the zero crossing!)
      denergy = vrf * cos(phirf) - eloss ! gev
 
*---- Use EL1 rather than EL for the length.
      dl = el1 * half
      if (lwake) eloss = zero
 
*---- Loop for all particles.
      do 10 itrack = 1, ktrack
 
*---- Set up.
        px = track(2,itrack)
        py = track(4,itrack)
        pt = track(6,itrack)
 
*---- Thin quad to represent focusing at structure entrance.
        if (dodefl) then
          px = px - denergy / el1 / two / ener1 * track(1,itrack)
          py = py - denergy / el1 / two / ener1 * track(3,itrack)
        endif
 
*---- Drift to centre.
        ttt = one/sqrt(one+two*pt*beti+pt**2 - px**2 - py**2)
        track(1,itrack) = track(1,itrack) + dl*ttt*px
        track(3,itrack) = track(3,itrack) + dl*ttt*py
        track(5,itrack) = track(5,itrack)
     +  + dl*(beti - (beti+pt)*ttt) + dl*pt*dtbyds
 
*---- Acceleration.
*     (NOTE: phase is referenced to the crest, NOT the zero crossing!)
        etmp = ener1 + denergy
        pt = pt + ((vrf + volt_err)
     +     * cos(phirf + alag_err - omega * track(5,itrack))
     +     - eloss - denergy) / (etmp * (one+deltas))
        px = px * ener1 / etmp
        py = py * ener1 / etmp
        pt = pt * ener1 / etmp
 
*---- Update relativistic terms at centre of structure.
      pc = sqrt(abs(etmp**2 - amass**2))
      gamma = etmp / amass
      beta = pc / etmp
      etas   = beta * gamma * (one + deltas)
      gammas = sqrt(one + etas**2)
      betas  = etas / gammas
      bi2gi2 = one / etas ** 2
      dtbyds = deltas * etas / betas
      beti = one / betas
 
*---- Drift to end.
        ttt = one/sqrt(one+two*pt*beti+pt**2 - px**2 - py**2)
        track(1,itrack) = track(1,itrack) + dl*ttt*px
        track(3,itrack) = track(3,itrack) + dl*ttt*py
        track(5,itrack) = track(5,itrack)
     +  + dl*(beti - (beti+pt)*ttt) + dl*pt*dtbyds
 
*---- Thin quad to represent focusing at structure exit.
        if (dodefl) then
          px = px + denergy / el1 / two / (ener1 + denergy)
     +       * track(1,itrack)
          py = py + denergy / el1 / two / (ener1 + denergy)
     +       * track(3,itrack)
        endif
 
        track(2,itrack) = px
        track(4,itrack) = py
        track(6,itrack) = pt
   10 continue
 
*---- Energy change
      ener1 = ener1 + denergy
 
*---- If there were wakefields, track the wakes and then the 2nd half
*     of the structure.
      if (lwake) then
        call ttwake(two*el1, nbin, binmax, lfile, tfile, ener1, track,
     +              ktrack)
 
*---- Track 2nd half of structure -- loop for all particles.
        do 20 itrack = 1, ktrack
 
*---- Set up.
          px = track(2,itrack)
          py = track(4,itrack)
          pt = track(6,itrack)
 
*---- Thin quad to represent focusing at structure entrance.
          if (dodefl) then
            px = px - denergy / el1 / two / ener1 * track(1,itrack)
            py = py - denergy / el1 / two / ener1 * track(3,itrack)
          endif
 
*---- Drift to centre.
          beti = one / betas
          ttt = one/sqrt(one+two*pt*beti+pt**2 - px**2 - py**2)
          track(1,itrack) = track(1,itrack) + dl*ttt*px
          track(3,itrack) = track(3,itrack) + dl*ttt*py
          track(5,itrack) = track(5,itrack)
     +    + dl*(beti - (beti+pt)*ttt) + dl*pt*dtbyds
 
*---- Acceleration.
*     (NOTE: phase is referenced to the crest, NOT the zero crossing!)
          pt = pt + ((vrf + volt_err)
     +       * cos(phirf + alag_err - omega * track(5,itrack))
     +       - eloss - denergy) / (ener1 + denergy) / (one+deltas)
          px = px * ener1 / (ener1 + denergy)
          py = py * ener1 / (ener1 + denergy)
          pt = pt * ener1 / (ener1 + denergy)
 
*---- Drift to end.
          beti = one / betas
          ttt = one/sqrt(one+two*pt*beti+pt**2 - px**2 - py**2)
          track(1,itrack) = track(1,itrack) + dl*ttt*px
          track(3,itrack) = track(3,itrack) + dl*ttt*py
          track(5,itrack) = track(5,itrack)
     +    + dl*(beti - (beti+pt)*ttt) + dl*pt*dtbyds
 
*---- Thin quad to represent focusing at structure exit.
          if (dodefl) then
            px = px + denergy / el1 / two / (ener1 + denergy)
     +         * track(1,itrack)
            py = py + denergy / el1 / two / (ener1 + denergy)
     +         * track(3,itrack)
          endif
 
          track(2,itrack) = px
          track(4,itrack) = py
          track(6,itrack) = pt
   20   continue
 
*---- Energy change.
        ener1 = ener1 + denergy
      endif
 
*---- Update relativistic terms at exit of structure.
      en0 = ener1
      pc = sqrt(abs(en0**2 - amass**2))
      gamma = en0 / amass
      beta = pc / en0
      etas   = beta * gamma * (one + deltas)
      gammas = sqrt(one + etas**2)
      betas  = etas / gammas
 
*---- warn user if energy value had to be defaulted.
      if (error) then
         write (msg(1), 900) ener1 - denergy
         call aawarn ('ttlcav', 1, msg)
      endif
 
  900 format ('Set the initial energy value to: ',1p,g12.5)
 
      end
