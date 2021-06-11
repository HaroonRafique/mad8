      subroutine enbeam
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   BEAM command: Store data from BEAM command into BEAM bank.         *
* 18 attributes:                                                       *
*   PARTICLE, MASS,     CHARGE,   ENERGY,   PC,       GAMMA,           *
*   EX,       EY,       EXN,      EYN,      ET,       SIGT,            *
*   SIGE,     KBUNCH,   NPART,    BCURRENT, BUNCHED,  RADIATE          *
* Additional quantities kept in BEAM bank:                             *
*   BETA      (real)    Relativistic parameter v/c.                    *
*   U0        (real)    Radiation loss per turn in GeV.                *
*   ARAD      (real)    Classical particle radius.                     *
*   PDAMP(3)  (real)    Damping partition numbers.                     *
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
      integer mbarad,mbbeta,mbbnum,mbchrg,mbcurr,mbdamp,mbdata,mbener,
     +mbet,mbex,mbexn,mbey,mbeyn,mbfbch,mbfrad,mbfreq,mbgamm,mbmass,
     +mbpart,mbpc,mbpnum,mbsige,mbsigt,mbu0,mbsequ,mbbv
 
*---- Attribute positions in BEAM bank.
      parameter         (mbpart =  1, mbsequ =  2,
     +                   mbmass =  3, mbchrg =  4,
     +                   mbener =  5, mbpc   =  6, mbgamm =  7,
     +                   mbex   =  8, mbexn  =  9, mbey   = 10,
     +                   mbeyn  = 11, mbet   = 12, mbsigt = 13,
     +                   mbsige = 14, mbbnum = 15, mbpnum = 16,
     +                   mbcurr = 17, mbfbch = 18, mbfrad = 19,
     +                   mbfreq = 20, mbbeta = 21, mbu0   = 22,
     +                   mbarad = 23, mbbv   = 24,
     +                   mbdamp = 25, mbdata = 27)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
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
      integer ipart,maxprt
      double precision pachrg,pamass
 
      parameter         (maxprt =  6)
 
      character*(mcnam) padict(maxprt)
      dimension         pamass(maxprt), pachrg(maxprt)
      integer           itype(mbdata)
 
      data padict / 'ELECTRON', 'PROTON', 'POSITRON', 'ANTIPROTON',
     +              'POSMUON',  'NEGMUON' /
      data pamass /  emass,      pmass,    emass,      pmass,
     +               mumass,     mumass /
      data pachrg /  -1.0,        1.0,      1.0,       -1.0,
     +               1.0,        -1.0 /
 
*---- BEAM common already contains data from BEAM bank.
*     Overwrite new data coming from BEAM command.
      call utgtyp(lccmd, itype)
      call utgnam(lccmd, mbpart, mbpart, prtnam)
      call utgnam(lccmd, mbsequ, mbsequ, bsequnam)
      call utgflt(lccmd, mbmass, mbcurr, amass)
      call utglog(lccmd, mbfbch, mbfrad, fbch)
      if (itype(mbbv) .ne. 0) call utgflt(lccmd, mbbv, mbbv, beambv)
*---- Store particle name, mass and charge.
      if (itype(mbpart) .ne. 0) then
        call utlook(prtnam, padict, maxprt, ipart)
        if (ipart .ne. 0) then
          prtnam = padict(ipart)
          if (itype(mbmass) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- MASS will be ignored.')
          endif
          if (itype(mbchrg) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- CHARGE will be ignored.')
          endif
          amass = pamass(ipart)
          charge = pachrg(ipart)
        else
          call aawarn('ENBEAM', 1,
     +    'Unknown particle ' // prtnam // ' ignored')
        endif
      else if (amass .le. 0.0) then
        call aafail('ENBEAM', 1,
     +  'Inconsistent data on BEAM --- expect MASS > 0.')
      endif
*--- store sequence name, use default if none
      if (itype(mbsequ) .eq. 0) bsequnam = 'NO_SEQU'
*---- Store energy, p*c and gamma.
      if (itype(mbener) .ne. 0) then
        if (itype(mbpc) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- PC will be ignored.')
        endif
        if (itype(mbgamm) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- GAMMA will be ignored.')
        endif
        if (en0 .le. amass) then
          call aafail('ENBEAM', 1,
     +    'Inconsistent data on BEAM --- expect ENERGY > MASS.')
        else
          pc = sqrt(en0**2 - amass**2)
          gamma = en0 / amass
          beta = pc / en0
        endif
      else if (itype(mbpc) .ne. 0) then
        if (itype(mbgamm) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- GAMMA will be ignored.')
        endif
        en0 = sqrt(pc**2 + amass**2)
        gamma = en0 / amass
        beta = pc / en0
      else if (itype(mbgamm) .ne. 0) then
        if (gamma .le. 1.0) then
          call aafail('ENBEAM', 1,
     +    'Inconsistent data on BEAM --- expect GAMMA > 1.')
        else
          en0 = gamma * amass
          pc = sqrt(en0**2 - amass**2)
          beta = pc / en0
        endif
      else
        if (en0 .le. amass) then
          call aafail('ENBEAM', 1,
     +    'Inconsistent data on BEAM --- expect ENERGY > MASS.')
        else
          pc = sqrt(en0**2 - amass**2)
          gamma = en0 / amass
          beta = pc / en0
        endif
      endif
      betas = beta
      gammas = gamma
 
*---- Horizontal emittance.
      if (itype(mbex) .ne. 0) then
        if (itype(mbexn) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- EXN will be ignored.')
        endif
        exn = ex * (4.0 * beta * gamma)
      else if (itype(mbexn) .ne. 0) then
        ex = exn / (4.0 * beta * gamma)
      endif
 
*---- Vertical emittance.
      if (itype(mbey) .ne. 0) then
        if (itype(mbeyn) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- EYN will be ignored.')
        endif
        eyn = ey * (4.0 * beta * gamma)
      else if (itype(mbeyn) .ne. 0) then
        ey = eyn / (4.0 * beta * gamma)
      endif
 
*---- Longitudinal emittance: Cannot be dealt with at this time.
      if (itype(mbet) .ne. 0) then
        ietflg = 1
        if (itype(mbsigt) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- SIGT will be ignored.')
        endif
        if (itype(mbsige) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- SIGE will be ignored.')
        endif
      else if (itype(mbsigt) .ne. 0) then
        ietflg = 2
        if (itype(mbsige) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- SIGE will be ignored.')
        endif
      else if (itype(mbsige) .ne. 0) then
        ietflg = 3
      endif
 
*---- Beam current and particle number: Deferred handling.
      if (itype(mbcurr) .ne. 0) then
        ipnflg = 1
        if (itype(mbpnum) .ne. 0) then
            call aawarn('ENBEAM', 1,
     +      'Redundant data on BEAM --- NPART will be ignored.')
        endif
      else if (itype(mbpnum) .ne. 0) then
        ipnflg = 2
      endif
 
*---- Fill in BEAM bank and force re-evaluation of maps.
      if (.not. error) then
        arad = erad * emass / amass * charge**2
        call enput
        call aapdrp
      endif
 
      end
