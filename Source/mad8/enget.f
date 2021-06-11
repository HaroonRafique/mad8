      subroutine enget
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fill complete BEAM common from BEAM bank.                          *
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
      double precision loc_elk(mfact), loc_elm(0:mbmult)
      integer i,k,iflag,locpt,bsequ_number
      character *(mcnam) seqnm
*-- flags for strength inversion depending on bvbeam
*     all flags in loc_elk except stated otherwise
*     drift, rbend(loc_elm), sbend(loc_elm), matrix, quadrupole
*     sextupole, octupole, multipole (loc_elm), solenoid, rfcavity
*     elseparator, srotation, yrotation, hkicker, kicker
*     vkicker, hmonitor, monitor, vmonitor, ecollimator
*     rcollimator, beambeam, lump, instrument, marker
*     gbend(loc_elm)
      data loc_elk /
     +  1.d0,  1.d0,  1.d0,  1.d0, -1.d0,
     + -1.d0, -1.d0,  1.d0,  1.d0,  1.d0,
     +  1.d0,  1.d0,  1.d0,  1.d0,  1.d0,
     +  1.d0,  1.d0,  1.d0,  1.d0,  1.d0,
     +  1.d0,  1.d0,  1.d0,  1.d0,  1.d0,
     +  1.d0,  24 * 1.d0 /
*--- multipole and field error inversion (per order, 1 = dipole)
      data loc_elm / 1.d0, mbmult * -1.d0 /
      if (lbeam .eq. 0)  then
        prtnam = ' '
        bsequnam = 'NO_SEQU'
        amass = 0.0
        charge = 0.0
        en0 = 0.0
        pc = 0.0
        gamma = 0.0
        ex = 1.0
        exn = 0.0
        ey = 1.0
        eyn = 0.0
        et = 1.0
        sigt = 0.0
        sige = 0.0
        bunch = 1.0
        parnum = 0.0
        currnt = 0.0
        sigx = 0.0
        sigy = 0.0
        freq0 = 0.0
        beta = 0.0
        u0 = 0.0
        arad = 0.0
        beambv = 1.d0
        pdamp(1) = 0.0
        pdamp(2) = 0.0
        pdamp(3) = 0.0
        fbch = .true.
        frad = .false.
        ietflg = 1
        ipnflg = 1
      elseif (currseq .ne. 0)  then
        seqnm = sequnam
        k = bsequ_number(seqnm)
        if (k .eq. 0)  then
          seqnm = 'NO_SEQU'
          k = 1
        endif
*--- copy bank
        locpt = lq(lq(lroot-mbeam)-k)
        k = iq(locpt-3)
        i = iq(locpt-1)
        call ucopy(lq(locpt-k-1), lq(lbeam-k-1), k+1)
        call ucopy(iq(locpt+1), iq(lbeam+1), i)
        call utgnam(lbeam, mbpart, mbpart, prtnam)
        call utgnam(lbeam, mbsequ, mbsequ, bsequnam)
        call utgflt(lbeam, mbmass, mbcurr, amass)
        call utglog(lbeam, mbfbch, mbfrad, fbch)
        call utgflt(lbeam, mbfreq, mbdata, freq0)
        iflag  = iq(lbeam-5)
        ietflg = mod(iflag,16)
        ipnflg = mod(iflag/16,16)
      endif
      if (beambv .lt. 0.d0)  then
        do i = 1, mfact
          elkfact(i) = abs(beambv) * loc_elk(i)
        enddo
        do i = 0, mbmult
          elmfact(i) = abs(beambv) * loc_elm(i)
        enddo
      else
        do i = 1, mfact
          elkfact(i) = beambv
        enddo
        do i = 0, mbmult
          elmfact(i) = beambv
        enddo
      endif
*---- Make sure that flags are set properly.
      do 10 i = 1, maxdof
        doflag(i) = .true.
   10 continue
      dorad  = frad
 
      end
