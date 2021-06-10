      subroutine bmcurr(e0, pgiven, igiven)
      implicit none
************************************************************************
*
*     Returns currents, luminosities, particle numbers...
*     in BEAMIS, common /BMPMRL/ (+CA BMPMCM)
*
*--- input
*  E0        energy in eV
*  PGIVEN    TRUE if power given
*  IGIVEN    TRUE if current given
*
************************************************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
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
      integer mgcmd,micmd,mlcmd,mncmd,mnmbmi,mnmcav,mpbuck,mpclor,
     +mpcoup,mpdelq,mpevar,mpexda,mpi4i2,mpintr,mpkhm,mpmidc,mpnint,
     +mprang,mpsing,mpsynr,mptauq,mptous,mpxbsz,mpybsz,mpytol,mrcmd,
     +msbmpm,msbmrs
      double precision eight,fifty,five,four,half,one,p15d5,p16d0,p17d1,
     +p1d2,p1d3,p1d6,p1d9,p1dm15,p1dm2,p1dm3,p1dm4,p1dm6,p1dm8,p1dm9,
     +p23d0,p25d0,p2dm1,p32d0,p3d6,p55d0,p5dm3,p6d2,p6dm2,p8d2,pfacnb,
     +pfacnq,pfsig,rtodeg,seven,six,sixty,ten,three,twenty,two,twopi,
     +twothd,zero
      parameter      (zero   = 0.0d0,        one    = 1.0d0,
     +                two    = 2.0d0,        three  = 3.0d0,
     +                four   = 4.0d0,        five   = 5.0d0,
     +                six    = 6.0d0,        seven  = 7.0d0,
     +                eight  = 8.0d0,        ten    = 10.0d0,
     +                p16d0  = 16.0d0,       twenty = 20.0d0,
     +                p23d0  = 23.0d0,       p25d0  = 25.0d0,
     +                p32d0  = 32.0d0,       fifty  = 50.0d0,
     +                p55d0  = 55.0d0,       sixty  = 60.0d0,
     +                p1d2   = 1.0d2,        p17d1  = 17.0d1,
     +                p6d2   = 6.0d2,        p8d2   = 8.0d2,
     +                p1d3   = 1.0d3,        p1d6   = 1.0d6,
     +                p3d6   = 3.0d6,        p15d5  = 15.0d5,
     +                p1d9   = 1.0d9,        half   = 0.5d0,
     +                p1dm15 = 1d-15,        p1dm9  = 1.0d-9,
     +                p1dm8  = 1.0d-8,       p1dm6  = 1.0d-6,
     +                p1dm4  = 1.0d-4,       p1dm3  = 1.0d-3,
     +                p1dm2  = 1.0d-2,       p5dm3  = 5.0d-3,
     +                p6dm2  = 6.0d-2,       p2dm1  = 0.2d0    )
 
      parameter      (pfacnb = 0.40404d0,    pfacnq = 0.31859d0,
     +                pfsig  = 0.804d0                         )
 
      parameter      (twopi  = two * pi,     rtodeg = 180.0d0 / pi,
     +                twothd = two / three                     )
 
      parameter      (msbmpm = 2,            msbmrs = 16,
     +                mnmbmi = 80,           mnmcav = 9        )
 
      parameter      (micmd = 1,             mrcmd = micmd + 10,
     +                mlcmd = mrcmd + 6,     mncmd = mlcmd,
     +                mgcmd = mncmd + 2                        )
 
      parameter      (mpnint = 1                               )
 
      parameter      (mpdelq = 1,            mptauq = 2,
     +                mpbuck = 3,            mpcoup = 4,
     +                mpi4i2 = 5,            mpexda = 6,
     +                mpxbsz = 7,            mpybsz = 8,
     +                mpkhm  = 9,            mpytol = 10       )
 
      parameter      (mpsynr = 1,            mpclor = 2,
     +                mptous = 3,            mpsing = 4,
     +                mpevar = 5,            mpmidc = 6        )
 
      parameter      (mpintr = 1,            mprang = 2        )
 
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
      integer idcoup,idener,idtauq,iflgbm,ihilim,ihirng,ilolim,ilorng,
     +intrct,irg1,irg2,isup,iubdef,iucdef,lbmpm,lbmrs,lbref1,lbref2,
     +nbmcav,nbmelm,nint
      double precision alamda,amasc2,beamis,bucket,coupl,cvbtrf,cvfill,
     +cvfreq,cvharm,cvleng,cvpow,cvpsi,cvshnt,cvvolt,delq,dspytl,elak1,
     +elak2,elang,ele1d,ele2d,eleng,enerev,energv,exdata,fctkhm,fxbeam,
     +fybeam,game0,power,radius,si4i2,tauq,tauqs
      common /bmpmcm/   lbmpm, lbmrs, lbref1, lbref2
      save              /bmpmcm/
      common /bmpmin/   nint, ilolim, ihilim, ilorng, ihirng, intrct,
     +                  irg1, irg2, isup, nbmelm, nbmcav, idener,
     +                  idcoup, idtauq, iflgbm,
     +                  iucdef(mnmcav),iubdef(mnmbmi)
      save              /bmpmin/
      common /bmpmrl/   delq, bucket, si4i2, coupl, tauq,
     +                  exdata, fxbeam, fybeam, fctkhm, dspytl,
     +                  radius, energv, enerev, power, tauqs,
     +                  amasc2, alamda, game0, beamis(mnmbmi)
      save              /bmpmrl/
      common /bmpmel/   eleng, elang, elak1, elak2, ele1d, ele2d
      save              /bmpmel/
      common /bmpmcv/   cvvolt, cvpsi, cvfreq, cvleng, cvbtrf,
     +                  cvpow, cvshnt, cvfill, cvharm
      save              /bmpmcv/
      common /bmpmlg/   synrad, clorb, tousch, single, evary, polwig,
     +                  midarc, sym
      logical           synrad, clorb, tousch, single, evary, polwig,
     +                  midarc, sym
      save              /bmpmlg/
      common /bmpmch/   chname, chtype, chbvar(mnmbmi)
      save   /bmpmch/
      character*(mcnam) chname, chtype
      character*40      chbvar
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
      integer ierr
      double precision bmzpow,dummy,e0,temp,temp1,temp2
      dimension dummy(2)
 
      logical pgiven, igiven
 
      external bmzpow
 
      if (pgiven)  then
        if (single)  then
          temp1 = beamis(37)
        else
          temp1 = two * beamis(37)
        endif
        temp2
     +  = (power - beamis(37)**2 / (cvleng * cvshnt * beamis(53)**2))
     +  / temp1 + p1dm2
        call bmfdz1(bmzpow, e0, p1dm3, temp2, p1dm6, 1000, beamis(54),
     +  ierr)
        beamis(55) = beamis(54)
        if (ierr .ne. 0)  then
          call aawarn('BMCURR', 2, 'Error when trying to find current'
     +    // ' --- values resulting from this will be wrong.')
        endif
        call bmludq(beamis(54), beamis(56), beamis(58), beamis(60),
     +  beamis(63))
*--- mark delta_Q as calculated
        iubdef(62) = 0
*--- voltage loss due to higher mode losses
        beamis(65) = p1d6 * beamis(33) * beamis(54)
        if (iubdef(36) .eq. 0)  then
*--- voltage not given - calculate
          call bmvolt(e0, beamis(54), beamis(36), beamis(64))
        else
          call bmvdat(beamis(36), e0, beamis(54), beamis(56),
     +    beamis(58))
*--- mark tau_Q as calculated
          idtauq = 0
        endif
        call bmsync(e0)
*--- call BMPOWR to get psi_RF and beta_RF
        call bmpowr(e0, beamis(54), dummy, beamis(70),
     +  beamis(71))
        if (iq(lq(lbmpm-1)+micmd+mpcoup) .eq. 0)  then
          if (beamis(62) .lt. delq)  then
*--- modify coupling because delta_q (calc) < delta_q (given)
            call bmcoup(coupl)
            idcoup = 0
*--- recalculate coupling dependent parameters
            call bmcdep(coupl)
            call bmludq(beamis(54), beamis(56), beamis(58),
     +      beamis(60), beamis(63))
          endif
        endif
*--- come here if power not given
      elseif (igiven)  then
*--- current given
        beamis(54) = beamis(35)
        beamis(55) = beamis(35)
        iubdef(54) = 1
        call bmludq(beamis(54), beamis(56), beamis(58), beamis(60),
     +  beamis(63))
*--- mark delta_Q as calculated
        iubdef(62) = 0
*--- voltage loss due to higher mode losses
        beamis(65) = p1d6 * beamis(33) * beamis(54)
        if (iubdef(36) .eq. 0)  then
*--- voltage not given - calculate
          call bmvolt(e0, beamis(54), beamis(36), beamis(64))
        else
          call bmvdat(beamis(36), e0, beamis(54), beamis(56),
     +    beamis(58))
*--- mark tau_Q as calculated
          idtauq = 0
        endif
        call bmsync(e0)
        call bmpowr(e0, beamis(54), beamis(72), beamis(70),
     +  beamis(71))
        if (iq(lq(lbmpm-1)+micmd+mpcoup) .eq. 0)  then
          if (beamis(62) .lt. delq)  then
*--- modify coupling because delta_q (calc) < delta_q (given)
            call bmcoup(coupl)
            idcoup = 0
*--- recalculate coupling dependent parameters
            call bmcdep(coupl)
            call bmludq(beamis(54), beamis(56), beamis(58),
     +      beamis(60), beamis(63))
          endif
        endif
      else
*--- consider DELQ as given
*    particle numbers:
        temp = twopi * delq * bunch * (beamis(49)
     +  + beamis(50)) * game0 / arad
        beamis(56) = temp * beamis(49) / beamis(28)
        beamis(57) = temp * beamis(50) / beamis(29)
*---  currents:
        beamis(54) = qelect * beamis(24) * beamis(56)
        beamis(55) = qelect * beamis(24) * beamis(57)
        call bmludq(beamis(54), dummy, beamis(58), beamis(60),
     +  beamis(63))
*--- voltage loss due to higher mode losses
        beamis(65) = p1d6 * beamis(33) * min(beamis(54), beamis(55))
        if (iubdef(36) .eq. 0)  then
*--- voltage not given - calculate
          call bmvolt(e0, min(beamis(54), beamis(55)), beamis(36),
     +    beamis(64))
        else
          call bmvdat(beamis(36), e0, min(beamis(54), beamis(55)),
     +    min(beamis(56), beamis(57)), min(beamis(58), beamis(59)))
*--- mark tau_Q as calculated
          idtauq = 0
        endif
        call bmsync(e0)
        call bmpowr(e0, min(beamis(54), beamis(55)), beamis(72),
     +  beamis(70), beamis(71))
      endif
 
      end
