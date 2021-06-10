      subroutine bmbcon(e0)
      implicit none
************************************************************************
*
*     Returns energy dependent parameters (used for energy variation)
*     in BEAMIS, common /BMPMRL/ (+CA BMPMCM)
*
* input:
*    E0      energy in eV
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
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      integer ierr
      double precision bmeqfa,bmfcn,e0,fac,rhs,se2,siglog,sxi2,ub,xtau
 
      external bmeqfa, bmfcn
 
*--- gamma = E/mc**2
      game0 = e0 / amasc2
*--- B_rho
      beamis(74) = e0 / clight
*--- polarization time
      beamis(76) = eight * circ / (five * sqrt(three) *
     +alamda * clight * arad * beamis(3) * game0**5)
*--- radiation loss per turn
      beamis(37) = two * arad * e0 * game0**3 * beamis(2) / p3d6
*--- common factor for damping times following
      fac = (three * amasc2**3 * circ) / (arad * e0**3 * clight)
*--- damping times tau_x, tau_y, tau_e
      beamis(38) = fac / (beamis(2) - beamis(4))
      beamis(39) = fac / beamis(2)
      beamis(40) = fac / (two * beamis(2) + beamis(4))
*--- common factor for energy spread and emittances
      fac = p55d0 * alamda * e0**2 / (p32d0 * sqrt(three) * amasc2
     +**2)
*--- sigma_e
      se2 = fac *  beamis(3) / (two * beamis(2) + beamis(4))
      beamis(41) = sqrt(abs(se2))
*--- E_x_0
      beamis(44) = fac * beamis(5) / (beamis(2) - beamis(4))
*--- sig_x_int, sig_y_int
      sxi2 = abs(beamis(44) * beamis(28))
      beamis(42) = sqrt(sxi2)
      beamis(43) = zero
*--- coupling dependent parameters
      call bmcdep(coupl)
*--- bucket
      if (iq(lq(lbmpm-1)+micmd+mpbuck) .eq. 0)  then
*--- bucket not given
        rhs = half * beamis(40) / tauqs
        call bmfdz1(bmeqfa, rhs, one, fifty, p1dm6, 1000, xtau, ierr)
        beamis(51) = beamis(41) * sqrt(two * xtau)
        bucket = sqrt(two * xtau)
      else
*--- bucket given
        beamis(51) = bucket * beamis(41)
        iubdef(51) = 1
        xtau       = min(half * bucket**2, p17d1)
        tauqs      = beamis(40) / (two * xtau * exp(-xtau))
*--- mark tau_Q as calculated
        idtauq = 0
      endif
      ub = beamis(51) * e0
      siglog = log(beamis(51))
      beamis(52) = (p16d0 / three) * arad**2 * falfa
     +* ((log(four * game0**2) - half) * ( - siglog - five / eight)
     +- p2dm1 + siglog * (half * siglog + pfsig))
      rhs = pi * ub**2 * cvharm * abs(beamis(20))
     +/ (beamis(37) * e0 * p1d6 )
      call bmfdz1(bmfcn, rhs, p1dm8, one, p1dm6, 1000, beamis(53),
     +ierr)
 
      end
