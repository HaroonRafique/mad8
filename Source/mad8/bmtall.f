      subroutine bmtall
      implicit none
************************************************************************
*
*     Calculates Touschek lifetime, synchrotron radiation, closed orbit
*     amplication factors, ...
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
      integer i,ie,ierr,iret,l,nrc
      double precision avbetx,avbety,bfacx,bfacy,bmgau1,bmjvav,bxbx,
     +bxby,ec,gamax,gamay,pbx,pby,photon,pqx,pqy,pw,qfacx,qfacy,rhob,
     +sigxd,sigyd,snbq,sum,sx2,sx2d,sy2,sy2d,tang,temp,vec
 
      dimension vec(7)
 
      logical wedge, usrng, bmusrg
 
      external bmjvav
 
*  VEC  (real)   vector of auxiliary variables
*                (1) = GAM   (E / mc**2)
*                (2) = AP0D
*                (3) = COUPL
*                (4) = BMSIGE**2 (BEAMIS(41)**2)
*                (5) = E_x_0
*                (6) = SJXY  (BEAMIS(25) / BEAMIS(26))
*                (7) = SIGB  (BEAMIS(51))
 
      vec(1) = enerev / amasc2
      vec(3) = coupl
      vec(4) = beamis(41)**2
      vec(5) = beamis(44)
      vec(6) = beamis(25) / beamis(26)
      vec(7) = beamis(51)
      ierr = 0
 
*--- reset user range count for elements
      nrc = 0
*--- reset Touschek sum
      sum  = zero
*--- factors
      qfacx = sqrt(beamis(77) * isup) / sin(pi * qx)
      qfacy = sqrt(beamis(78) * isup) / sin(pi * qy)
      bfacx = sqrt(beamis(79) * isup) / sin(pi * qx)
      bfacy = sqrt(beamis(80) * isup) / sin(pi * qy)
      do 10 i = 1, nbmelm
        call bmeget(i, iret)
*--- count user range
        usrng = bmusrg(i, ilorng, ihirng, iflgbm)
        if (usrng)  nrc = nrc + 1
*--- Touschek lifetime
        if (eleng .gt. zero .and. tousch)  then
          wedge  = chtype(:5) .eq. 'SBEND'
          if (wedge)  then
            tang = ele1d
          else
            tang = half * elang + ele1d
          endif
          vec(2) = disp(2) + disp(1) * tan(tang) * elang / eleng
          sum = sum + bmgau1(bmjvav, vec, zero, eleng, p1dm4, ie)
          ierr = max(ierr, ie)
        endif
        sx2 = vec(5) * betx + vec(4) * disp(1)**2
        sy2 = half * vec(5) * vec(6) * bety
        sx2d = sx2 * (one + alfx**2) / betx**2 + vec(4) * disp(2)**2
        sy2d = sy2 * (one + alfy**2) / bety**2
        if (usrng)  then
          l = lq(lbmrs-3) + mwflt * (nrc-1) + 1
          call ucopy(fxbeam * sqrt(sx2), q(l), mwflt)
          l = lq(lbmrs-4) + mwflt * (nrc-1) + 1
          call ucopy(fybeam * sqrt(sy2), q(l), mwflt)
          l = lq(lbmrs-5) + mwflt * (nrc-1) + 1
          call ucopy(sqrt(sx2d), q(l), mwflt)
          l = lq(lbmrs-6) + mwflt * (nrc-1) + 1
          call ucopy(sqrt(sy2d), q(l), mwflt)
        endif
        if (synrad)  then
          if (elang .ne. zero .or. elak1 .ne. zero)  then
            if( elang .eq. zero)  then
              rhob = one / (abs(elak1) * sqrt(sx2 + sy2))
              snbq = pfacnq * p1d3 / qelect
            else
              rhob = eleng / abs(elang)
              snbq = pfacnb * p1d3 / qelect
            endif
            ec = p15d5 * clight * hbar * vec(1)**3 / rhob
            pw = twothd * arad * amasc2 * vec(1)**4
     +           * min(beamis(54), beamis(55)) / rhob**2
            photon = p1dm6 * pw * snbq / ec**2
          else
            ec     = zero
            rhob   = zero
            pw     = zero
            photon = zero
          endif
          if (usrng)  then
            gamax = (one + alfx**2) / betx**2
            gamay = (one + alfy**2) / bety**2
            sigxd = sqrt(sx2 * gamax + beamis(41) * disp(2)**2)
            sigyd = sqrt(sy2 * gamay)
            l = lq(lbmrs-5) + mwflt * (nrc-1) + 1
            call ucopy(sigxd, q(l), mwflt)
            l = lq(lbmrs-6) + mwflt * (nrc-1) + 1
            call ucopy(sigyd, q(l), mwflt)
            l = lq(lbmrs-7) + mwflt * (nrc-1) + 1
            call ucopy(rhob, q(l), mwflt)
            l = lq(lbmrs-8) + mwflt * (nrc-1) + 1
            call ucopy(ec, q(l), mwflt)
            l = lq(lbmrs-9) + mwflt * (nrc-1) + 1
            call ucopy(pw, q(l), mwflt)
            l = lq(lbmrs-10) + mwflt * (nrc-1) + 1
            call ucopy(photon, q(l), mwflt)
          endif
        endif
        if (clorb .and. usrng)  then
          l = lq(lbmrs-1) + mwflt * (nrc-1) + 1
          call ucopy(q(l), avbetx, mwflt)
          l = lq(lbmrs-2) + mwflt * (nrc-1) + 1
          call ucopy(q(l), avbety, mwflt)
          l = lq(lbmrs-11) + mwflt * (nrc-1) + 1
          call ucopy(q(l), bxbx, mwflt)
          l = lq(lbmrs-12) + mwflt * (nrc-1) + 1
          call ucopy(q(l), bxby, mwflt)
          if (elang .ne. zero .or. elak1 .ne. zero)  then
            if (elang .eq. zero)  then
*--- quadrupole
              pqx = abs(qfacx * sqrt(avbetx))
              pqy = abs(qfacy * sqrt(avbety))
              pbx = abs(bfacx * sqrt(avbetx))
              pby = abs(bfacy * sqrt(avbety))
            elseif (elak1 .eq. zero)  then
*--- normal bending magnet (K = 0)
              pqx = abs(qfacx * sqrt(bxbx))
              pqy = abs(qfacy * sqrt(bxby))
              pbx = abs(bfacx * sqrt(bxbx))
              pby = abs(bfacy * sqrt(bxby))
            else
*--- gradient magnet (K # 0)
              pqx = abs(qfacx * sqrt(avbetx))
              pqy = abs(qfacy * sqrt(avbety))
              pbx = abs(bfacx * sqrt(bxbx))
              pby = abs(bfacy * sqrt(bxby))
            endif
            l = lq(lbmrs-13) + mwflt * (nrc-1) + 1
            call ucopy(pqx, q(l), mwflt)
            l = lq(lbmrs-14) + mwflt * (nrc-1) + 1
            call ucopy(pqy, q(l), mwflt)
            l = lq(lbmrs-15) + mwflt * (nrc-1) + 1
            call ucopy(pbx, q(l), mwflt)
            l = lq(lbmrs-16) + mwflt * (nrc-1) + 1
            call ucopy(pby, q(l), mwflt)
          endif
        endif
   10 continue
      if (tousch)  then
        if (ierr .ne. 0)  then
          call aawarn('BMTALL', 1,
     +    'Touschek integral - too high accuray, approximation taken.')
        endif
        temp = isup * sum / (eight * sqrt(pi) * beamis(67) * circ)
        beamis(75) = (vec(1) * beamis(51))**2 * bunch
     +  / (four * arad**2 * clight * temp * min(beamis(56), beamis(57)))
      endif
 
      end