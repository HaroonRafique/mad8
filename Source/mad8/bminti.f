      subroutine bminti
      implicit none
************************************************************************
*
*     Returns beam integrals in BEAMIS, common /BMPMRL/ (+CA BMPMCM)
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
      integer i,ierr,iret,l,nrc
      double precision a,a1,a2,a3,a4,a5,alph1x,alph1y,angle,arg,avbetx,
     +avbety,b,b2avs,b3avs,bav32x,bav32y,bbarx,bbary,beta0d,beta1d,
     +bgrad,bmbetv,bmetab,bmgau1,bmhavr,brac,bxbyav,c,cangle,carg,den,
     +eta1d,eta2,etap,fal,feta,fh,fneta,gam,gam1x,gam1y,gamx,gamy,ph1,
     +ph2,rho,rhoinv,rk,rk2,rk3,sangle,sarg,sig,tang,term1,term2,term3,
     +tph1,tph2,vec,w,y,z
 
      dimension vec(5)
 
      logical wedge, usrng, bmusrg
 
      external bmbetv
 
*--- statement functions
 
      b2avs(a,b,c) = a**2 + eleng * (-a * b
     +               + eleng * ((two * a * c + b**2) / three
     +               + eleng * (-half * b * c + c**2 * eleng
     +               / five)))
 
      b3avs(a,b,c) = a**3 + eleng * (-half * three * a**2 * b
     +               + eleng * (a * (a * c + b**2)
     +               + eleng * (-b * (six * a * c + b**2) / four
     +               + eleng * (three * c * (a * c + b**2) / five
     +               + eleng * c**2 * (-half * b
     +               + eleng * c / seven)))))
 
      bmetab(a,b) = a * sangle / angle
     +              + sig * b * (one - cangle) / (angle * rk)
     +              + sig * (angle - sangle) / (rho * angle * rk2)
 
*--- reset count for user selected print range
      nrc = 0
 
      do 20 i = 1, nbmelm
        call bmeget(i, iret)
*--- count user range
        usrng = bmusrg(i, ilorng, ihirng, iflgbm)
        if (usrng)  nrc = nrc + 1
        wedge  = chtype(:5) .eq. 'SBEND'
        if (elang * eleng .ne. zero)  then
          rhoinv = elang / eleng
        else
          rhoinv = zero
        endif
        if (elang .ne. zero) then
          rho   = one / rhoinv
          bgrad = elak1 * rho**2
          if (wedge) then
            ph1 = ele1d
            ph2 = ele2d
          else
            ph1 = elang * half + ele1d
            ph2 = elang * half + ele2d
          endif
          tph1 = tan(ph1)
          tph2 = tan(ph2)
          rk2  = (one - bgrad) * rhoinv**2
*--- keep sign of 1 - n
          sig    = sign(one, rk2)
          rk2 = abs(rk2)
          rk  = sqrt(rk2)
          rk3 = rk * rk2
          angle  = rk * eleng
          if (sig .lt. zero)  then
            sangle = sinh(angle)
            cangle = cosh(angle)
          else
            sangle = sin(angle)
            cangle = cos(angle)
          endif
          beta0d = - two * alfx
          if (wedge) then
*---  Formulae for wedge (sector) magnets
*     -----------------------------------
            feta  = bmetab(disp(1), disp(2))
            fh    = bmhavr(disp(1), disp(2), betx, beta0d, eleng,
     +              rho, sig, angle, sangle, cangle, rk2, rk3)
            fneta = feta * bgrad * rhoinv**3
          else
*---  Formulae for non-normal boundary magnet (rectangular)
*     -----------------------------------------------------
            eta1d = disp(2) + disp(1) * tph1 * rhoinv
            eta2  = disp(1) * cangle + eta1d * sangle / rk + sig
     +      * (one - cangle) * rhoinv / rk2
            beta1d = beta0d + two * betx * tph1 * rhoinv
            feta  = bmetab(disp(1), eta1d)
            fh    = bmhavr(disp(1), eta1d, betx, beta1d, eleng,
     +              rho, sig, angle, sangle, cangle, rk2, rk3)
            fneta = feta * bgrad * rhoinv**3 + (disp(1) * tph1 + eta2
     +      * tph2) / (two * eleng * rho**2)
          endif
 
          term1 = eleng * feta * rhoinv
          term2 = eleng * rhoinv**2
          term3 = eleng * rhoinv**3
          beamis(1) = beamis(1) + term1
          beamis(2) = beamis(2) + term2
          beamis(3) = beamis(3) + abs(term3)
          beamis(4) = beamis(4) + term1 * rhoinv**2
     +                - two * eleng * fneta
          beamis(5) = abs(term3) * fh + beamis(5)
          beamis(15) = beamis(15) + term3
          if (clorb)  then
*--- calculate sums for closed orbit amp. factors (BMTALL)
            fal = rhoinv * tan (half * elang)
            alph1x = alfx - betx * fal
            alph1y = alfy + bety * fal
            gam1x  = (one + alph1x**2) / betx
            gam1y  = (one + alph1y**2) / bety
            bbarx  = betx - eleng * (alph1x - gam1x * eleng / three )
            bbary  = bety - eleng * (alph1y - gam1y * eleng / three )
            beamis(79) = beamis(79) + bbarx * elang**2
            beamis(80) = beamis(80) + bbary * elang**2
            if (usrng)  then
*--- store BBAR in bank
              l = lq(lbmrs-11) + mwflt * (nrc-1) + 1
              call ucopy(bbarx, q(l), mwflt)
              l = lq(lbmrs-12) + mwflt * (nrc-1) + 1
              call ucopy(bbary, q(l), mwflt)
            endif
          endif
*--- end of IF (ELANG .NE. ZERO)  THEN
        endif
 
        if (elak1 .ne. zero ) then
*     Calculate integrals I6X  &  I6Y using <BX> & <BY> for quadrupoles
*     Calculate integrals I8X  &  I8Y using <BX> & <BY> for quadrupoles
*     Calculate dJ/ddelta
*     -----------------------------------------------------------------
 
*--- horizontal
 
          gam = (one + alfx**2) / betx
          rk2 = rhoinv**2 - elak1
          sig = sign(one, rk2)
          rk = sqrt(abs(rk2))
          arg   = eleng * rk
          angle = two * arg
          if (wedge)  then
            tang = ele1d
          else
            tang = half * elang + ele1d
          endif
          etap = disp(2) + disp(1) * tan(tang) * rhoinv
          if (rk2 .ge. zero ) then
*     KH>0
*     ----
            sangle = sin(angle)
            cangle = cos(angle)
            sarg = sin(arg)
            carg = cos(arg)
          else
*     KH<0
*     ----
            sangle = sinh(angle)
            cangle = cosh(angle)
            sarg = sinh(arg)
            carg = cosh(arg)
          endif
*--- first dJ/ddelta etc.
          w = etap / rk
          y = rhoinv / rk2
          z = disp(1) - y
          a1 = two * w * y
          a2 = two * y * z
          a3 = two * w * z
          brac = sig * a1
     +    + half * arg * (sig * w**2 + z**2 + two * y**2)
     +    - sig * a1 * carg + sarg
     +    * (a2 + half * ((z**2 - sig * w**2) * carg + a3 * sarg))
*--- multiply integral by K**2 and the factor from ds = darg / sk
          beamis(16) = beamis(16) + elak1**2 * brac / rk
*--- now the other integrals
          den = two * rk2 * angle
          avbetx = (betx * rk2 * (sangle + angle) - two * alfx *
     +    rk * ( one - cangle) + gam * (angle - sangle)) / den
          if (usrng)  then
*--- store average betx in bank
            l = lq(lbmrs-1) + mwflt * (nrc-1) + 1
            call ucopy(avbetx, q(l), mwflt)
          endif
          beamis(6) = beamis(6) + rk2**2 * eleng * avbetx
*--- store constant arguments for BMBETV
          vec(1) = rk
          vec(2) = rk2
          vec(3) = betx
          vec(4) = alfx
          vec(5) = gam
          bav32x = bmgau1(bmbetv, vec, zero , eleng, p1dm2, ierr)
     +             / eleng
          if (ierr .ne. 0)  then
            call aawarn('BMINTI', 1,
     +      'BMGAU1: Too high accuray, approximation taken.')
          endif
          beamis(11) = beamis(11) + abs(rk2**3) * eleng * bav32x
 
*--- vertical
 
          gam = (one + alfy**2) / bety
          rk2 = elak1
          rk = sqrt(abs(rk2))
          angle = two * eleng * rk
          if (rk2 .ge. zero ) then
*     KV>0
*     ----
            sangle = sin(angle)
            cangle = cos(angle)
          else
*     KV<0
*     ----
            sangle = sinh(angle)
            cangle = cosh(angle)
          endif
          den = two * rk2 * angle
          avbety = (bety * rk2 * (sangle + angle) - two * alfy *
     +    rk * ( one - cangle) + gam * (angle - sangle)) / den
          if (usrng)  then
*--- store average bety in bank
            l = lq(lbmrs-2) + mwflt * (nrc-1) + 1
            call ucopy(avbety, q(l), mwflt)
          endif
          beamis(7) = beamis(7) + rk2**2 * eleng * avbety
          if (clorb)  then
*--- calculate sums for closed orbit amp. factors (BMTALL)
            beamis(77) = beamis(77) + avbetx * (elak1 * eleng)**2
            beamis(78) = beamis(78) + avbety * (elak1 * eleng)**2
          endif
*--- store constant arguments for BMBETV
          vec(1) = rk
          vec(2) = rk2
          vec(3) = bety
          vec(4) = alfy
          vec(5) = gam
          bav32y = bmgau1(bmbetv, vec, zero , eleng, p1dm2, ierr)
     +             / eleng
          if (ierr .ne. 0)  then
            call aawarn('BMINTI', 1,
     +      'BMGAU1: Too high accuray, approximation taken.')
          endif
          beamis(12) = beamis(12) + abs(rk2**3) * eleng * bav32y
        endif
*--- end of IF (ELAK1 .NE. ZERO)  THEN
 
        if (elak2 .ne. zero ) then
 
*     Calculate integrals I7X,I7Y,I9X & I9Y using <B> for sextupoles
*     --------------------------------------------------------------
          beamis(8) = beamis(8) + eleng * elak2**2
     +    * b2avs(betx, two * alfx, (one + alfx**2) / betx)
          beamis(13) = beamis(13) + eleng * abs(elak2**3)
     +    * b3avs(betx, two * alfx, (one + alfx**2) / betx)
          beamis(9) = beamis(9) + eleng * elak2**2
     +    * b2avs(bety, two * alfy, (one + alfy**2) / bety)
          beamis(14) = beamis(14) + eleng * abs(elak2**3)
     +    * b3avs(bety, two * alfy, (one + alfy**2) / bety)
 
*     Calculate integral I7XY using  <BX.BY>  for sextupoles
*     ------------------------------------------------------
          gamx = (one + alfx**2) / betx
          gamy = (one + alfy**2) / bety
          a1 = betx * bety
          a2 = alfx * bety + alfy * betx
          a3 = four * alfx * alfy + bety * gamx + betx * gamy
          a4 = gamx * alfy + gamy * alfx
          a5 = gamx * gamy
          bxbyav = a1 + eleng *(- a2 + eleng * (a3 / three
     +    + eleng * (- half * a4 + eleng * a5 / five)))
          beamis(10) = beamis(10) + eleng * elak2**2 * bxbyav
        endif
*--- end of IF (ELAK2 .NE. ZERO)  THEN
 
   20 continue
 
      do 30 i = 1, 16
   30 beamis(i) = beamis(i) * isup
      beamis(15) = p8d2 * beamis(15) / (five * sqrt(three) * beamis(3))
 
      end
