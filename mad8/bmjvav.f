      function bmjvav(s, vec)
      implicit none
************************************************************************
*
*    Returns the value of <J/V>
*
*--- input
*  S    (real)   independent variable
*  VEC  (real)   vector of auxiliary variables
*                (1) = GAM   (E / mc**2)
*                (2) = AP0D
*                (3) = COUPL
*                (4) = BMSIGE**2 (BEAMIS(41)**2)
*                (5) = E_x_0
*                (6) = SJXY  (BEAMIS(25) / BEAMIS(26))
*                (7) = SIGB  (BEAMIS(51))
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
      integer i,ierr,n
      double precision a,ajs,al,am11,am12,am13,am21,am22,am23,arg,as,
     +bmgau2,bmjofs,bmjvav,bot,brac,bs,bt,carg,delta,den,etads,etas,
     +gamma,gxs,h,rk,rk2,s,sarg,sigx,sigxd,sigy,sqd,vec,vec2
 
      dimension vec(7), vec2(2), as(2), bs(2)
 
      external bmjofs
 
      do 10  i = 1, 2
        if (i .eq. 1)  then
*--- horizontal
          al  = alfx
          bt  = betx
          rk2 = (elang / s)**2 - elak1
        else
*--- vertical
          al  = alfy
          bt  = bety
          rk2 = elak1
        endif
        gamma = (one + al**2) / bt
        if (abs(rk2) .lt. p1dm8)  then
          am11 = one
          am12 = s
          am13 = half * elang * s
          am21 = zero
          am22 = one
          am23 = elang
        else
          rk = sqrt(abs(rk2))
          arg  = s * rk
          if (rk2 .lt. zero)  then
            carg = cosh(arg)
            sarg = sinh(arg)
          else
            carg = cos(arg)
            sarg = sin(arg)
          endif
          am11 = carg
          am12 = sarg / rk
          am13 = (one - carg) * elang / (s * rk2)
          am21 = - sarg * sign(rk, rk2)
          am22 = carg
          am23 = sarg * elang / (s * rk)
        endif
        as(i) = al * (am11 * am22 + am12 * am21) - bt * am11 * am21 -
     +  gamma * am12 * am22
        bs(i) = bt * am11**2 - two * al * am11 * am12 + gamma * am12**2
        if (i. eq. 1)  then
          etas  = disp(1) * am11 + vec(2) * am12 + am13
          etads = disp(1) * am21 + vec(2) * am22 + am23
        endif
   10 continue
      gxs = (one + as(1)**2) / bs(1)
      bot = one + vec(3)**2
      den = sqrt(bot)
      sigx = sqrt(abs(vec(5) * bs(1) / bot + etas**2 * vec(4)))
      sigy = vec(3) * sqrt(abs(vec(5) * bs(2) * vec(6))) / den
      sigxd = sqrt(abs(vec(5) * gxs / bot + etads**2 * vec(4)))
*--- if (gam * sigxd / sigb) > 100. then an approximation is used
*    for J(s):
      if (vec(1) * sigxd / vec(7) .gt. p1d2) then
        delta = sqrt(three) * vec(1) * sigxd
        sqd = sqrt(one + delta**2)
        brac = log(two / vec(7)) - p23d0 / four + half * log
     +  ((sqd - one ) / (sqd + one )) + (two / delta) * log(delta
     +  + sqd)
        ajs = sqd / delta + brac / (two * delta)
      else
*--- the correct formula is used, involving N INTEGRAL
        vec2(1) = vec(7)
        vec2(2) = sigxd * vec(1)
*--- set upper limit where J(s) < 0.0001
        a = one
        h = one
        n = 0
   20   if (bmjofs(a + h, vec) .gt. p1dm4)  then
          h = two * h
          n = n + 1
          goto 20
        endif
        do 30  i = 1, n
          h = half * h
          if (bmjofs(a + h, vec) .gt. p1dm4)  a = a + h
   30   continue
        a   = max(a, vec2(1)) + h
        ajs = bmgau2(bmjofs, vec2, vec2(1), a, p1dm4, ierr)
      endif
      bmjvav = ajs / (sigx * sigy)
 
      end
