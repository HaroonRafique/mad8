      subroutine bmfdz2(f, fconst, a, b, eps, itermx, xz, ierr)
      implicit none
************************************************************************
*
*     Zero finder (straight copy of BMFDZ1 because FORTRAN does not
*     allow recursive calls).
*
*--- input
*    F         function of which zero is to be found, F = F(X, FCONST)
*    FCONST    constants passed to F
*    A         lower limit of search interval
*    B         upper limit of search interval
*    EPS       abs. max. of F required
*    ITERMX    max. no. of iterations
*--- output
*    XZ        zero of F if IERR = 0
*    IERR      0: OK
*              1: F(A, FCONST) * F(B, FCONST) > 0, XZ set to A
*              2: max. no. of iteration reached, XZ best value
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
 
      integer ierr,iter,itermx
      double precision a,b,ca,cb,cc,ee,eps,epsi,f,f1,f2,f3,fa,fb,fconst,
     +ff,fx,r,u1,u2,u3,u4,x1,x2,x3,xa,xb,xx,xz
 
      external  f
 
      dimension fconst(*)
 
      epsi = max(eps, p1dm15)
      xa = min(a, b)
      xb = max(a, b)
      fa = f(xa, fconst)
      fb = f(xb, fconst)
      if (fa * fb .gt. zero) then
        xz = half * (a + b)
        ierr = 1
        goto 999
      endif
      ierr = 0
      iter = 0
   10 continue
      xz = (xa + xb) / 2
      r = xz - xa
      ee = epsi * (abs(xz) + 1)
      if (r .le. ee) goto 999
      f1 = fa
      x1 = xa
      f2 = fb
      x2 = xb
   20 continue
      iter = iter + 1
      if (iter .gt. itermx) then
        ierr = 2
        goto 999
      endif
      fx = f(xz, fconst)
      if (fx * fa .gt. zero) then
        xa = xz
        fa = fx
      else
        xb = xz
        fb = fx
      endif
*--- parabola iteration
   30 continue
      u1 = f1 - f2
      u2 = x1 - x2
      u3 = f2 - fx
      u4 = x2 - xz
      if (u2 .eq. zero .or. u4 .eq. zero) go to 10
      f3 = fx
      x3 = xz
      u1 = u1 / u2
      u2 = u3 / u4
      ca = u1 - u2
      cb = (x1 + x2) * u2 - (x2 + xz) * u1
      cc = (x1 - xz) * f1 - x1 * (ca * x1 + cb)
      if (ca .eq. zero) then
        if (cb .eq. zero) go to 10
        xz = - cc / cb
      else
        u3 = cb / (2 * ca)
        u4 = u3**2 - cc / ca
        if (u4 .lt. zero) go to 10
        xz = - u3 + sign(sqrt(u4), xz + u3)
      endif
      if (xz .lt. xa .or. xz .gt. xb) go to 10
      r = min(abs(xz - x3), abs(xz - x2))
      ee = epsi * (abs(xz) + 1)
      if (r .gt. ee) then
        f1 = f2
        x1 = x2
        f2 = f3
        x2 = x3
        go to 20
      endif
      fx = f(xz, fconst)
      if (fx .eq. zero) goto 999
      if (fx * fa .lt. 0) then
        xx = xz - ee
        if (xx .le. xa) go to 999
        ff = f(xx, fconst)
        fb = ff
        xb = xx
      else
        xx = xz + ee
        if (xx .ge. xb) go to 999
        ff = f(xx, fconst)
        fa = ff
        xa = xx
      endif
      if (fx * ff .gt. 0) then
        iter = iter + 2
        if (iter .gt. itermx) then
          ierr = 2
          goto 999
        endif
        f1 = f3
        x1 = x3
        f2 = fx
        x2 = xz
        xz = xx
        fx = ff
        go to 30
      endif
 
  999 end
