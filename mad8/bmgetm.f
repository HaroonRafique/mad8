      subroutine bmgetm(nvar, x0, const, iter, xf, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Perform one or two parameter minimization                          *
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
 
      integer i,ierr,iter,nn,nvar
      double precision a11,a12,a22,b11,b12,b22,bmgx,bmgxxd,bmgxyd,bmgy,
     +bmgyxd,bmgyyd,c1,c2,const,det,deti,dp,ps2,ps2n,vx,vxx,vxy,vy,vyx,
     +vyy,x,x0,xf,xn
      dimension x0(2), xf(2), const(4)
      dimension x(2), xn(2), dp(2)
 
      ierr = 0
      nn = 0
      do 10 i = 1, 2
        x(i)  = x0(i)
        xf(i) = x0(i)
   10 xn(i) = x0(i)
      if (nvar .eq. 1)  then
*--- minimize for second vcariable only (psi)
        vy = bmgy(xn, const)
        ps2 = vy**2
   20   continue
        nn   = nn + 1
        x(2) = xn(2)
        vyy = bmgyyd(x, const)
        if (abs(vyy) .lt. p1dm9) then
          ierr = 1
          goto 999
        endif
        xn(2) = x(2) - vy / vyy
        vy = bmgy(xn, const)
        ps2n  = vy**2
        if (nn .lt. iter .and. ps2n .lt. ps2) then
          ps2  = ps2n
          x(2) = xn(2)
          goto 20
        elseif (nn .eq. iter) then
          x(2) = xn(2)
        endif
        xf(2) = x(2)
      else
*--- minimize for both variables
        vx = bmgx(x, const)
        vy = bmgy(x, const)
        ps2 = vx**2 + vy**2
   30   continue
        nn  = nn + 1
        vxx = bmgxxd(x, const)
        vxy = bmgxyd(x, const)
        vyx = bmgyxd(x, const)
        vyy = bmgyyd(x, const)
        a11 = vxx**2 + vxy**2
        a12 = vxx * vyx + vxy * vyy
        a22 = vyx**2 + vyy**2
        det = a11 * a22 - a12**2
        if (abs(det)*p1d9 .lt. max(abs(a11), abs(a22), abs(a12))) then
 
          ierr = 2
          goto 999
        endif
        deti  = one / det
        b11   = a22 * deti
        b12   = -a12 * deti
        b22   = a11 * deti
        c1    = -(vxx * vx + vyx * vy)
        c2    = -(vyx * vx + vyy * vy)
        dp(1) = b11 * c1 + b12 * c2
        dp(2) = b12 * c1 + b22 * c2
        do 40 i = 1, 2
   40   xn(i) = x(i) + dp(i)
        vx = bmgx(xn, const)
        vy = bmgy(xn, const)
        ps2n = vx**2 + vy**2
        if (nn .lt. iter .and. ps2n .lt. ps2) then
          ps2 = ps2n
          do 50 i = 1, 2
   50     x(i) = xn(i)
          goto 30
        elseif (nn .eq. iter) then
          do 60 i = 1, 2
   60     x(i) = xn(i)
        endif
        do 70 i = 1, 2
   70   xf(i) = x(i)
      endif
 
  999 end
