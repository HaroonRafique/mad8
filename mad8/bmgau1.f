      function bmgau1(fuser, vec, a, b, eps, ierr)
      implicit none
************************************************************************
*     This is a (modified) copy of DGAUSS, renamed BMGAU1. Necessary
*     because of single / double precision.
*
*     Adaptive Gaussian quadrature.
*
*     BMGAU1 is set equal to the approximate value of the integral of
*     the function FUSER over [A,B], with accuracy parameter EPS.
*     VEC contains the additional user parameters for FUSER.
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
 
      integer i,ierr
      double precision a,aa,b,bb,bmgau1,c1,c2,const,eps,fuser,s16,s8,u,
     +vec,w,x
      dimension vec(*)
      dimension w(12), x(12)
 
      external fuser
 
      data w / 0.10122 85362 90376 259d0,
     1         0.22238 10344 53374 471d0,
     2         0.31370 66458 77887 287d0,
     3         0.36268 37833 78361 983d0,
     4         0.27152 45941 17540 949d-1,
     5         0.62253 52393 86478 929d-1,
     6         0.95158 51168 24927 848d-1,
     7         0.12462 89712 55533 872d0,
     8         0.14959 59888 16576 732d0,
     9         0.16915 65193 95002 538d0,
     a         0.18260 34150 44923 589d0,
     b         0.18945 06104 55068 496d0/
      data x / 0.96028 98564 97536 232d0,
     1         0.79666 64774 13626 740d0,
     2         0.52553 24099 16328 986d0,
     3         0.18343 46424 95649 805d0,
     4         0.98940 09349 91649 933d0,
     5         0.94457 50230 73232 576d0,
     6         0.86563 12023 87831 744d0,
     7         0.75540 44083 55003 034d0,
     8         0.61787 62444 02643 748d0,
     9         0.45801 67776 57227 386d0,
     a         0.28160 35507 79258 913d0,
     b         0.95012 50983 76374 402d-1/
************************************************************************
 
      bmgau1 = zero
      ierr = 0
      if (b .eq. a) goto 999
      const = p5dm3 / (b - a)
      bb = a
*--- loop
   10 aa = bb
      bb = b
   20 c1 = half * (bb + aa)
      c2 = half * (bb - aa)
      s8 = zero
      do 30 i = 1, 4
        u = c2 * x(i)
        s8 = s8 + w(i) * (fuser(c1 + u, vec) + fuser(c1 - u, vec))
   30 continue
      s8 = c2 * s8
      s16 = zero
      do 40 i = 5, 12
        u = c2 * x(i)
        s16 = s16 + w(i) * (fuser(c1 + u, vec) + fuser(c1 - u, vec))
   40 continue
      s16 = c2 * s16
      if ( abs(s16 - s8) .gt. eps * (1. + abs(s16)) ) then
        if ( one + abs(const * c2) .eq. one) then
          bmgau1 = zero
          ierr = 1
          goto 999
        endif
        bb = c1
        goto 20
      endif
      bmgau1 = bmgau1 + s16
      if (bb .ne. b) go to 10
 
  999 end
