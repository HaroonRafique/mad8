      subroutine tmsect(fsec, el, h, dh, sk1, sk2, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for a sector dipole without fringe fields.           *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   EL        (real)    Length of the magnet body.                     *
*   H         (real)    Reference curvature of magnet.                 *
*   DH        (real)    Dipole field error.                            *
*   SK1       (real)    Quadrupole strength.                           *
*   SK2       (real)    Sextupole strengh.                             *
* Output:                                                              *
*   EK(6)     (real)    Kick due to dipole.                            *
*   RE(6,6)   (real)    Transfer matrix.                               *
*   TE(6,6,6) (real)    Second order terms.                            *
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
      double precision bi,bi2,bi2gi2,c1,c2,c3,c4,cg0,cg1,cg2,ch0,ch1,
     +ch2,cm,cp,cx,cy,cyy,dd,dh,difsq,dm,dp,dx,dyy,ek,el,fm,fp,fx,fyy,
     +gx,h,h2,hx,re,s1,s2,s3,s4,sk1,sk2,sm,sp,sumsq,sx,sy,syy,t1,t116,
     +t126,t166,t2,t216,t226,t266,t336,t346,t436,t446,t5,t516,t526,t566,
     +te,xk,xkl,xklsq,xksq,xs6,y0,y1,y2,y2klsq,y2ksq,yk,ykl,yklsq,yksq,
     +ys2,zc,zd,zf,zs
      logical           fsec
      dimension         ek(6), re(6,6), te(6,6,6)
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
      parameter         (c1  =   1.0d0,
     +                   c2  =   1.0d0 /      2.0d0,
     +                   c3  =   1.0d0 /     24.0d0,
     +                   c4  =   1.0d0 /    720.0d0)
      parameter         (s1  =   1.0d0,
     +                   s2  =   1.0d0 /      6.0d0,
     +                   s3  =   1.0d0 /    120.0d0,
     +                   s4  =   1.0d0 /   5040.0d0)
      parameter         (cg0 =   1.0d0 /     20.0d0,
     +                   cg1 =   5.0d0 /    840.0d0,
     +                   cg2 =  21.0d0 /  60480.0d0)
      parameter         (ch0 =   1.0d0 /     56.0d0,
     +                   ch1 =  14.0d0 /   4032.0d0,
     +                   ch2 = 147.0d0 / 443520.0d0)
 
      bi = 1.0 / betas
      bi2 = bi * bi
      bi2gi2 = 1.0 / (betas * gammas) ** 2
 
*---- Initialize.
      call uzero(ek, 1, 6*mwflt)
      call m66one(re)
 
*---- Horizontal.
      xksq = h**2 + sk1
      xk = sqrt(abs(xksq))
      xkl = xk * el
      xklsq = xksq * el**2
      if (abs(xklsq) .lt. 1.0e-2) then
        cx = (c1 - xklsq * (c2 - xklsq*c3))
        sx = (s1 - xklsq * (s2 - xklsq*s3)) * el
        dx = (c2 - xklsq * (c3 - xklsq*c4)) * el**2
        fx = (s2 - xklsq * (s3 - xklsq*s4)) * el**3
        gx = (cg0 - xklsq * (cg1 - xklsq*cg2)) * el**5
        hx = (ch0 - xklsq * (ch1 - xklsq*ch2)) * el**7
      else
        if (xklsq .gt. 0.0) then
          cx = cos(xkl)
          sx = sin(xkl) / xk
        else
          cx = cosh(xkl)
          sx = sinh(xkl) / xk
        endif
        dx = (1.0 - cx) / xksq
        fx = (el  - sx) / xksq
        gx = (3.0*el - sx*(4.0-cx)) / (2.0*xksq**2)
        hx = (15.0*el - sx*(22.0-9.0*cx+2.0*cx**2)) / (6.0*xksq**3)
      endif
      re(1,1) = cx
      re(1,2) = sx
      re(1,6) = h * dx * bi
      re(2,1) = - xksq * sx
      re(2,2) = cx
      re(2,6) = h * sx * bi
      re(5,2) = - re(1,6)
      re(5,1) = - re(2,6)
      re(5,6) = el*bi2gi2 - h**2*fx*bi2
      ek(1) = - dh*dx
      ek(2) = - dh*sx
      ek(5) =   h*dh*fx*bi + el*dtbyds
 
*---- Vertical.
      yksq = - sk1
      yk = sqrt(abs(yksq))
      ykl = yk*el
      yklsq = yksq*el**2
      if (abs(yklsq) .lt. 1.0e-2) then
        cy = (c1 - yklsq * (c2 - yklsq*c3))
        sy = (s1 - yklsq * (s2 - yklsq*s3)) * el
      else if (yklsq .gt. 0.0) then
        cy = cos(ykl)
        sy = sin(ykl) / yk
      else
        cy = cosh(ykl)
        sy = sinh(ykl) / yk
      endif
      re(3,3) = cy
      re(3,4) = sy
      re(4,3) = - yksq * sy
      re(4,4) = cy
      ek(3)   = 0.0
      ek(4)   = 0.0
 
*---- Second-order terms.
      if (fsec) then
        call uzero(te, 1, 216*mwflt)
 
*---- Pure horizontal terms.
        xs6 = (sk2 + 2.0*h*sk1) / 6.0
        ys2 = (sk2 +     h*sk1) / 2.0
        h2 = h / 2.0
        t116 = xs6 * (3.0*sx*fx - dx**2) - h * sx**2
        t126 = xs6 * (sx*dx**2 - 2.0*cx*gx) - h * sx * dx
        t166 = xs6 * (dx**3 - 2.0*sx*gx) - h2 * dx**2
        t216 = xs6 * (3.0*cx*fx + sx*dx)
        t226 = xs6 * (3.0*sx*fx + dx**2)
        t266 = xs6 * (sx*dx**2 - 2.0*cx*gx)
        t516 = h * xs6 * (3.0*dx*fx - 4.0*gx) +
     +         (sk1/2.0) * (fx + sx*dx)
        t526 = h * xs6 * (dx**3 - 2.0*sx*gx) + (sk1/2.0) * dx**2
        t566 = h * xs6 * (3.0*hx - 2.0*dx*gx) +
     +         (sk1/2.0) * gx - fx
        t1 = (sk1/2.0) * (dx**2 - sx*fx) - dx
        t2 = (sk1/2.0) * (el*dx - fx)
        t5 = fx - sk1 * (gx - fx*dx / 2.0)
        te(1,1,1) = - xs6 * (sx**2 + dx) - h2*xksq*sx**2
        te(1,1,2) = (- xs6*dx + h2*cx) * sx
        te(1,2,2) = (- xs6*dx + h2*cx) * dx
        te(1,1,6) = (- h2*t116 + (sk1/4.0)*el*sx) * bi
        te(1,2,6) = (- h2*t126 + (sk1/4.0) * (el*dx - fx) - sx/2.0) * bi
        te(1,6,6) = (- h**2*t166 + h*t1) * bi2 - h2 * dx * bi2gi2
        te(2,1,1) = - xs6 * (1.0 + 2.0*cx) * sx
        te(2,1,2) = - xs6 * (1.0 + 2.0*cx) * dx
        te(2,2,2) = - (2.0*xs6*dx + h2) * sx
        te(2,1,6) = (- h2*t216 - (sk1/4.0) * (sx - el*cx)) * bi
        te(2,2,6) = (- h2*t226 + (sk1/4.0) * el * sx) * bi
        te(2,6,6) = (- h**2*t266 + h*t2) * bi2 - h2 * sx * bi2gi2
        te(5,1,1) = (h2*xs6 * (sx*dx + 3.0*fx) -
     +              (sk1/4.0) * (el - cx*sx)) * bi
        te(5,1,2) = (h2*xs6*dx**2 + (sk1/4.0)*sx**2) * bi
        te(5,2,2) = (h*xs6*gx - sk1 * (fx - sx*dx) / 4.0 - sx/2.0) * bi
        te(5,1,6) = h2 * ((t516 - sk1 * (el*dx - fx) / 2.0) * bi2 +
     +                    sx * bi2gi2)
        te(5,2,6) = h2 * ((t526 - sk1 * (dx**2 - sx*fx) / 2.0) * bi2 +
     +                    dx * bi2gi2)
        te(5,6,6) = (h**2 * (t566 + t5) * bi2 +
     +              (3.0/2.0) * (h**2*fx - el) * bi2gi2) * bi
 
*---- Mixed terms.
        y2ksq = 4.0 * yksq
        call tmfoc(el, y2ksq, cyy, syy, dyy, fyy)
        y2klsq = y2ksq * el**2
        if (max(abs(y2klsq),abs(xklsq)) .le. 1.0e-2) then
          y0 = 1.0
          y1 = xklsq + y2klsq
          y2 = xklsq**2 + xklsq*y2klsq + y2klsq**2
          zc = (y0 - (y1 - y2 / 30.0) / 12.0) * el**2 /   2.0
          zs = (y0 - (y1 - y2 / 42.0) / 20.0) * el**3 /   6.0
          zd = (y0 - (y1 - y2 / 56.0) / 30.0) * el**4 /  24.0
          zf = (y0 - (y1 - y2 / 72.0) / 42.0) * el**5 / 120.0
        else if (xksq .le. 0.0  .or.  yksq .le. 0.0) then
          dd = xksq - y2ksq
          zc = (cyy - cx) / dd
          zs = (syy - sx) / dd
          zd = (dyy - dx) / dd
          zf = (fyy - fx) / dd
        else
          sumsq = (xk/2.0 + yk) ** 2
          difsq = (xk/2.0 - yk) ** 2
          call tmfoc(el, sumsq, cp, sp, dp, fp)
          call tmfoc(el, difsq, cm, sm, dm, fm)
          zc = sp * sm / 2.0
          zs = (sp*cm - cp*sm) / (4.0*xk*yk)
          if (xksq .gt. y2ksq) then
            zd = (dyy - zc) / xksq
            zf = (fyy - zs) / xksq
          else
            zd = (dx - zc) / y2ksq
            zf = (fx - zs) / y2ksq
          endif
        endif
        t336 = sk2 * (cy*zd - 2.0*sk1*sy*zf) + h * sk1 * fx * sy
        t346 = sk2 * (sy*zd - 2.0*cy*zf) + h * fx * cy
        t436 = 2.0 * ys2 * fx * cy - sk2 * sk1 * (sy*zd - 2.0*cy*zf)
        t446 = 2.0 * ys2 * fx * sy - sk2 * (cy*zd - 2.0*sk1*sy*zf)
        te(1,3,3) = + sk2*sk1*zd + ys2*dx
        te(1,3,4) = + sk2*zs/2.0
        te(1,4,4) = + sk2*zd - h2*dx
        te(2,3,3) = + sk2*sk1*zs + ys2*sx
        te(2,3,4) = + sk2*zc/2.0
        te(2,4,4) = + sk2*zs - h2*sx
        te(3,1,3) = + sk2*(cy*zc/2.0 - sk1*sy*zs) + h2*sk1*sx*sy
        te(3,1,4) = + sk2*(sy*zc/2.0 - cy*zs) + h2*sx*cy
        te(3,2,3) = + sk2*(cy*zs/2.0 - sk1*sy*zd) + h2*sk1*dx*sy
        te(3,2,4) = + sk2*(sy*zs/2.0 - cy*zd) + h2*dx*cy
        te(3,3,6) = (h2*t336 - sk1*el*sy/4.0) * bi
        te(3,4,6) = (h2*t346 - (sy + el*cy) / 4.0) * bi
        te(4,1,3) = sk2*sk1*(cy*zs - sy*zc/2.0) + ys2*sx*cy
        te(4,1,4) = sk2*(sk1*sy*zs - cy*zc/2.0) + ys2*sx*sy
        te(4,2,3) = sk2*sk1*(cy*zd - sy*zs/2.0) + ys2*dx*cy
        te(4,2,4) = sk2*(sk1*sy*zd - cy*zs/2.0) + ys2*dx*sy
        te(4,3,6) = (h2*t436 + sk1 * (sy - el*cy) / 4.0) * bi
        te(4,4,6) = (h2*t446 - sk1*el*sy/4.0) * bi
        te(5,3,3) = (- h*sk2*sk1*zf - h*ys2*fx + sk1*(el-cy*sy)/4.0)*bi
        te(5,3,4) = (- h*sk2*zd/2.0 - sk1*sy**2/4.0) * bi
        te(5,4,4) = (- h*sk2*zf + h*h2*fx - (el + sy*cy)/4.0) * bi
        call tmsymm(te)
 
*---- Effect of dipole error.
        if (dh .ne. 0.0) then
          re(1,1) = re(1,1) + dh * t116
          re(1,2) = re(1,2) + dh * t126
          re(1,6) = re(1,6) + dh * (2.0*h*t166 - t1) * bi
          re(2,1) = re(2,1) + dh * (t216 - h*sx)
          re(2,2) = re(2,2) + dh * t226
          re(2,6) = re(2,6) + dh * (2.0*h*t266 - t2) * bi
          re(5,1) = re(5,1) - dh * t516 * bi
          re(5,2) = re(5,2) - dh * (t526 - dx) * bi
          re(5,6) = re(5,6) -
     +              dh * h * ((2.0*t566 + t5) * bi2 + fx * bi2gi2)
          re(3,3) = re(3,3) - dh * t336
          re(3,4) = re(3,4) - dh * t346
          re(4,3) = re(4,3) - dh * t436
          re(4,4) = re(4,4) - dh * t446
          ek(1) = ek(1) - dh**2 * t166
          ek(2) = ek(2) - dh**2 * t266
          ek(5) = ek(5) + dh**2 * t566 * bi
        endif
      endif
 
      end
