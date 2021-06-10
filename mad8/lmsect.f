      subroutine lmsect(nord, el, h, dh, sk1, sk2, sk3, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for sector dipole.                               *
*   Excitation error DH is treated in a linear approximation.          *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Dipole length.                                 *
*   H         (real)    Curvature of reference orbit.                  *
*   DH        (real)    Curvature error (excitation error of K0).      *
*   SK1       (real)    Quadrupole strength.                           *
*   SK2       (real)    Sextupole strength.                            *
*   SK3       (real)    Octupole strength.                             *
* Output:                                                              *
*   FP, FM    (map)     Element map.                                   *
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
      integer nord
      double precision bi,bi2,c1,c2,c3,c4,cg0,cg1,cg2,ch0,ch1,ch2,cm,cp,
     +cx,cy,cyy,dd,dh,dif,dm,dp,dpt,dx,dyy,el,fm,fminus,fp,fplus,fx,fyy,
     +gx,h,hx,s1,s2,s3,s4,sk1,sk2,sk3,sm,sp,sum,sx,sy,syy,xk,xkl,xklsq,
     +xksq,xs6,y0,y1,y2,yk,ykl,yklsq,yksq,ys2,yyklsq,yyksq,zc,zd,zf,zs
      dimension         fp(*), fm(6,6)
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
 
*---- Horizontal focusing functions.
      xksq = h**2 + sk1
      xk  = sqrt(abs(xksq))
      xkl = xk * el
      xklsq = xksq * el**2
      if (abs(xklsq) .lt. 1.0e-2) then
        cx = (c1 - xklsq * (c2 - xklsq * c3))
        sx = (s1 - xklsq * (s2 - xklsq * s3)) * el
        dx = (c2 - xklsq * (c3 - xklsq * c4)) * el**2
        fx = (s2 - xklsq * (s3 - xklsq * s4)) * el**3
        gx = (cg0 - xklsq * (cg1 - xklsq * cg2)) * el**5
        hx = (ch0 - xklsq * (ch1 - xklsq * ch2)) * el**7
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
 
*---- Vertical focusing functions.
      yksq = - sk1
      yk  = sqrt(abs(yksq))
      ykl = yk * el
      yklsq = yksq * el**2
      if (abs(yklsq) .lt. 1.0e-2) then
        cy = (c1 - yklsq * (c2 - yklsq * c3))
        sy = (s1 - yklsq * (s2 - yklsq * s3)) * el
      else if (yklsq .gt. 0.0) then
        cy = cos(ykl)
        sy = sin(ykl) / yk
      else
        cy = cosh(ykl)
        sy = sinh(ykl) / yk
      endif
 
*---- Fill in matrix.
      call lmone(nord, fp, fm)
      fm(1,1) = + cx
      fm(1,2) = + sx
      fm(1,6) = dx * h / betas
      fm(2,1) = - sx * xksq
      fm(2,2) = + cx
      fm(2,6) = sx * h / betas
      fm(3,3) = + cy
      fm(3,4) = + sy
      fm(4,3) = - sy * yksq
      fm(4,4) = + cy
      fm(5,1) = - fm(2,6)
      fm(5,2) = - fm(1,6)
      fm(5,6) = - fx*(h/betas)**2 + el/(betas*gammas)**2
 
*---- Excitation error.
      fp(1) = - dh * sx
      fp(2) = - dh * dx
      fp(6) = - dh * fx * h / betas - el*dtbyds
 
*---- Radiation loss.
*     Loss for ideal orbit is assumed.
      if (dorad) then
        dpt = - (2.0 * arad * charge * gammas**3) * el * h**2 / 3.0
        fp(1) = fp(1) + 0.5 * dpt * fm(2,6)
        fp(2) = fp(2) - 0.5 * dpt * fm(1,6)
        fp(5) = dpt
        fp(6) = fp(6) - 0.5 * dpt * fm(5,6)
      endif
 
*---- Third order, pure horizontal terms.
      if (nord .ge. 3) then
        xs6 = (sk2 + 2.0*h*sk1) / 6.0
        bi = 1.0 / betas
        bi2 = bi**2
        fp(28) = - xs6*sx*(2.0 + cx**2)/3.0 - h*xksq**2*sx**3/6.0
        fp(29) = + xs6*dx*(1.0 + cx + cx**2) - h*xksq*sx**2*cx/2.0
        fp(34) = - xs6*sx**3 - h*sx*cx**2/2.0
        fp(49) = + xs6*dx**2*(2.0 + cx)/3.0 + h*(dx + sx**2*cx)/6.0
        fp(33) = bi*(- h*xs6*(xksq*gx + cx*sx*dx) + h**2*xksq*sx**3/2.0
     +               + sk1*(el - cx*sx)/4.0)
        fp(38) = bi*(h*xs6*dx**2*(1.0 + 2.0*cx) + h**2*sx**2*cx
     +             + sk1*sx**2/2.0)
        fp(53) = bi*(- h*xs6*(sx*dx**2 + gx)
     +         - h**2*(fx + sx*dx*(1.0+2.0*cx))/4.0 + (el + sx*cx)/4.0)
        fp(48) = h*bi2*(- h*xs6*(sx*dx**2 - gx - gx) - h**2*sx**3/2.0 -
     +  sk1*(fx + sx*dx)/2.0 - sx / (2.0*gammas**2))
        fp(63) = h*bi2*(h*xs6*dx**3 + h**2*sx**2*dx/2.0 - sx**2/2.0 +
     +  dx / (2.0*gammas**2))
        fp(83) = h**2*bi**3*(- h*xs6*hx - h**2*(sx*dx**2 + gx)/6.0 +
     +  (fx + sx*dx)/4.0 - fx / (2.0*gammas**2)) + el / (2.0*betas**3
     +  *gammas**2)
 
*---- Third order, coupling terms.
        yyksq = 4.0 * yksq
        call tmfoc(el, yyksq, cyy, syy, dyy, fyy)
        yyklsq = yyksq * el**2
        if (max(abs(yyklsq),abs(xklsq)) .le. 1.0e-2) then
          y0 = 1.0
          y1 = xklsq + yyklsq
          y2 = xklsq**2 + xklsq*yyklsq + yyklsq**2
          zc = (y0 - (y1 - y2 / 30.0) / 12.0) * el**2 /   2.0
          zs = (y0 - (y1 - y2 / 42.0) / 20.0) * el**3 /   6.0
          zd = (y0 - (y1 - y2 / 56.0) / 30.0) * el**4 /  24.0
          zf = (y0 - (y1 - y2 / 72.0) / 42.0) * el**5 / 120.0
        else if (xksq .le. 0.0 .or. yksq .le. 0.0) then
          dd = xksq - yyksq
          zc = (cyy - cx) / dd
          zs = (syy - sx) / dd
          zd = (dyy - dx) / dd
          zf = (fyy - fx) / dd
        else
          sum = (xk/2.0 + yk) ** 2
          dif = (xk/2.0 - yk) ** 2
          call tmfoc(el, sum, cp, sp, dp, fplus)
          call tmfoc(el, dif, cm, sm, dm, fminus)
          zc = sp * sm / 2.0
          zs = (sp*cm - cp*sm) / (4.0*xk*yk)
          if (xksq .gt. yyksq) then
            zd = (dyy - zc) / xksq
            zf = (fyy - zs) / xksq
          else
            zd = (dx - zc) / yyksq
            zf = (fx - zs) / yyksq
          endif
        endif
        ys2 = (sk2 + h*sk1) / 2.0
        fp(39) = + sk1*sk2*(xksq*sx*zd + cx*zs) + ys2*sx
        fp(40) = - sk2*(xksq*sx*zs + cx*zc)
        fp(43) = + sk2*(xksq*sx*zd + cx*zs) - h*sx/2.0
        fp(54) = + sk1*sk2*(cx*zd - sx*zs) - ys2*dx
        fp(55) = - sk2*(cx*zs - sx*zc)
        fp(58) = + sk2*(cx*zd - sx*zs) + h*dx/2.0
        fp(67) = (h*sk2*sk1*(zf + dx*zs - sx*zd) + h*ys2*fx - sk1*(el -
     +  sy*cy)/4.0) / betas
        fp(70) = (- h*sk2*(zd + dx*zc - sx*zs) - sk1*sy**2/2.0) / betas
        fp(76) = (h*sk2*(zf + dx*zs - sx*zd) - h**2*fx/2.0 + (el + sy
     +  *cy)/4.0) / betas
      endif
 
*---- Fourth order not yet available.
      end
