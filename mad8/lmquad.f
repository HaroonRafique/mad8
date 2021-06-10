      subroutine lmquad(nord, el, sk1, tilt, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Lie algebraic map for a quadrupole.                               *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Quadrupole length.                             *
*   SK1       (real)    Quadrupole strenth.                            *
*   TILT      (real)    Quadrupole tilt.                               *
* Output:                                                              *
*   FP, FM    (map)     Quadrupole map.                                *
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
      double precision c2x,c2y,cx,cy,ekx,ekxl,ekxl2,el,fm,fp,s2x,s2y,
     +sk1,sx,sy,tilt
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
 
*---- Linear terms.
      call lmone(nord, fp, fm)
      ekx = sqrt(abs(sk1))
      ekxl = ekx * el
      if (abs(ekxl) .lt. 1.0e-3) then
        ekxl2 = sk1 * el**2
        cx = (1.0 - ekxl2 / 2.0)
        sx = (1.0 - ekxl2 / 6.0) * el
        cy = (1.0 + ekxl2 / 2.0)
        sy = (1.0 + ekxl2 / 6.0) * el
      else if (sk1 .gt. 0.0) then
        cx = cos(ekxl)
        sx = sin(ekxl) / ekx
        cy = cosh(ekxl)
        sy = sinh(ekxl) / ekx
      else
        cx = cosh(ekxl)
        sx = sinh(ekxl) / ekx
        cy = cos(ekxl)
        sy = sin(ekxl) / ekx
      endif
      fm(1,1) = + cx
      fm(1,2) = + sx
      fm(2,1) = - sx * sk1
      fm(2,2) = + cx
      fm(3,3) = + cy
      fm(3,4) = + sy
      fm(4,3) = + sy * sk1
      fm(4,4) = + cy
      fm(5,6) = el/(betas*gammas)**2
      fp(6) = - el*dtbyds
 
*---- Third order terms.
      if (nord .ge. 3) then
        s2x = cx * sx
        s2y = cy * sy
        fp(33) = sk1 * (el - s2x) / (4.0 * betas)
        fp(38) = sk1 * sx**2 / (2.0 * betas)
        fp(53) = (el + s2x) / (4.0 * betas)
        fp(67) = - sk1 * (el - s2y) / (4.0 * betas)
        fp(70) = - sk1 * sy**2 / (2.0 * betas)
        fp(76) = (el + s2y) / (4.0 * betas)
        fp(83) = fm(5,6) / (2.0 * betas)
      endif
 
*---- Fourth order terms.
      if (nord .ge. 4) then
        c2x = 2.0*cx**2 - 1.0
        c2y = 2.0*cy**2 - 1.0
        fp( 84) = sk1**2 * (s2x * (4.0 - c2x) - 3.0*el) / 64.0
        fp( 85) = - sk1**3 * sx**4 / 8.0
        fp( 90) = (3.0 / 32.0) * sk1 * (c2x*s2x - el)
        fp( 95) = sk1**2 * (2.0*el - s2x * (2.0 - c2y) -
     +    s2y * (2.0 - c2x)) / 32.0
        fp( 96) = sk1 * (c2y * (2.0 - c2x) - 1.0 -
     +    4.0*sk1*s2x*s2y) / 32.0
        fp( 99) = sk1 * (s2x * (2.0 + c2y) - s2y * (2.0 - c2x) -
     +    2.0*el) / 32.0
        fp(104) = sk1 * ((el - s2x) + (3.0 * s2x + el * (c2x - 4.0)) /
     +  (2.0 * betas**2)) / 8.0
        fp(105) = (cx**4 - 1.0) / 8.0
        fp(110) = sk1 * (1.0 - c2x * (2.0 - c2y) -
     +    4.0*sk1*s2x*s2y) / 32.0
        fp(111) = sk1 * (s2x*c2y - c2x*s2y) / 8.0
        fp(114) = (c2x * (2.0 + c2y) - 4.0*sk1*s2x*s2y - 3.0) / 32.0
        fp(119) = - sk1 * (el*s2x + (2.0 - betas**2) * sx**2) / (4.0 *
     +  betas**2)
        fp(140) = - (s2x * (4.0 + c2x) + 3.0*el) / 64.0
        fp(145) = sk1 * (2.0*el - s2y * (2.0 + c2x) +
     +    s2x * (2.0-c2y)) / 32.0
        fp(146) = (c2y * (2.0 + c2x) + 4.0*sk1*s2x*s2y - 3.0) / 32.0
        fp(149) = - (2.0*el + s2x * (2.0 + c2y) +
     +    s2y * (2.0 + c2x)) / 32.0
        fp(154) = ((el + s2x) - (5.0 * s2x + el * (6.0 + c2x)) / (2.0 *
     +  betas**2)) / 8.0
        fp(175) = sk1**2 * (s2y * (4.0 - c2y) - 3.0*el) / 64.0
        fp(176) = + sk1**3 * sy**4 / 8.0
        fp(179) = (3.0 / 32.0) * sk1 * (el - c2y*s2y)
        fp(184) = - sk1 * ((el - s2y) + (3.0 * s2y + el * (c2y - 4.0)) /
     +  (2.0 * betas**2)) / 8.0
        fp(185) = (cy**4 - 1.0) / 8.0
        fp(190) = sk1 * (el*s2y + (2.0 - betas**2) * sy**2) / (4.0 *
     +  betas**2)
        fp(195) = - (s2y * (4.0 + c2y) + 3.0*el) / 64.0
        fp(200) = ((el + s2y) - (5.0 * s2y + el * (6.0 + c2y)) / (2.0 *
     +  betas**2)) / 8.0
        fp(209) = fm(5,6) * (1.0 - 5.0 / betas**2) / 8.0
      endif
 
*---- Transform for rotation.
      if (tilt .ne. 0.) call lmtilt(nord, tilt, fp, fm)
 
      end
