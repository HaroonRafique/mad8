      subroutine lmsep(nord, el, efield, tilt, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for an electrostatic separator.                  *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Separator length.                              *
*   EFIELD    (real)    Separator field.                               *
*   TILT      (real)    Separator tilt.                                *
* Output:                                                              *
*   FP, FM    (map)     Separator map.                                 *
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
      double precision by120,by2,by24,by6,ch,cy,dy,efield,ekick,ekl,el,
     +eps,f444,f446,f466,f666,fact,fm,fp,one,sh,sy,ten3m,three,tilt
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
 
      parameter         (one     = 1.d0)
      parameter         (by2     = 1.d0/2.d0)
      parameter         (by6     = 1.d0/6.d0)
      parameter         (by24    = 1.d0/24.d0)
      parameter         (by120   = 1.d0/120.d0)
      parameter         (three   = 3.0d0)
      parameter         (eps     = 1.d-4)
      parameter         (ten3m   = 1.d-3)
 
*---- Prepare linear transformation parameters.
*     DY = (COSH(K*L) - 1) / K.
      ekick  = efield * ten3m * charge / (pc * (one + deltas))
      ekl = ekick * el
      if (abs(ekl) .gt. eps) then
        ch = cosh(ekl)
        cy = ch
        sh = sinh(ekl)
        sy = sh / ekick
        dy = (ch - one) / ekick**2
      else
        ch = (one + by2  * ekl**2)
        cy = ch
        sy = (one + by6  * ekl**2) * el
        sh = sy * ekick
        dy = (by2 + by24 * ekl**2) * el**2
      endif
 
*---- Kicks.
      call lmone(nord, fp, fm)
      fp(3) = sy * (ekick / betas)
      fp(4) = - dy * (ekick / betas)
      fp(6) = - el * dtbyds
 
*---- Transfer matrix.
      fm(1,2) = el
      fm(3,3) = ch - ekl * sh / betas**2
      fm(3,4) = sy
      fm(3,6) = (dy - el * sy / betas**2) * ekick
      fm(4,3) = (sh - ekl * ch / betas**2) * ekick
      fm(4,4) = ch
      fm(4,6) = (sh - ekl * ch / betas**2)
      fm(5,3) = - fm(4,6)
      fm(5,4) = - dy * ekick
      fm(5,6) = - (sy - el * cy / betas**2)
 
*---- Third order.
      if (nord .ge. 3) then
        fp(50) = + ekl * ch / (2.0 * betas)
        fp(51) = - el * sh / (2.0 * betas)
        fp(53) = + el * ch / (2.0 * betas)
 
        fact = el * (ekl**2 / betas**2 + three / gammas**2) / betas**3
        f444 = fact * sh**3 + fm(3,3) * (three * el * ch * sh / betas)
        f446 = fact * sh**2 * ch + el * (fm(3,3) * (ch**2 + sh**2) + fm
     +  (4,6) * (ch*sh)) / betas
        f466 = fact * sh * ch**2 + el * (fm(4,6) * (ch**2 + sh**2) + fm
     +  (3,3) * (ch*sh)) / betas
        f666 = fact * ch**3 + fm(4,6) * (three * el * ch * sh / betas)
 
        fp(74) = - f444 * by6
        fp(68) = + f446 * by2 * ekick
        fp(76) = + f446 * by2
        fp(65) = - f466 * by2 * ekick**2
        fp(70) = - f466 * ekick
        fp(79) = - f466 * by2
        fp(64) = + f666 * by6 * ekick**3
        fp(67) = + f666 * by2 * ekick**2
        fp(73) = + f666 * by2 * ekick
        fp(83) = + f666 * by6
      endif
 
*---- $$$ Fourth order not available $$$
 
*---- Transform for rotation.
      if (tilt .ne. 0.0) call lmtilt(3, tilt, fp, fm)
 
      end
