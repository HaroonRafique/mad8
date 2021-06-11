      subroutine lmsext(nord, el, sk2, tilt, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie algebraic map for a sextupole.                                 *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Sextupole length.                              *
*   SK2       (real)    Sextupole strength.                            *
*   TILT      (real)    Sextupole tilt.                                *
* Output:                                                              *
*   FP, FM    (map)     Sextupole map.                                 *
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
      double precision el,fact,fm,fp,sext,sk2,tilt
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
      double precision cohelp
 
*---- Communication area for TM module.
*     Positive sign means deflection to negative coordinates.
      common /tmcomm/   cohelp
      save              /tmcomm/
 
*---- Linear terms.
      call lmone(nord, fp, fm)
      fm(1,2) = el
      fm(3,4) = el
      fm(5,6) = el/(betas*gammas)**2
      fp(6) = - el*dtbyds
 
*---- Third order terms.
      if (nord .ge. 3) then
        sext = sk2 * cohelp
        fact = sext * el
        fp(28) = - fact / 6.0
        fp(39) = + fact / 2.0
        fact = fact * el
        fp(29) = + fact / 4.0
        fp(40) = - fact / 2.0
        fp(54) = - fact / 4.0
        fact = fact * el
        fp(34) = - fact / 6.0
        fp(43) = + fact / 6.0
        fp(55) = + fact / 3.0
        fact = fact * el
        fp(49) = + fact / 24.0
        fp(58) = - fact / 8.0
        fp(53) = el / (2.0 * betas)
        fp(76) = el / (2.0 * betas)
        fp(83) = fm(5,6) / (2.0 * betas)
      endif
 
*---- Fourth order terms.
      if (nord .ge. 4) then
        fact = sext * sext * el * el * el
        fp( 84) = + fact / 48.0
        fp( 95) = + fact / 24.0
        fp(175) = + fact / 48.0
        fact = fact * el
        fp( 85) = - fact / 24.0
        fp( 96) = - fact / 24.0
        fp(110) = - fact / 24.0
        fp(176) = - fact / 24.0
        fact = fact * el
        fp( 90) = + fact / 32.0
        fp( 99) = + fact / 480.0
        fp(111) = + fact * (7.0 / 120.0)
        fp(145) = + fact / 480.0
        fp(179) = + fact / 32.0
        fact = fact * el
        fp(105) = - fact / 96.0
        fp(114) = - fact / 96.0
        fp(146) = - fact / 96.0
        fp(185) = - fact / 96.0
        fact = fact * el
        fp(140) = + fact / 672.0 - el / 8.0
        fp(149) = + fact / 336.0 - el / 4.0
        fp(195) = + fact / 672.0 - el / 8.0
        fact = sext * el * el * el / betas
        fp(109) = + fact / 12.0
        fp(132) = - fact / 12.0
        fp(161) = - fact / 6.0
        fact = fact * el
        fp(144) = - fact / 24.0
        fp(167) = + fact / 8.0
        fp(154) = el * (1.0 - 3.0 / betas**2) / 4.0
        fp(200) = el * (1.0 - 3.0 / betas**2) / 4.0
        fp(209) = fm(5,6) * (1.0 - 5.0 / betas**2) / 8.0
      endif
 
*---- Transform for rotation.
      if (tilt .ne. 0.) call lmtilt(nord, tilt, fp, fm)
 
      end
