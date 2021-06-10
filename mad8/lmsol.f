      subroutine lmsol(nord, el, sks, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a solenoid.                                  *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Solenoid length.                               *
*   SKS       (real)    Solenoid strength.                             *
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
      double precision co,el,fact,fm,fp,si,sibk,sik,sk,skl,sks
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
 
*---- Set up C's and S's.
      call lmone(nord, fp, fm)
      sk = sks / 2.0
      skl = el * sk
      co = cos(skl)
      si = sin(skl)
      if (abs(skl) .lt. 1.0d-8) then
        sibk = (1 - skl**2 / 6.0) * el
      else
        sibk = si / sk
      endif
 
*---- Linear terms.
      sik = si * sk
      fm(1,1) = co * co
      fm(1,2) = co * sibk
      fm(1,3) = co * si
      fm(1,4) = si * sibk
      fm(2,1) = - co * sik
      fm(2,2) = co * co
      fm(2,3) = - si * sik
      fm(2,4) = co * si
      fm(3,1) = - co * si
      fm(3,2) = - si * sibk
      fm(3,3) = co * co
      fm(3,4) = co * sibk
      fm(4,1) = si * sik
      fm(4,2) = - co * si
      fm(4,3) = - co * sik
      fm(4,4) = co * co
      fm(5,6) = el/(betas*gammas)**2
      fp(6) = - el*dtbyds
 
*---- Third order terms.
      if (nord .ge. 3) then
        fp(53) = el / (2.0 * betas)
        fp(76) = el / (2.0 * betas)
        fp(83) = fm(5,6) / (2.0 * betas)
        fp(57) = 2.0 * fp(53) * sk
        fp(45) = - fp(57)
        fp(33) = fp(53) * sk**2
        fp(67) = fp(33)
      endif
 
*---- Fourth order terms.
      if (nord .ge. 4) then
        fp(140) = - el / 8.0
        fp(149) = - el / 4.0
        fp(195) = - el / 8.0
        fp(154) = - el * (1.0 - 3.0 / betas**2) / 4.0
        fp(200) = - el * (1.0 - 3.0 / betas**2) / 4.0
        fp(209) = - fm(5,6) * (5.0 - 1.0 / betas**2) / 8.0
        fact = el * sk
        fp(107) = + fact / 2.0
        fp(130) = + fact / 2.0
        fp(141) = - fact / 2.0
        fp(159) = - fact / 2.0
        fp(135) = + fact * (1.0 - 3.0 / betas**2) / 2.0
        fp(164) = - fact * (1.0 - 3.0 / betas**2) / 2.0
        fact = fact * sk
        fp(111) = + fact
        fp( 90) = - fact / 4.0
        fp(179) = - fact / 4.0
        fp( 99) = - (fact * 3.0) / 4.0
        fp(145) = - (fact * 3.0) / 4.0
        fp(104) = - fact * (1.0 - 3.0 / betas**2) / 4.0
        fp(184) = - fact * (1.0 - 3.0 / betas**2) / 4.0
        fact = fact * sk
        fp( 87) = + fact / 2.0
        fp( 91) = - fact / 2.0
        fp(121) = + fact / 2.0
        fp(155) = - fact / 2.0
        fact = fact * sk
        fp( 84) = - fact / 8.0
        fp( 95) = - fact / 4.0
        fp(175) = - fact / 8.0
      endif
 
      end
