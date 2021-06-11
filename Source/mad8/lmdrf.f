      subroutine lmdrf(nord, el, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a drift space.                               *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Drift length.                                  *
* Output:                                                              *
*   FP, FM    (map)     Drift map.                                     *
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
      double precision el,fm,fp
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
      fm(1,2) = el
      fm(3,4) = el
      fm(5,6) = el / (betas*gammas)**2
      fp(6) = - el*dtbyds
 
*---- Third order terms.
      if (nord .ge. 3) then
        fp(53) = el / (2.0 * betas)
        fp(76) = el / (2.0 * betas)
        fp(83) = fm(5,6) / (2.0 * betas)
      endif
 
*---- Fourth order terms.
      if (nord .ge. 4) then
        fp(140) = - el / 8.0
        fp(149) = - el / 4.0
        fp(195) = - el / 8.0
        fp(154) = el * (1.0 - 3.0 / betas**2) / 4.0
        fp(200) = el * (1.0 - 3.0 / betas**2) / 4.0
        fp(209) = fm(5,6) * (1.0 - 5.0 / betas**2) / 8.0
      endif
 
      end
