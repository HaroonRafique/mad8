      subroutine lmdsp1(ipos, nord, d, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a misalignment at entrance.                  *
* Input:                                                               *
*   IPOS      (integer) Position in beam line (not used).              *
*   NORD      (integer) Order desired.                                 *
*   D(6)      (real)    Components of misalignemt.                     *
* Output:                                                              *
*   FP, FM    (map)     Map representing displacement.                 *
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
      integer ipos,nord
      double precision d,fm,fp,half,phi,psi,s2,the,two,w
      dimension         d(6), fp(*), fm(6,6)
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
 
      dimension         w(3,3)
      parameter         (two = 2.0d0, half = 0.5d0)
 
*---- Build rotation matrix and compute additional drift length.
      the = d(5)
      phi = d(4)
      psi = d(6)
      call sumtrx(the, phi, psi, w)
      s2 = (w(1,3) * d(1) + w(2,3) * d(2) + w(3,3) * d(3)) / w(3,3)
 
*---- F1 terms (kicks).
      call lmone(nord, fp, fm)
      fp(1) = - w(1,3) / w(3,3)
      fp(2) = d(1) + fp(1) * s2
      fp(3) = - w(2,3) / w(3,3)
      fp(4) = d(2) + fp(3) * s2
      fp(5) = 0.0
      fp(6) = d(3) / betas
 
*---- F2 terms (transfer matrix).
      fm(2,2) = w(1,1)
      fm(2,4) = w(2,1)
      fm(2,6) = w(3,1) / betas
      fm(4,2) = w(1,2)
      fm(4,4) = w(2,2)
      fm(4,6) = w(3,2) / betas
 
      fm(1,1) =   w(2,2) / w(3,3)
      fm(1,2) = fm(1,1) * s2
      fm(1,3) = - w(1,2) / w(3,3)
      fm(1,4) = fm(1,3) * s2
      fm(3,1) = - w(2,1) / w(3,3)
      fm(3,2) = fm(3,1) * s2
      fm(3,3) =   w(1,1) / w(3,3)
      fm(3,4) = fm(3,3) * s2
      fm(5,1) = w(1,3) / (w(3,3) * betas)
      fm(5,2) = fm(5,1) * s2
      fm(5,3) = w(2,3) / (w(3,3) * betas)
      fm(5,4) = fm(5,3) * s2
      fm(5,6) = - s2 / (betas * gammas)**2
 
*---- F3  and F4 terms are ignored.
 
      end
