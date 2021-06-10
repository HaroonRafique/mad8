      subroutine tmgsec(fsec, el, h, dh, sk1, sks, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for bending magnets of all types.                    *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   EL        (real)    Length of the magnet body.                     *
*   H         (real)    Reference curvature of magnet.                 *
*   DH        (real)    Dipole field error.                            *
*   SK1       (real)    Quadrupole strength.                           *
*   SKS       (real)    Skew quadrupole strengh.                       *
* Output:                                                              *
*   EK(6)     (real)    Kick due to element.                           *
*   RE(6,6)   (real)    Transfer matrix.                               *
*   TE(6,6,6) (real)    Second-order terms.                            *
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
      double precision c1,c2,cphi,d1,d2,dh,dif,ek,el,f1,f2,h,one,re,
     +root,s1,s2,sk1,sks,sphi,sum,te,three,two,w1,w2,xlm1,xlm2,zero
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
 
      parameter         (zero = 0.0d0)
      parameter         (one = 1.0d0, two = 2.0d0, three = 3.0d0)
 
*---- Initialize.
      call uzero(ek, 1, 6*mwflt)
      call m66one(re)
 
*---- First-order terms.
      sum  = h**2 / two
      dif  = sum + sk1
      root = sqrt(dif**2 + sks**2)
      cphi = dif / root
      sphi = sks / root
      xlm1 = sum + (dif*cphi - sks*sphi)
      xlm2 = sum - (dif*cphi - sks*sphi)
      call tmfoc(el, xlm1, c1, s1, d1, f1)
      call tmfoc(el, xlm2, c2, s2, d2, f2)
      w1 = - xlm1 * s1
      w2 = - xlm2 * s2
 
      re(1,1) = ((c1 + c2) + (c1 - c2) * cphi) / two
      re(1,2) = ((s1 + s2) + (s1 - s2) * cphi) / two
      re(2,1) = ((w1 + w2) + (w1 - w2) * cphi) / two
      re(2,2) = re(1,1)
 
      re(1,3) = (c2 - c1) * sphi / two
      re(1,4) = (s2 - s1) * sphi / two
      re(2,3) = (w2 - w1) * sphi / two
      re(2,4) = re(1,3)
 
      re(3,1) = (c2 - c1) * sphi / two
      re(3,2) = (s2 - s1) * sphi / two
      re(4,1) = (w2 - w1) * sphi / two
      re(4,2) = re(3,1)
 
      re(3,3) = ((c1 + c2) - (c1 - c2) * cphi) / two
      re(3,4) = ((s1 + s2) - (s1 - s2) * cphi) / two
      re(4,3) = ((w1 + w2) - (w1 - w2) * cphi) / two
      re(4,4) = re(1,1)
 
      re(1,6) = h * ((d1 + d2) + (d1 - d2) * cphi) / two
      re(2,6) = h * ((s1 + s2) + (s1 - s2) * cphi) / two
      re(3,6) = h * (d2 - d1) / two
      re(4,6) = h * (s2 - s1) / two
      re(5,1) = - re(2,6)
      re(5,2) = - re(1,6)
      re(5,3) = - re(4,6)
      re(5,4) = - re(3,6)
      re(5,6) = el/(betas*gammas)**2 -
     +          h**2 * ((f1 + f2) + (f1 - f2) * cphi) / two
      ek(5)   = el * dtbyds
 
*---- Second-order terms.
      if (fsec) then
        call uzero(te, 1, 216*mwflt)
      endif
 
      end
