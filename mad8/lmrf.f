      subroutine lmrf(nord, el, rfv, rfl, rff, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a RF cavity.                                 *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Cavity length.                                 *
*   RFV       (real)    Cavity voltage.                                *
*   RFL       (real)    Cavity phase lag.                              *
*   RFF       (real)    Cavity frequency.                              *
* Output:                                                              *
*   FP, FM    (map)     Cavity map.                                    *
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
      integer idm,idp,isave,nord
      double precision el,fm,fp,omega,one,phirf,rff,rfl,rfv,ten3m,ten6p,
     +twopi,vrf
      dimension         fp(*), fm(6,6)
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
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
      parameter         (one = 1.0d0, ten3m = 1.0d-3, ten6p = 1.0d+6)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
 
*---- Extract data for element.
      isave = iwork
      omega = rff * ten6p * twopi / clight
      vrf   = rfv * ten3m / (pc * (one + deltas))
      phirf = rfl * twopi
 
*---- Transfer map for cavity as a thin lens.
      call lmone(nord, fp, fm)
      fp(5) = vrf * sin(phirf)
      fm(6,5) = - vrf * cos(phirf) * omega
*      IF (NORD .GE. 3) FP(80) = - VRF * SIN(PHIRF) * OMEGA**2 / 6.0
*      IF (NORD .GE. 4) FP(205) = VRF * COS(PHIRF) * OMEGA**3 / 24.0
 
*---- If length is non-zero, sandwich between two drifts.
*     Assign temporary storage.
      if (el .ne. 0.0) then
        idm   = iwork
        idp   = idm + 36
        iwork = idp + itop6(nord)
        if (iwork .gt. nwork) then
          call mzwork(0, dq(1), dq(iwork+1), 2)
          nwork = iwork
        endif
 
*---- Transfer map for drift of half length.
        call lmdrf(nord, el / 2.0, dq(idp+1), dq(idm+1))
 
*---- Concatenate the three parts.
        call lmcat(nord, dq(idp+1), dq(idm+1), fp, fm, fp, fm)
        call lmcat(nord, fp, fm, dq(idp+1), dq(idm+1), fp, fm)
      endif
 
*---- Release working storage.
      iwork = isave
 
      end
