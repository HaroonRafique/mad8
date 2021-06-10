      subroutine ttcsr(f_csr, el, ang, track, ktrack)
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Include the effect of logitudinal coherent radiation assuming the  *
*   steady-state solution with a gaussian bunch                        *
* Input:                                                               *
*   FCSR      (real)    Scale factor for CSR fields                    *
*   EL        (real)    Length of bend magnet                          *
*   ANG       (real)    Bend angle                                     *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NTRACK    (integer) Number of surviving tracks.                    *
*----------------------------------------------------------------------*
* Created:  ??-JUL-1994, T. Raubenheimer (SLAC)                        *
*----------------------------------------------------------------------*
 
      implicit none
      double precision f_csr, el, ang, track(6,*)
      integer ktrack
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
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
*---- The function FF is the F(s/sigz) described in the DESY FEL paper:
*     ff(s)=int(dxp/(x-xp)**(1/3)*diff(exp(-xp**2/2),xp),xp=-infty..x);
*     the points were calculated using MAPLE.
 
      integer i, nz
      double precision z, sigz, avez, const, f
      double precision  ff(11), xx(11)
      data              ff / 0.8665911471d-5,0.0007257949504,
     +                       0.02193754614,  0.2361703259,
     +                       0.8770375303,   1.005634918,
     +                       0.1074721257,  -0.3249321455,
     +                      -0.2397423123,  -0.1490668117,
     +                      -0.1049894581 /
      data              xx / -5.d0, -4.d0, -3.d0, -2.d0, -1.d0, 0.d0,
     +                        1.d0,  2.d0,  3.d0,  4.d0,  5.d0 /
 
*---- Calculate the rms bunch length
      sigz = 0.d0
      avez = 0.d0
      do i=1, ktrack
        avez = avez + track(5,i)
        sigz = sigz + track(5,i)**2
      enddo
      avez = avez / max(ktrack,1)
      sigz = sqrt(max(0.d0, sigz/max(ktrack,1)-avez**2))
 
*---- const = 2*r0/sqrt(2*pi)/3**(1/3)*N/gamma*Lb/R**(2/3)/sigz**(4/3)
      const = 1.55e-15 * parnum / gamma * el**(0.33333333d0)
     +      * abs(ang)**(0.666666667d0) / sigz**(1.33333333d0) * f_csr
      do i = 1, ktrack
        z = (track(5,i) - avez ) / sigz
        if (z .le. -5.d0) then
          f = 0.d0
        else if (z .ge. 5.d0) then
          f = ff(11)
        else
          z = z + 6.d0
          nz = int(z)
          f = ff(nz) + (ff(nz+1)-ff(nz)) * (z-nz)
        endif
        track(6,i) = track(6,i) + const * f
      enddo
 
      write (6,800) f_csr, el, ang, avez, sigz, parnum, gamma
 
  800 format (' f_csr: ',1pg10.3,'  el,ang: ',2g13.4,
     +        '  avez,sigz: ',2g13.4,'  n,gamma: ',2g13.4)
 
      end
