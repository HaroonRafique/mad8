      subroutine wmaxmin(track)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Computes maximum and minimum betatron invariants during tracking.  *
* Input:                                                               *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
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
      integer i,kp,kq
      double precision track,utwopi,wx,wxy,wy,z,zn
      dimension         track(6)
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
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      integer itrturns,ktrturns
      double precision chkbelow,deltax,dtune,dynapfrac,fracmin,smear,
     +trstep,tunx,tuny,wxmax,wxmin,wxstart,wxymax,wxymin,wxystart,wymax,
     +wymin,wystart,yapunov,zendyn,zstart
 
*---- Communication area for DYNAP command.
      common /trcdyn/   trknam(2)
      character*(mcnam) trknam
      common /trfdyn/   chkbelow, deltax, dtune, dynapfrac, fracmin,
     +                  smear, trstep, tunx, tuny,
     +                  wxmax, wxmin, wymax, wymin, wxymax, wxymin,
     +                  wxstart, wystart, wxystart, yapunov,
     +                  zstart(6), zendyn(6)
      common /tridyn/   itrturns, ktrturns
      common /trldyn/   fastune, lyapflag, orbflag
      logical           fastune, lyapflag, orbflag
      save              /trcdyn/, /trfdyn/, /tridyn/, /trldyn/
      double precision aival,eigen,reval
 
*---- Initial conditions for optical functions for tracking.
      common /troptc/   eigen(6,6), reval(6), aival(6)
      save              /troptc/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0d0 / (2.0d0 * pi))
      dimension         z(6), zn(6)
 
*---- Copy track coordinates.
      do 10 i = 1, 6
        z(i) = track(i) - orbit0(i)
   10 continue
 
*---- Convert to normalized values.
      do 20 kq = 1, 5, 2
        kp = kq + 1
        zn(kq) = eigen(2,kp) * z(1) - eigen(1,kp) * z(2)
     +         + eigen(4,kp) * z(3) - eigen(3,kp) * z(4)
     +         + eigen(6,kp) * z(5) - eigen(5,kp) * z(6)
        zn(kp) = eigen(1,kq) * z(2) - eigen(2,kq) * z(1)
     +         + eigen(3,kq) * z(4) - eigen(4,kq) * z(3)
     +         + eigen(5,kq) * z(6) - eigen(6,kq) * z(5)
   20 continue
 
*---- Convert to amplitudes (and phases: not computed).
      wx = zn(1)**2 + zn(2)**2
      wy = zn(3)**2 + zn(4)**2
      wxy = wx + wy
 
*---- Compare to and redefine WMIN and WMAX in TRDYNAP.
      if (wx.gt.wxmax) then
        wxmax = wx
      else if (wx.lt.wxmin) then
        wxmin = wx
      endif
      if (wy.gt.wymax) then
        wymax = wy
      else if (wy.lt.wymin) then
        wymin = wy
      endif
      if (wxy.gt.wxymax) then
        wxymax = wxy
      else if (wxy.lt.wxymin) then
        wxymin = wxy
      endif
 
      end
