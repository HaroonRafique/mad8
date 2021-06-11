      subroutine enprgl
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print global data for machine.                                     *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer len
      double precision eta,t0
 
*---- Global parameters.
      call utleng(prtnam, len)
      if (alfa .gt. 0.0) then
        gamtr = sqrt(1.0 / alfa)
      else if (alfa .eq. 0.0) then
        gamtr = 0.0
      else
        gamtr = - sqrt(-1.0 / alfa)
      endif
      t0 = 1.0 / freq0
      eta = alfa - 1.0 / gamma**2
      write (iqpr2, 910) prtnam(1:len), frad, circ, freq0, t0, alfa,
     +eta, gamtr, currnt, bunch, parnum, en0, gamma, beta
 
  910 format(' '/' Global parameters for ',a,'S, radiate = ',l1,':'/' '/
     +       t6,'C',t16,f14.6,' m',t46,'f0',t56,f14.6,' MHz',
     +       t86,'T0',t96,f14.6,' microseconds'/
     +       t6,'alfa',t16,e18.6,t46,'eta',t56,e18.6,
     +       t86,'gamma(tr)',t96,f14.6/
     +       t6,'Bcurrent',t16,f14.6,' A/bunch',t46,'Kbunch',t56,f14.6,
     +       t86,'Npart',t96,e18.6,' per bunch'/
     +       t6,'E',t16,f14.6,' GeV',t46,'gamma',t56,f14.6,
     +       t86,'beta',t96,f14.6)
 
      end
