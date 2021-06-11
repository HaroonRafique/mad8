      subroutine twmain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Switch routine (subprocess code) for Twiss section.                *
* Input:                                                               *
*   IPR       (integer) Process code.                                  *
*   ISP       (integer) Subprocess code.                               *
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
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer ipr,isp,itwflg
 
      data itwflg       / 0 /
 
      if (itwflg .eq. 0) then
        call mzlink(0, '/TWLINK/', ltwlin, ltwlin, ltwsum)
        itwflg = 1
      endif
 
*---- User-defined services.
      if (isp .le. 0  .or.  isp .gt. 40) then
        call usercm(ipr, isp)
 
*---- ISP = 1, TWISS.
      else if (isp .eq. 1) then
        call twiss
 
*---- ISP = 2, OPTICS.
      else if (isp .eq. 2) then
        call twoptc
 
*---- ISP = 5, NORMAL.
      else if (isp .eq. 3) then
        call emnorm
 
*---- ISP = 6, EMIT.
      else if (isp .eq. 4) then
        call ememdo
 
*---- ISP = 7, EIGEN.
      else if (isp .eq. 5) then
        call emevdo
 
*---- ISP = 8, ENVELOPE.
      else if (isp .eq. 6) then
        call emendo
 
*---- ISP = 9, TWISS3
      else if (isp .eq. 7) then
        call emtwdo
 
*---- ISP = 11, BMPM
      else if (isp .eq. 21) then
        call bmmain
 
*---- ISP = 12, IBS.
      else if (isp .eq. 22) then
        call twibs
 
*---- ISP = 13, SECTORMAP
      else if (isp .eq. 23) then
        call twsmap
      endif
 
*---- Save computed quantities.
      if (isp .ge. 1  .and.  isp .le. 10) then
        call aasetp('QX',   qx)
        call aasetp('QY',   qy)
        call aasetp('QS',   qs)
        call aasetp('QX''', xix)
        call aasetp('QY''', xiy)
        call aasetp('ALFX', alfx0)
        call aasetp('ALFY', alfy0)
        call aasetp('BETX', betx0)
        call aasetp('BETY', bety0)
        call aasetp('X0',   orbit0(1))
        call aasetp('PX0',  orbit0(2))
        call aasetp('Y0',   orbit0(3))
        call aasetp('PY0',  orbit0(4))
        call aasetp('T0',   orbit0(5))
        call aasetp('PT0',  orbit0(6))
      endif
 
*---- Drop working space.
      iwork = 0
      nwork = 0
      call mzwork(0, dq(1), dq(1), - 1)
 
      end
