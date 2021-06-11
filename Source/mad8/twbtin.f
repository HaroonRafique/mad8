      subroutine twbtin(lseq, chrom, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initial values for Twiss parameters.                               *
* Input:                                                               *
*   LSEQ(1)   (pointer) Beam line sequence bank.                       *
*   CHROM     (logical) True, to initialize also chromatic functions.  *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
* Important common data:                                               *
*   RT(6,6)   /MAPTRN/  One turn transfer matrix.                      *
*   TT(6,6,6) /MAPTRN/  Second order terms.                            *
*             /OPTIC0/  Initial values are stored here.                *
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
      integer i,ileng,j,k
      double precision aux,ax,ay,bx,by,eps,sinmu2,temp
      logical           chrom, eflag
      integer           lseq(*)
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
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
 
      character*(mcnam) linnam
      character*(mcrng) rngnam
      dimension         aux(6)
      parameter         (eps = 1.0d-8)
 
*---- Initial dispersion.
      call twdisp(rt, rt(1,6), disp0)
      disp0(5) = 0.0
      disp0(6) = 1.0
 
*---- Derivative of transfer matrix w.r.t. delta(p)/p.
      if (chrom) then
        do 30 i = 1, 6
          aux(i) = 0.0
          do 20 k = 1, 6
            temp = 0.0
            do 10 j = 1, 6
              temp = temp + tt(i,j,k) * disp0(j)
   10       continue
            aux(i) = aux(i) + temp * disp0(k)
            rtp(i,k) = 2.0 * temp
   20     continue
   30   continue
 
*---- Derivative of dispersion.
        call twdisp(rt, aux, ddisp0)
        ddisp0(5) = 0.0
        ddisp0(6) = 0.0
      endif
 
*---- Horizontal motion.
      betx0 = 0.0
      alfx0 = 0.0
      amux0 = 0.0
      wx0   = 0.0
      phix0 = 0.0
      dmux0 = 0.0
      cosmux = (rt(1,1) + rt(2,2)) / 2.0
      stabx = abs(cosmux) .lt. 1.0
      if (stabx) then
        sinmu2 = - rt(1,2)*rt(2,1) - 0.25*(rt(1,1) - rt(2,2))**2
        if (sinmu2 .lt. 0.0) sinmu2 = eps
        sinmux = sign(sqrt(sinmu2), rt(1,2))
        betx0 = rt(1,2) / sinmux
        alfx0 = (rt(1,1) - rt(2,2)) / (2.0 * sinmux)
        if (chrom) then
          bx = rtp(1,2) / rt(1,2) +
     +         (rtp(1,1) + rtp(2,2)) * cosmux / (2.0 * sinmu2)
          ax = (rtp(1,1) - rtp(2,2)) / (2.0 * sinmux) -
     +         alfx0 * rtp(1,2) / rt(1,2)
          wx0 = sqrt(bx**2 + ax**2)
          if (wx0 .gt. 1.0e-8) phix0 = atan2(ax,bx)
        endif
      endif
 
*---- Vertical motion.
      bety0 = 0.0
      alfy0 = 0.0
      amuy0 = 0.0
      wy0   = 0.0
      phiy0 = 0.0
      dmuy0 = 0.0
      cosmuy = (rt(3,3) + rt(4,4)) / 2.0
      staby = abs(cosmuy) .lt. 1.0
      if (staby) then
        sinmu2 = - rt(3,4)*rt(4,3) - 0.25*(rt(3,3) - rt(4,4))**2
        if (sinmu2 .lt. 0.) sinmu2 = eps
        sinmuy = sign(sqrt(sinmu2), rt(3,4))
        bety0 = rt(3,4) / sinmuy
        alfy0 = (rt(3,3) - rt(4,4)) / (2.0 * sinmuy)
        if (chrom) then
          by = rtp(3,4) / rt(3,4) +
     +         (rtp(3,3) + rtp(4,4)) * cosmuy / (2.0 * sinmu2)
          ay = (rtp(3,3) - rtp(4,4)) / (2.0 * sinmuy) -
     +         alfy0 * rtp(3,4) / rt(3,4)
          wy0 = sqrt(by**2 + ay**2)
          if (wy0 .gt. 1.0e-8) phiy0 = atan2(ay,by)
        endif
      endif
 
*---- Give message, if unstable.
      eflag = .false.
      if (.not. (stabx .and. staby)) then
        call uhtoc(q(lseq(1)+msbn), mcwrd, linnam, mcnam)
        call utleng(linnam, ileng)
        call uhtoc(q(lseq(1)+msrn), mcwrd, rngnam, 40)
        if (staby) then
          write (msg, 910) linnam(1:ileng), rngnam, 'X', deltas
          call aawarn('TWBTIN', 3, msg)
        else if (stabx) then
          write (msg, 910) linnam(1:ileng), rngnam, 'Y', deltas
          call aawarn('TWBTIN', 3, msg)
        else
          write (msg, 920) linnam(1:ileng), rngnam, deltas
          call aawarn('TWBTIN', 3, msg)
        endif
        eflag = .true.
      endif
 
  910 format('Beam line "',a,'", range: ',a/
     +       'plane ',a1,' is unstable  for delta(p)/p =',f12.6/
     +       'Twiss suppressed.')
  920 format('Beam line "',a,'", range: ',a/
     +       'both planes are unstable for delta(p)/p = ',f12.6/
     +       'Twiss suppressed.')
 
      end
