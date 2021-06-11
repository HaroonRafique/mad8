      subroutine mtbtin(iflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initial values for linear coupling parameters.                     *
* Output:                                                              *
*   IFLAG     (integer) Error flag.                                    *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer iflag
      double precision a,arg,aux,d,den,det,dtr,eps,sinmu2
 
      parameter         (eps = 1.0d-10)
      dimension         aux(2,2), a(2,2), d(2,2)
 
*---- Initial dispersion.
      call twdisp(rt, rt(1,6), disp0)
      disp0(5) = 0.0
      disp0(6) = 1.0
 
*---- Matrix C + B(bar) and its determinant.
      aux(1,1) = rt(3,1) + rt(2,4)
      aux(1,2) = rt(3,2) - rt(1,4)
      aux(2,1) = rt(4,1) - rt(2,3)
      aux(2,2) = rt(4,2) + rt(1,3)
      det = aux(1,1) * aux(2,2) - aux(1,2) * aux(2,1)
 
*---- Coupling matrix.
      dtr = (rt(1,1) + rt(2,2) - rt(3,3) - rt(4,4)) / 2.0
      arg = det + dtr**2
      if (arg .ge. 0.0) then
        den = - (dtr + sign(sqrt(arg),dtr))
        if (abs(den) .lt. eps) then
          r0mat(1,1) = 0.0
          r0mat(1,2) = 0.0
          r0mat(2,1) = 0.0
          r0mat(2,2) = 0.0
        else
          r0mat(1,1) = aux(1,1) / den
          r0mat(1,2) = aux(1,2) / den
          r0mat(2,1) = aux(2,1) / den
          r0mat(2,2) = aux(2,2) / den
        endif
 
*---- Decouple: Find diagonal blocks.
        a(1,1) = rt(1,1) - rt(1,3)*r0mat(1,1) - rt(1,4)*r0mat(2,1)
        a(1,2) = rt(1,2) - rt(1,3)*r0mat(1,2) - rt(1,4)*r0mat(2,2)
        a(2,1) = rt(2,1) - rt(2,3)*r0mat(1,1) - rt(2,4)*r0mat(2,1)
        a(2,2) = rt(2,2) - rt(2,3)*r0mat(1,2) - rt(2,4)*r0mat(2,2)
        d(1,1) = rt(3,3) + r0mat(1,1)*rt(1,3) + r0mat(1,2)*rt(2,3)
        d(1,2) = rt(3,4) + r0mat(1,1)*rt(1,4) + r0mat(1,2)*rt(2,4)
        d(2,1) = rt(4,3) + r0mat(2,1)*rt(1,3) + r0mat(2,2)*rt(2,3)
        d(2,2) = rt(4,4) + r0mat(2,1)*rt(1,4) + r0mat(2,2)*rt(2,4)
 
*---- First mode.
        cosmux = (a(1,1) + a(2,2)) / 2.0
        stabx  = abs(cosmux) .lt. 1.0
        if (stabx) then
          sinmu2 = - a(1,2)*a(2,1) - 0.25*(a(1,1) - a(2,2))**2
          if (sinmu2 .lt. 0.0) sinmu2 = 0.0
          sinmux = sign(sqrt(sinmu2), a(1,2))
          betx0 = a(1,2) / sinmux
          alfx0 = (a(1,1) - a(2,2)) / (2.0 * sinmux)
        else
          betx0 = 0.0
          alfx0 = 0.0
        endif
 
*---- Second mode.
        cosmuy = (d(1,1) + d(2,2)) / 2.0
        staby  = abs(cosmuy) .lt. 1.0
        if (staby) then
          sinmu2 = - d(1,2)*d(2,1) - 0.25*(d(1,1) - d(2,2))**2
          if (sinmu2 .lt. 0.) sinmu2 = 0.0
          sinmuy = sign(sqrt(sinmu2), d(1,2))
          bety0 = d(1,2) / sinmuy
          alfy0 = (d(1,1) - d(2,2)) / (2.0 * sinmuy)
        else
          betx0 = 0.0
          alfy0 = 0.0
        endif
 
*---- Unstable due to coupling.
      else
        stabx = .false.
        staby = .false.
      endif
      iflag = 1
      if (stabx .and. staby) iflag = 0
 
*---- Initial phase angles.
      amux0 = 0.0
      amuy0 = 0.0
 
      end
