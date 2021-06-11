      subroutine twsumm(symm, nsup)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute summary data for TWISS and OPTICS commands.                *
* Important common data:                                               *
*             /MAPTRN/  One turn transfer map.                         *
*             /OPTIC0/  Initial values.                                *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Include energy in summary calculation for non-periodic case        *
* Modified: 10-SEP-1999, M. Woodley (SLAC)                             *
*   Use the determinant of the longitudinal 2x2 part of the R-matrix   *
*   instead of R(6,6) for the energy scaling.                          *
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
      integer i,nsup
      double precision ax0,ay0,bx0,by0,eta,sd,sx,sy,t2,tb,twopi,utwopi,
     +zero,one,two
      logical           symm
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer ndelta
 
*---- Common for Twiss module.
      common /twchar/   funnam, optnam, sumnam, betnam
      common /twdata/   ndelta, chrom, couple
      save              /twdata/, /twchar/
      character*(mcnam) funnam, optnam, sumnam, betnam
      logical           chrom, couple
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0 * pi, utwopi = 1.0 / twopi)
      parameter         (zero = 0.0d0, one = 1.0d0, two = 2.0d0)
 
      double precision  f, detl
      double precision  frt(6,6), frtp(6,6) ! MDW: for energy scaling
*---- Summary data for non-periodic case.
      if (inval) then
        qx = amux * utwopi
        qy = amuy * utwopi
        if (.not. couple) then
 
*---- Tor/MDW: include energy scaling for uncoupled non-periodic case;
*     scale by square root of the determinant of the longitudinal 2x2
*     part of the R-matrix
          detl = rt(5,5) * rt(6,6) - rt(5,6) * rt(6,5)
          f = one / sqrt(detl)
          call m66scl(f, rt, frt)
          call m66scl(f, rtp, frtp)
 
          tb = frt(1,1) * betx0 - frt(1,2) * alfx0
          t2 = tb**2 + frt(1,2)**2
          bx0 = wx0 * cos(phix0)
          ax0 = wx0 * sin(phix0)
          xix = dmux0 + frt(1,2) * (frt(1,2) * ax0 - tb * bx0) / t2
     +        + (frt(1,1) * frtp(1,2) - frt(1,2) * frtp(1,1)) / betx
          xix = xix * utwopi
 
          tb = frt(3,3) * bety0 - frt(3,4) * alfy0
          t2 = tb**2 + frt(3,4)**2
          by0 = wy0 * cos(phiy0)
          ay0 = wy0 * sin(phiy0)
          xiy = dmuy0 + frt(3,4) * (frt(3,4) * ay0 - tb * by0) / t2
     +        + (frt(3,3) * frtp(3,4) - frt(3,4) * frtp(3,3)) / bety
          xiy = xiy * utwopi
        endif
        circ = suml
        alfa = 0.0
        gamtr = 0.0
        cosmux = 0.0
        cosmuy = 0.0
 
*---- Summary data for periodic case.
      else
        if (symm) then
          amux = amux + amux
          amuy = amuy + amuy
          suml = suml + suml
        endif
        qx = nsup * amux * utwopi
        qy = nsup * amuy * utwopi
        sd = rt(5,6)
        sx = tt(1,1,6) + tt(2,2,6)
        sy = tt(3,3,6) + tt(4,4,6)
        do 120 i = 1, 4
          sd = sd + rt(5,i) * disp0(i)
          sx = sx + (tt(1,1,i) + tt(2,2,i)) * disp0(i)
          sy = sy + (tt(3,3,i) + tt(4,4,i)) * disp0(i)
  120   continue
        if (.not. couple) then
          if (stabx) then
            xix = - nsup * sx / (twopi * sinmux)
          else
            xix = 0.0
          endif
          if (staby) then
            xiy = - nsup * sy / (twopi * sinmuy)
          else
            xiy = 0.0
          endif
        endif
        circ = nsup * suml
        eta = - sd * betas**2 / suml
        alfa = 1.0 / gammas**2 + eta
        if (alfa .gt. 0.0) then
          gamtr = sqrt(1.0 / alfa)
        else if (alfa .eq. 0.0) then
          gamtr = 0.0
        else
          gamtr = - sqrt(- 1.0 / alfa)
        endif
      endif
 
      end
