      subroutine ha4ana
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Analyse the fourth order effects of sextupoles,                    *
*   Fourth order meaning octupole effects = quarter integer effects.   *
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
      double precision ensige,ensigx,ensigy
 
*---- Communication area for HARMON module.
      common /harchr/   lngnam, shtnam
      common /harflt/   ensigx, ensigy, ensige
      save              /harchr/, /harflt/
      character*(mcnam) lngnam, shtnam
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer igo,ip,j0,k0,l0,m0,n1,n2,np
      double precision ans1,ans2,ans3,beat4,des,dqde,dqdy,dqs,dqs20,
     +dqx4,dqxdey,dqy4,dqydex,ex0,ey0,fact4,fn1,fn2,g,g0022,g0040,g1111,
     +g2002,g2020,g2200,g4000,gmod
 
      parameter         (beat4 = 1.553774d0, fact4 = 1.446735d0)
 
      dimension         ans1(2),  ans2(2),  ans3(2),  g(2)
      dimension         g2200(2), g0022(2), g1111(2), g4000(2),
     +                  g0040(2), g2020(2), g2002(2)
      logical first
 
*---- Fourth order effects of sextupoles.
      write (iqpr2, 910)
      ex0 = ex * ensigx**2
      ey0 = ey * ensigy**2
 
*==== Q shift effects.
*---- G2200.
      write (iqpr2, 920)
*     IF (.NOT. SKEW) THEN
        call ha4sum(3, 0, 0, 0, 0, 3, 0, 0, 0, ans1)
        call ha4sum(2, 1, 0, 0, 1, 2, 0, 0, 0, ans2)
        g2200(1) = - 9.0*ans1(1) - 3.0*ans2(1)
        g2200(2) = - 9.0*ans1(2) - 3.0*ans2(2)
*     ELSE
*       CALL HA4SUM(2, 0, 1, 0, 0, 2, 0, 1, 0, ANS1)
*       CALL HA4SUM(0, 2, 1, 0, 2, 0, 0, 1, 0, ANS2)
*       CALL HA4SUM(1, 1, 1, 0, 1, 1, 0, 1, 0, ANS3)
*       G2200(1) = - (ANS1(1) + ANS2(1) + ANS3(1))
*       G2200(2) = - (ANS1(2) + ANS2(2) + ANS3(2))
*     ENDIF
      dqde = 2.0 * g2200(1)
      dqx4 = dqde * ex0
      write (iqpr2, 930) g2200, dqde, dqx4
 
*---- G00220.
*     IF (.NOT. SKEW) THEN
        call ha4sum(1, 0, 2, 0, 0, 1, 0, 2, 0, ans1)
        call ha4sum(1, 0, 0, 2, 0, 1, 2, 0, 0, ans2)
        call ha4sum(1, 0, 1, 1, 0, 1, 1, 1, 0, ans3)
        g0022(1) = - (ans1(1) + ans2(1) + ans3(1))
        g0022(2) = - (ans1(2) + ans2(2) + ans3(2))
*     ELSE
*       CALL HA4SUM(0, 0, 3, 0, 0, 0, 0, 3, 0, ANS1)
*       CALL HA4SUM(0, 0, 2, 1, 0, 0, 1, 2, 0, ANS2)
*       G0022(1) = - 9.0*ANS1(1) - 3.0ANS2(1)
*       G0022(2) = - 9.0*ANS1(2) - 3.*ANS2(2)
*     ENDIF
      dqde = 2.0 * g0022(1)
      dqdy = dqde * ey0
      write (iqpr2, 940) g0022, dqde, dqdy
 
*---- G11110.
*     IF (.NOT. SKEW) THEN
        call ha4sum(1, 0, 2, 0, 0, 1, 0, 2, 0, ans1)
        call ha4sum(0, 1, 2, 0, 1, 0, 0, 2, 0, ans2)
        call ha4sum(2, 1, 0, 0, 0, 1, 1, 1, 0, ans3)
*     ELSE
*       CALL HA4SUM(2, 0, 0, 1, 0, 2, 1, 0, 0, ANS1)
*       CALL HA4SUM(2, 0, 1, 0, 0, 2, 0, 1, 0, ANS2)
*       CALL HA4SUM(0, 0, 2, 1, 1, 1, 0, 1, 0, ANS3)
*     ENDIF
 
*---- Third term is 2 * (ANS3 + CONJG(ANS3)).
      g1111(1) = - 4.0 * (ans1(1) + ans2(1) + ans3(1))
      g1111(2) = - 4.0 * (ans1(2) + ans2(2))
      dqxdey = g1111(1)
      dqydex = dqxdey
      dqx4 = dqxdey * ey0
      dqy4 = dqydex * ex0
      write (iqpr2, 950) g1111, dqxdey, dqydex, dqx4, dqy4
 
*==== Resonance effects.
      first = .true.
 
*---- G4000p.
   70 continue
      write (iqpr2, 960)
      np = int(4.0 * qx / nsup)
      if (.not. first) np = np + 1
*     IF (.NOT. SKEW) THEN
        call ha4sum(2, 1, 0, 0, 3, 0, 0, 0, np, g4000)
        g(1) = 3.0 * g4000(1)
        g(2) = 3.0 * g4000(2)
*     ELSE
*       CALL HA4SUM(2, 0, 0, 1, 2, 0, 1, 0, NP, G4000)
*       G(1) = G4000(1)
*       G(2) = G4000(2)
*     ENDIF
      j0 = 4
      k0 = 0
      l0 = 0
      m0 = 0
      assign 80 to igo
      go to 120
 
*---- G0040p.
   80 continue
      np = int(4.0 * qy / nsup)
      if (.not. first) np = np + 1
*     IF (.NOT. SKEW) THEN
        call ha4sum(0, 1, 2, 0, 1, 0, 2, 0, np, g0040)
        g(1) = g0040(1)
        g(2) = g0040(2)
*     ELSE
*       CALL HA4SUM(0, 0, 2, 1, 0, 0, 3, 0, NP, G0040)
*       G(1) = 3.0 * G0040(1)
*       G(2) = 3.0 * G0040(2)
*     ENDIF
      j0 = 0
      l0 = 4
      assign 90 to igo
      go to 120
 
*---- G2020.
   90 continue
      np = int((2.0 * qx + 2.0 * qy) / nsup)
      if (.not. first) np = np + 1
*     IF (.NOT. SKEW) THEN
        call ha4sum(2, 1, 0, 0, 1, 0, 2, 0, np, ans1)
        call ha4sum(0, 1, 2, 0, 3, 0, 0, 0, np, ans2)
        call ha4sum(1, 0, 1, 1, 1, 0, 2, 0, np, ans3)
        g2020(1) = ans1(1) + 3.0*ans2(1) + 2.0*ans3(1)
        g2020(2) = ans1(2) + 3.0*ans2(2) + 2.0*ans3(2)
*     ELSE
*       CALL HA4SUM(1, 1, 1, 0, 2, 0, 1, 0, NP, ANS1)
*       CALL HA4SUM(0, 0, 2, 1, 2, 0, 1, 0, NP, ANS2)
*       CALL HA4SUM(2, 0, 0, 1, 0, 0, 3, 0, NP, ANS3)
*       G2020(1) = 2.0*ANS1(1) + ANS2(1) + 3.0*ANS3(1)
*       G2020(2) = 2.0*ANS1(2) + ANS2(2) + 3.0*ANS3(2)
*     ENDIF
      j0 = 2
      l0 = 2
      g(1) = g2020(1)
      g(2) = g2020(2)
      assign 150 to igo
 
*==== Resonance widths.
  120 continue
      n1 = j0 + k0
      n2 = l0 + m0
      fn1 = n1
      fn2 = n2
      g(1) = - g(1)
      g(2) = - g(2)
      gmod = sqrt(g(1)**2 + g(2)**2)
      des = gmod * sqrt(ex0)**n1 * sqrt(ey0)**n2 *
     +      (fn1**2 / ex0 + fn2**2 / ey0)
      dqs = des * beat4**2 / sqrt(fn1**2 + fn2**2)
      dqs20 = dqs * fact4
      ip = nsup * np
      write (iqpr2, 970) j0, k0, l0, m0, ip, g, des, dqs, dqs20
      go to igo, (80, 90, 150)
 
*==== Difference resonances.
*---- G2002.
  150 continue
      np = int((2.0 * qx - 2.0 * qy) / nsup)
      if (.not. first) np = np + 1
*     IF (.NOT. SKEW) THEN
        call ha4sum(2, 1, 0, 0, 1, 0, 0, 2, np, ans1)
        call ha4sum(0, 1, 0, 2, 3, 0, 0, 0, np, ans2)
        call ha4sum(1, 0, 1, 1, 1, 0, 0, 2, np, ans3)
        g2002(1) = ans1(1) + 3.0*ans2(1) + 2.0*ans3(1)
        g2002(2) = ans1(2) + 3.0*ans2(2) + 2.0*ans3(2)
*     ELSE
*       CALL HA4SUM(1, 1, 0, 1, 2, 0, 0, 1, NP, ANS1)
*       CALL HA4SUM(0, 0, 1, 2, 2, 0, 0, 1, NP, ANS2)
*       CALL HA4SUM(2, 0, 1, 0, 0, 0, 0, 3, NP, ANS3)
*       G2002(1) = 2.0*ANS1(1) + ANS2(1) + 3.0*ANS3(1)
*       G2002(2) = 2.0*ANS1(2) + ANS2(2) + 3.0*ANS3(2)
*     ENDIF
      gmod = sqrt(g2002(1)**2 + g2002(2)**2)
      ip = nsup * np
      write (iqpr2, 980) ip, g2002, gmod
 
*---- Set up for resonances above the working point.
      if (.not. first) return
      first = .false.
      go to 70
 
  910 format(' '/' Fourth order effects of sextupoles:')
  920 format(' '/' Q shift effects')
  930 format(' '/20x,'G95000',6x,'dQx/dEx',10x,'dQx'/1p,4e13.5)
  940 format(' '/20x,'G00950',6x,'dQy/dEy',10x,'dQy'/1p,4e13.5)
  950 format(' '/20x,'G11110',6x,'dQx/dEy',6x,'dQy/dEx',10x,'dQx',10x,
     +       'dQy'/1p,6e13.5)
  960 format(' '/' Resonance effects')
  970 format(' '/22x,'cos',9x,'sin',10x,'dE',10x,'dQ',6x,'dQ(20)'/
     +       ' G',4i1,',',i3,3x,1p,5e12.4)
  980 format(' '/22x,'cos',9x,'sin',5x,'modulus'/
     +       ' G2002',',',i3,3x,1p,3e12.4)
 
      end
