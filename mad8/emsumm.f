      subroutine emsumm(comand, rd, em, bmax, gmax, dismax)
      implicit none
*---------------------------------------------------------------------*
* Purpose:                                                            *
*   Finish radiation damping calculation and print summary.           *
* Input:                                                              *
*   COMAND    (name)    Name of command.                              *
*   RD(6,6)   (real)    Damped one-turn transfer matrix.              *
*   EM(6,6)   (real)    Undamped eigenvectors.                        *
*   BMAX(3,3) (real)    Maximum extents of modes.                     *
*   GMAX(3,3) (real)    Maximum divergences of modes.                 *
*   DISMAX(6) (real)    Maximum dispersion.                           *
*---------------------------------------------------------------------*
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer j,j1,j2,k,k1,k2,nline
      double precision aival,alj,bmax,bstar,clg,dismax,dummy,em,etpr,
     +expr,eypr,f0,gmax,gstar,rd,reval,sal,sigma,tau,ten3p,tenp6,tenp9,
     +three,tune,twopi
      character*(*)     comand
      dimension         rd(6,6), em(6,6), bmax(3,3), gmax(3,3)
      dimension         dismax(4)
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
      double precision cg,sum,sumu0
 
*---- Communication area for radiation damping.
      common /emdata/   cg, sum(3), sumu0
      save   /emdata/
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      character*(*)     title
 
      dimension         reval(6), aival(6), alj(3), tau(3), tune(3)
      dimension         sigma(6,6), bstar(3,3), gstar(3,3), dummy(6,6)
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
      parameter         (three = 3.0d0)
      parameter         (ten3p = 1.0d3, tenp6 = 1.0d6, tenp9 = 1.0d9)
      parameter         (title = 'Summary.')
 
*---- Synchrotron energy loss [GeV].
      if (stabt .and. frad) then
        u0 = sumu0 * nsup
 
*---- Tunes.
        call ladeig(rd, reval, aival, dummy)
        if (error) go to 999
        tune(1) = nsup * atan2(aival(1), reval(1)) / twopi
        if (tune(1) .lt. 0.0) tune(1) = tune(1) + nsup
        tune(2) = nsup * atan2(aival(3), reval(3)) / twopi
        if (tune(2) .lt. 0.0) tune(2) = tune(2) + nsup
        tune(3) = nsup * atan2(aival(5), reval(5)) / twopi
        if (tune(3) .lt. 0.0) tune(3) = - tune(3)
 
*---- Damping constants per turn.
        alj(1) = - log(reval(1)**2 + aival(1)**2) * (nsup / 2.0)
        alj(2) = - log(reval(3)**2 + aival(3)**2) * (nsup / 2.0)
        alj(3) = - log(reval(5)**2 + aival(5)**2) * (nsup / 2.0)
 
*---- Damping partition numbers.
        sal = 2.0 * en0 / u0
        pdamp(1) = alj(1) * sal
        pdamp(2) = alj(2) * sal
        pdamp(3) = alj(3) * sal
 
*---- Emittances.
        clg = ((55.0 * hbar * clight) / (96.0 * sqrt(three)))
     +      * ((nsup * arad * gammas**5) / amass)
        ex = clg * sum(1) / alj(1)
        ey = clg * sum(2) / alj(2)
        et = clg * sum(3) / alj(3)
 
*---- Damping constants per second and damping times.
        f0 = freq0 * tenp6
        alj(1) = abs(alj(1) * f0)
        alj(2) = abs(alj(2) * f0)
        alj(3) = abs(alj(3) * f0)
        tau(1) = 1.0 / alj(1)
        tau(2) = 1.0 / alj(2)
        tau(3) = 1.0 / alj(3)
      endif
 
*---- TRANSPORT sigma matrix.
      call emce2i(em, ex, ey, et, sigma)
 
*---- Extents at interaction point.
      do 30 j = 1, 3
        j1 = 2 * j -1
        j2 = 2 * j
        do 20 k = 1, 3
          k1 = 2 * k - 1
          k2 = 2 * k
          bstar(j,k) = em(j1,k1) * em(j1,k1) + em(j1,k2) * em(j1,k2)
          gstar(j,k) = em(j2,k1) * em(j2,k1) + em(j2,k2) * em(j2,k2)
   20   continue
   30 continue
 
*---- Store BEAM common to BEAM bank, ET is given.
      exn = ex * (4.0 * betas * gammas)
      eyn = ey * (4.0 * betas * gammas)
      ietflg = 1
      sigx = sqrt(abs(sigma(1,1)))
      sigy = sqrt(abs(sigma(3,3)))
      if (sigma(5,5) .gt. 0.d0 .or. sigma(6,6) .gt. 0.d0)  then
        sigt = sqrt(abs(sigma(5,5)))
        sige = sqrt(abs(sigma(6,6)))
      endif
      call enput
 
*---- Summary output; header and global parameters.
      call prhead(comand, title, deltas, 0, nline, 1)
      call enprgl
 
*---- Dynamic case.
      expr = ex * tenp6
      eypr = ey * tenp6
      etpr = et * tenp6
      if (stabt) then
        if (frad) write (iqpr2, 910) ten3p * u0
        write (iqpr2, 920) 1, 2, 3
        write (iqpr2, 930) qx, qy, qs
        if (frad) write (iqpr2, 940) tune
        write (iqpr2, 950) ((bstar(j,k), j = 1, 3), k = 1, 3),
     +                     ((gstar(j,k), j = 1, 3), k = 1, 3),
     +                     ((bmax(j,k), j = 1, 3), k = 1, 3),
     +                     ((gmax(j,k), j = 1, 3), k = 1, 3)
        if (frad) then
          write (iqpr2, 960) pdamp, alj, (tau(j), j = 1, 3),
     +                       expr, eypr, etpr
        endif
      else
        write (iqpr2, 920) 1, 2
        write (iqpr2, 930) qx, qy
        write (iqpr2, 970) ((bstar(j,k), j = 1, 2), k = 1, 2),
     +                     ((gstar(j,k), j = 1, 2), k = 1, 2),
     +                     ((bmax(j,k), j = 1, 2), k = 1, 2),
     +                     ((gmax(j,k), j = 1, 2), k = 1, 2)
      endif
 
*---- RF system.
      call enprrf
 
  910 format(t6,'U0',t16,f14.6,' [MeV/turn]')
  920 format(' '/' ',t42,3(9x,'M o d e',3x,i1:))
  930 format(' Fractional tunes',t30,'undamped',t42,3f20.8)
  940 format(' ',t30,'damped',t42,3f20.8)
  950 format(' '/' beta* [m]',t30,'x',t42,3e20.8/t30,'y',t42,3e20.8/
     +  t30,'t',t42,3e20.8/
     +  ' '/' gamma* [1/m]',t30,'px',t42,3e20.8/t30,'py',t42,3e20.8/
     +  t30,'pt',t42,3e20.8/
     +  ' '/' beta(max) [m]',t30,'x',t42,3e20.8/t30,'y',t42,3e20.8/
     +  t30,'t',t42,3e20.8/
     +  ' '/' gamma(max) [1/m]',t30,'px',t42,3e20.8/t30,'py',t42,3e20.8/
     +  t30,'pt',t42,3e20.8)
  960 format(' '/' Damping partition numbers',t42,3f20.8/
     +  ' Damping constants [1/s]',t46,3e20.8/
     +  ' Damping times [s]',t46,3e20.8/
     +  ' Emittances [pi micro m]',t42,3e20.8)
  970 format(' '/' beta* [m]',t30,'x',t42,2e20.8/t30,'y',t42,2e20.8/
     +  ' '/' gamma* [1/m]',t30,'px',t42,2e20.8/t30,'py',t42,2e20.8/
     +  ' '/' beta(max) [m]',t30,'x',t42,2e20.8/t30,'y',t42,2e20.8/
     +  ' '/' gamma(max) [1/m]',t30,'px',t42,2e20.8/t30,'py',t42,2e20.8)
 
  999 end
