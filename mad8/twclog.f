      subroutine twclog(bxbar, bybar, const)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Calculation of Coulomb logarithm (and print)                       *
*   based on the formulae in AIP physics vade mecum p.264 (1981)       *
* Input:                                                               *
*   BXBAR     (real)    Average horizontal beta.                       *
*   BYBAR     (real)    Average vertical beta.                         *
* Output:                                                              *
*   CONST     (real)    Constant in eq. (IV.9.1), ZAP user's manual.   *
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
      double precision bgam,bxbar,bybar,cbunch,const,coulog,debyel,
     +densty,etrans,pnbtot,qion,rmax,rmin,rmincl,rminqm,sigtcm,sigxcm,
     +sigycm,tempev,vol
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
 
*---- Calculate transverse temperature as 2*P*X',
*     i.e., assume the transverse energy is temperature/2.
      qion   = abs(charge)
      etrans = 0.5d9 * (gammas * en0 - amass) * (ex / bxbar)
      tempev = 2.0 * etrans
 
*---- Calculate beam volume to get density (in cm**-3).
      sigxcm = 100.0 * sqrt(ex * bxbar)
      sigycm = 100.0 * sqrt(ey * bybar)
      sigtcm = 100.0 * sigt
      if (fbch) then
        vol    = 8.0 * sqrt(pi**3) * sigxcm * sigycm * sigtcm
        densty = parnum / vol
      else
        vol    = 4.0 * pi * sigxcm * sigycm * 100.0 * circ
        pnbtot = currnt * circ / (qion * qelect * betas * clight)
        densty = pnbtot / vol
      endif
 
*---- Calculate RMAX as smaller of SIGXCM and DEBYE length.
      debyel = 743.40d0 * sqrt(tempev/densty) / qion
      rmax   = min(sigxcm,debyel)
 
*---- Calculate RMIN as larger of classical distance of closest approach
*     or quantum mechanical diffraction limit from nuclear radius.
      rmincl = 1.44d-7 * qion**2 / tempev
      rminqm = hbar*clight*1.0d5 / (2.0*sqrt(2.d-3*etrans*amass))
      rmin   = max(rmincl,rminqm)
      coulog = log(rmax/rmin)
      bgam = betas * gammas
      qion   = abs(charge)
      if (fbch) then
        const = parnum * coulog * arad**2 * clight / (8.0 * pi * betas
     +  **3 * gammas**4 * ex * ey * sige * sigt)
        cbunch = qion * parnum * qelect * betas * clight / circ
      else
        const = currnt * coulog * arad**2 /
     +  (4.0 * sqrt(pi) * qion * qelect * bgam**4 * ex * ey * sige)
      endif
      write (iqpr2, 910) const
 
      write (iqpr2, 920) en0, betas, gammas, coulog
 
*---- Print warning here if Coulomb logarithm gave bad results.
*     Usually this error is due to a starting guess far from
*     the equilibrium value.
      if (coulog .lt. 0.0) then
        call aawarn('TWCLOG', 1, 'Coulomb logarithm gives invalid'
     +  // ' result --- check input parameters.')
      endif
 
      write (iqpr2, 940) ex, ey
 
      if (fbch) then
        write (iqpr2, 950) sige, sigt, parnum, cbunch
      else
        write (iqpr2, 960) sige, currnt
      endif
 
  910 format(' '/5x,'CONST               = ',1p,e14.6)
  920 format(' '/5x,'ENERGY              = ',f14.6,' GeV'/
     +       5x,'BETA                = ',f14.6/
     +       5x,'GAMMA               = ',f14.3/
     +       5x,'COULOMB LOG         = ',f14.3)
  940 format(' '/5x,'X-emittance         = ',1p,e14.6,' m*rad'/
     +       5x,'Y-emittance         = ',   e14.6,' m*rad')
  950 format(' '/5x,'Momentum spread     = ',1p,e14.6/
     +       5x,'Bunch length        = ',0p,f14.6,' m'/' '/
     +       5x,'Particles per bunch = ',1p,e14.6/
     +       5x,'Bunch current       = ',1p,e14.6,' A')
  960 format(' '/5x,'Momentum spread     = ',1p,e14.6/' '/
     +       5x,'Current             = ',0p,f14.6,' A'/' ')
 
      end
