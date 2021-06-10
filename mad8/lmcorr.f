      subroutine lmcorr(nord, el, dpx, dpy, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a closed orbit corrector.                    *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Corrector length.                              *
*   DPX       (real)    Horizontal kick.                               *
*   DPY       (real)    Vertical kick.                                 *
* Output:                                                              *
*   FP, FM    (map)     Element map.                                   *
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
      integer nord
      double precision bil4,dppx,dppy,dpt,dpx,dpy,el,el2,fm,fp,one
      dimension         fp(*), fm(6,6)
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
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
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
 
      parameter         (one = 1.0d0)
 
*---- Set up identity map.
      call lmone(nord, fp, fm)
 
*---- Kicks.
      dppx = dpx / (one + deltas)
      dppy = dpy / (one + deltas)
      el2 = el / 2.0
      bil4 = el2 / (2.0 * betas)
      fp(1) =   dppx
      fp(2) = - el2 * dppx
      fp(3) =   dppy
      fp(4) = - el2 * dppy
      fp(6) =   bil4 * (dppx**2 + dppy**2) - el*dtbyds
 
*---- Radiation loss.
      if (dorad .and. el .ne. 0.0) then
        dpt = - (2.0 * arad * charge * gammas**3) * (dppx**2 + dppy**2)
     +  / (3.0 * el)
        fp(1) = dppx - dpt * dppx / (2.0 * betas)
        fp(3) = dppy - dpt * dppy / (2.0 * betas)
        fp(5) = dpt
      endif
 
*---- First-order terms (matrix).
      fm(1,2) =   el
      fm(1,6) = fp(2) / betas
      fm(5,2) = - fm(1,6)
      fm(3,4) =   el
      fm(3,6) = fp(4) / betas
      fm(5,4) = - fm(3,6)
      fm(5,6) = el / (betas*gammas)**2
 
*---- F3 polynomial.
      if (nord .ge. 3) then
        fp(53) = el / (2.0 * betas)
        fp(76) = el / (2.0 * betas)
        fp(83) = fm(5,6) / (2.0 * betas)
      endif
 
*---- F4 polynomial.
      if (nord .ge. 4) then
        fp(140) = - el / 8.0
        fp(149) = - el / 4.0
        fp(195) = - el / 8.0
        fp(154) = el * (1.0 - 3.0 / betas**2) / 4.0
        fp(200) = el * (1.0 - 3.0 / betas**2) / 4.0
        fp(209) = fm(5,6) * (1.0 - 5.0 / betas**2) / 8.0
      endif
 
      end
