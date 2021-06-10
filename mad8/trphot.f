      subroutine trphot(el, curv, rfac)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate random energy loss for photons, using a look-up table to  *
*   invert the function Y.  Ultra-basic interpolation computed;        *
*   leads to an extrapolation outside the table using the two outmost  *
*   point on each side (low and high).                                 *
*   Assumes ultra-relativistic particles (beta = 1).                   *
* Author: Ghislain Roy                                                 *
* Input:                                                               *
*   EL     (real)       Element length.                                *
*   CURV   (real)       Local curvature of orbit.                      *
* Output:                                                              *
*   RFAC   (real)       Relative energy loss due to photon emissions.  *
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
      integer i,ierror,j,maxtab,nphot
      double precision amean,curv,dexp,dlogr,el,frndm,rfac,scalen,
     +scaleu,slope,tabxi,taby,ucrit,xi
      real real_am
      character * 20 text
*---- Table definition.
      parameter         (maxtab = 101)
      dimension         taby(maxtab), tabxi(maxtab)
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
      parameter (scalen = 5.0d0 / (12.0d0 * hbar * clight))
      parameter (scaleu = hbar * 3.0d0 * clight / 2.0d0)
 
      data (taby(i), i = 1, 52)
     +/ -1.14084005d0,  -0.903336763d0, -0.769135833d0, -0.601840854d0,
     +  -0.448812515d0, -0.345502228d0, -0.267485678d0, -0.204837948d0,
     +  -0.107647471d0, -0.022640628d0,  0.044112321d0,  0.0842842236d0,
     +   0.132941082d0,  0.169244036d0,  0.196492359d0,  0.230918407d0,
     +   0.261785239d0,  0.289741248d0,  0.322174788d0,  0.351361096d0,
     +   0.383441716d0,  0.412283719d0,  0.442963421d0,  0.472622454d0,
     +   0.503019691d0,  0.53197819d0,   0.561058342d0,  0.588547111d0,
     +   0.613393188d0,  0.636027336d0,  0.675921738d0,  0.710166812d0,
     +   0.725589216d0,  0.753636241d0,  0.778558254d0,  0.811260045d0,
     +   0.830520391d0,  0.856329501d0,  0.879087269d0,  0.905612588d0,
     +   0.928626955d0,  0.948813677d0,  0.970829248d0,  0.989941061d0,
     +   1.0097903d0,    1.02691281d0,   1.04411256d0,   1.06082714d0,
     +   1.0750246d0,    1.08283985d0,   1.0899564d0,    1.09645379d0 /
      data (taby(i), i = 53, 101)
     +/  1.10352755d0,   1.11475027d0,   1.12564385d0,   1.1306442d0,
     +   1.13513422d0,   1.13971806d0,   1.14379156d0,   1.14741969d0,
     +   1.15103698d0,   1.15455759d0,   1.15733826d0,   1.16005647d0,
     +   1.16287541d0,   1.16509759d0,   1.16718769d0,   1.16911888d0,
     +   1.17075884d0,   1.17225218d0,   1.17350936d0,   1.17428589d0,
     +   1.17558432d0,   1.17660713d0,   1.17741513d0,   1.17805469d0,
     +   1.17856193d0,   1.17896497d0,   1.17928565d0,   1.17954147d0,
     +   1.17983139d0,   1.1799767d0,    1.18014216d0,   1.18026078d0,
     +   1.18034601d0,   1.1804074d0,    1.18045175d0,   1.1804837d0,
     +   1.18051291d0,   1.18053186d0,   1.18054426d0,   1.18055236d0,
     +   1.18055761d0,   1.18056166d0,   1.18056381d0,   1.1805656d0,
     +   1.18056655d0,   1.18056703d0,   1.18056726d0,   1.1805675d0,
     +   1.18056762d0 /
 
      data (tabxi(i), i = 1, 52)
     +/ -7.60090017d0,  -6.90775537d0,  -6.50229025d0,  -5.99146461d0,
     +  -5.52146101d0,  -5.20300722d0,  -4.96184492d0,  -4.76768923d0,
     +  -4.46540833d0,  -4.19970512d0,  -3.98998451d0,  -3.86323285d0,
     +  -3.70908213d0,  -3.59356928d0,  -3.50655794d0,  -3.39620972d0,
     +  -3.29683733d0,  -3.20645332d0,  -3.10109282d0,  -3.0057826d0,
     +  -2.9004221d0,   -2.80511189d0,  -2.70306253d0,  -2.60369015d0,
     +  -2.50103593d0,  -2.4024055d0 ,  -2.30258512d0,  -2.20727491d0,
     +  -2.12026358d0,  -2.04022098d0,  -1.89712d0   ,  -1.7719568d0,
     +  -1.71479833d0,  -1.60943794d0,  -1.51412773d0,  -1.38629436d0,
     +  -1.30933332d0,  -1.20397282d0,  -1.10866261d0,  -0.99425226d0,
     +  -0.89159810d0,  -0.79850775d0,  -0.69314718d0,  -0.59783697d0,
     +  -0.49429631d0,  -0.40047753d0,  -0.30110508d0,  -0.19845095d0,
     +  -0.10536054d0,  -0.05129330d0,   0.0d0,          0.048790119d0 /
      data (tabxi(i), i = 53, 101)
     +/  0.104360029d0,  0.198850885d0,  0.300104618d0,  0.350656837d0,
     +   0.398776114d0,  0.451075643d0,  0.500775278d0,  0.548121393d0,
     +   0.598836541d0,  0.652325153d0,  0.69813472d0 ,  0.746687889d0,
     +   0.802001595d0,  0.850150883d0,  0.900161386d0,  0.951657832d0,
     +   1.00063193d0,   1.05082154d0,   1.09861231d0,   1.13140213d0,
     +   1.1939224d0,    1.25276291d0,   1.3083328d0,    1.36097658d0,
     +   1.4109869d0,    1.45861506d0,   1.50407743d0,   1.54756248d0,
     +   1.60943794d0,   1.64865863d0,   1.70474803d0,   1.75785792d0,
     +   1.80828881d0,   1.85629797d0,   1.90210748d0,   1.9459101d0,
     +   2.0014801d0,    2.05412364d0,   2.10413408d0,   2.15176225d0,
     +   2.19722462d0,   2.25129175d0,   2.29253483d0,   2.35137534d0,
     +   2.40694523d0,   2.45100522d0,   2.501436d0,     2.60268974d0,
     +   2.64617491d0 /
 
*---- AMEAN is the average number of photons emitted.,
*     NPHOT is the integer number generated from Poisson's law.
      amean = scalen * abs(arad*pc*(1.0+deltas)*el*curv) * sqrt(3.0)
      rfac = 0.0d0
      real_am = amean
      if (real_am .gt. 0.0) then
        call poissn(real_am, nphot, ierror)
 
        if (ierror .ne. 0) then
          write(text, '(1p,d20.12)') amean
          call aafail('TRPHOT', 1,
     +    'Fatal: Poisson input mean =' // text)
        endif
 
*---- For all photons, sum the radiated photon energy,
*     in units of UCRIT (relative to total energy).
 
        if (nphot .ne. 0) then
          ucrit = scaleu * gammas**2 * abs(curv) / amass
          xi = 0.0d0
          do 30 i = 1, nphot
 
*---- Find a uniform random number in the range [ 0,3.256223 ].
*     Note that the upper limit is not exactly 15*sqrt(3)/8
*     because of imprecision in the integration of F.
            dlogr = log(3.256223d0 * frndm())
 
*---- Now look for the energy of the photon in the table TABY/TABXI
            do 10 j = 2, maxtab
              if (dlogr .le. taby(j) ) go to 20
 10         continue
 
*---- Perform linear interpolation and sum up energy lost.
 20         slope = (dlogr - taby(j-1)) / (taby(j) - taby(j-1))
            xi = dexp(tabxi(j-1) + slope * (tabxi(j) - tabxi(j-1)))
            rfac = rfac + ucrit * xi
   30     continue
        endif
      endif
 
      end
