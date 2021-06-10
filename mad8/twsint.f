      subroutine twsint(betax, betay, alx, dx, dpx, txi, tyi, tli)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Subroutine uses Simpson's rule integration                         *
*   to calculate Bjorken/Mtingwa integrals (eqn. 3.4)                  *
*   Particle Accelerators 13, 115 (1983)                               *
*                                                                      *
*   Equivalent expressions are found in Conte/Martini                  *
*   Particle Accelerators 17, 1 (1985)                                 *
*                                                                      *
*   Integrals are broken into decades to optimize speed.               *
*                                                                      *
*   For the VAX, values may not exceed 10**33, therefore TSTLOG=33     *
*   For the IBM, values may not exceed 10**74, therefore TSTLOG=74     *
*   (PMG, March 1988)                                                  *
*                                                                      *
*   The integral is split into MAXDEC decades with NS steps /decade.   *
*   TEST is used for testing convergence of the integral               *
* Input:                                                               *
*   BETAX     (real)    Horizontal beta.                               *
*   BETAY     (real)    Vertical beta.                                 *
*   ALX       (real)    Horizontal alpha.                              *
*   DX        (real)    Horizontal dispersion.                         *
*   DPX       (real)    Derivative of horizontal dispersion.           *
* Output:                                                              *
*   TXI       (real)    Horizontal rate / const.                       *
*   TYI       (real)    Vertical rate / const.                         *
*   TLI       (real)    Longitudinal rate / const.                     *
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
      integer iiz,iloop,maxdec,ns
      double precision a,al,alam,aloop,alx,am,b,betax,betay,bl,c1,c2,c3,
     +ccy,chklog,cl,coeff,cof,cprime,cscale,cx,cy,dpx,dx,f,func,h,phi,
     +polyl,polyx,polyy,power,r1,suml,sumx,sumy,td1,td2,ten,term,test,
     +tl1,tl2,tli,tmpl,tmpx,tmpy,tstlog,tx1,tx2,txi,ty1,ty2,tyi,zintl,
     +zintx,zinty
 
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
 
      parameter         (tstlog = 74.0)
      parameter         (power  = - 2.0d0 / 3.0d0)
      parameter         (maxdec = 30, ns = 50)
      parameter         (ten = 10.0, test = 1.0d-7)
      dimension         al(31), bl(30), coeff(2)
      data coeff        / 2.0, 4.0 /
 
      phi    = dpx + (alx * dx / betax)
      am     = 1.0
      c1     = (gammas * dx)**2 / (ex * betax)
      c3     = betax / ex
      c2     = c3 * (gammas*phi)**2
      cx     = c1 + c2
      cl     = am * (gammas/sige)**2
      cy     = betay / ey
      r1     = 3.0 / cy
      a      = cx + cl
      b      = (c3 + cy) * (c1 + cl) + cy * c2
 
*---- Define CPRIME=C*CSCALE to try to keep the value.
*     small enough for the VAX in single precision or
*     IBM in double precision.
*     Test LOG(C) to see if it needs scaling
      cscale = 1.0
      chklog = log10(c3) + log10(cy) + log10(c1 + cl)
      if (chklog .gt. tstlog) cscale = ten**(tstlog-chklog)
      cprime = c3 * cy * cscale * (c1 + cl)
 
*---- Split integral into decades, with NS steps per decade.
*     variables to save integral segments
      zintl  = 0.0
      zintx  = 0.0
      zinty  = 0.0
 
*---- Constants for integration loop.
*     To keep the numbers reasonable, the numerator is
*     scaled by 1/CPRIME and the denominator by 1/CPRIME**2.
*     The extra factor of CPRIME is accounted for after integrating
      ccy    = cprime**power
      td1    = (a + c3) * ccy
      td2    = 1.0 / (sqrt(ccy) * cscale * cy)
      tl1    = (2.0 * a - cy - c3) / cprime
      tl2    = (b - 2.0 * c3 * cy) / cprime
      ty1    = (- a - c3 + 2.0 * cy) / cprime
      ty2    = (b + cy * c3) / cprime - r1 / cscale
      tx1    = (2.0 * a * (cx - c3) - cy * cx -
     +   c3 * (cy - cl - 2.0 * c3 - 6.0 * c2)) / cprime
      tx2    = (c3 + cx) * ((b + c3 * cy) / cprime) -
     +   6.0 / cscale + 3.0 * c3 * cy * (cl / cprime)
 
      al(1)  = 0.0
 
      do 90 iloop = 1, maxdec
        bl(iloop) = ten**iloop
        al(iloop+1) = bl(iloop)
        h = (bl(iloop) - al(iloop)) / ns
        aloop = al(iloop)
 
*---- Evaluate Simpson's rule summation for one interval.
*     The integrand is calculated in the loop itself
        term = sqrt((cy + aloop) * ccy) *
     +       sqrt(aloop * ccy * aloop + td1 * aloop + td2)
        func = sqrt(aloop) / term**3
        polyl = tl1 * aloop + tl2
        polyx = tx1 * aloop + tx2
        polyy = ty1 * aloop + ty2
        suml = func * polyl
        sumx = func * polyx
        sumy = func * polyy
 
        do 10 iiz = 1, ns
          alam = aloop + iiz * h
          cof = coeff(mod(iiz,2)+1)
          term = sqrt((cy+alam)*ccy) *
     +         sqrt(alam*ccy*alam+td1*alam+td2)
          f = sqrt(alam) / term**3
          polyl = tl1 * alam + tl2
          polyx = tx1 * alam + tx2
          polyy = ty1 * alam + ty2
 
          suml = suml + cof * f * polyl
          sumx = sumx + cof * f * polyx
          sumy = sumy + cof * f * polyy
   10   continue
 
        suml = suml - f * polyl
        sumx = sumx - f * polyx
        sumy = sumy - f * polyy
        tmpl = (suml / 3.0) * h
        tmpx = (sumx / 3.0) * h
        tmpy = (sumy / 3.0) * h
        zintl = zintl + tmpl
        zintx = zintx + tmpx
        zinty = zinty + tmpy
 
*---- Test to see if integral has converged.
        if (abs(tmpl/zintl) .lt. test .and.
     +      abs(tmpx/zintx) .lt. test .and.
     +      abs(tmpy/zinty) .lt. test) go to 100
   90 continue
      write (msg, 910) maxdec
  910 format('Bjorken/Mtingwa integrals did not converge in ',
     +       i3,' decades.')
      call aawarn('TWSINT', 1, msg)
  100 continue
 
*---- Divide answers by cprime to account for scaling.
      txi    =      (zintx / cprime)
      tli    = cl * (zintl / cprime)
      tyi    = cy * (zinty / cprime)
 
      end
