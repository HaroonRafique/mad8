      subroutine mtsim1(fcn, nf, nx, x, dx, fvec, psim, fsim, wa)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Minimization using the SIMPLEX method by Nelder and Mead.          *
*   (Computer Journal 7, 308 (1965).                                   *
* Input:                                                               *
*   FCN       (subr)    Returns value of function to be minimized.     *
*   NF        (integer) Number of functions.                           *
*   NX        (integer) Number of parameters.                          *
*   X(NX)     (real)    Parameter values. On output, best estimate.    *
*   DX(NX)    (real)    Parameter errors. On output, error estimate.   *
* Output:                                                              *
*   FVEC(NF)  (real)    Vector of function values in best point.       *
* Working arrays:                                                      *
*   PSIM(NX,0:NX)       Coordinates of simplex vertices.               *
*   FSIM(0:NX)          Function values in simplex vertices.           *
*   WA(NX,4)            Working vectors.                               *
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
      integer i,idir,iflag,j,jh,jhold,jl,k,level,mbar,mrho,mstar,mstst,
     +ncycl,nf,nrstrt,ns,nx
      double precision alpha,beta,dx,eps,f,f1,f2,fbar,fbest,frho,
     +fsim,fstar,fstst,fvec,gamma,pb,pbest,pmax,pmin,psim,rho,rho1,rho2,
     +rhomax,rhomin,step,two,vdot,wa,x
      external          fcn
      dimension         x(nx), dx(nx), fvec(nf)
      dimension         psim(nx,0:nx), fsim(0:nx), wa(nx,4)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
 
      parameter         (alpha  = 1.0d0)
      parameter         (beta   = 0.5d0)
      parameter         (gamma  = 2.0d0)
      parameter         (rhomin = 4.0d0, rhomax = 8.0d0)
      parameter         (rho1   = 1.0d0 + alpha)
      parameter         (rho2   = rho1 + alpha * gamma)
      parameter         (eps    = 1.0d-8)
      parameter         (two    = 2.0d0)
 
      parameter         (mbar = 1, mstar = 2, mstst = 3, mrho = 4)
 
*---- Initialize penalty function.
      crout = 'SIMPLEX'
      cstat = 'start'
      call fcn(nf, nx, x, fvec, iflag)
      nfcn = nfcn + 1
      if (iflag .ne. 0) then
        msg(1) = 'Matching stopped -- start point seems to be unstable,'
        msg(2) = '(Maybe a "LINE = ..." condition is unstable).'
        call aawarn('MTSIM1', 2, msg)
        cstat = 'unstable'
        go to 400
      endif
      fmin = vdot(nf, fvec, fvec)
      edm = fmin
      nrstrt = 0
 
*---- Choose the initial simplex using single-parameter searches.
*     Keep initial point in PBAR.
  100 continue
      if (ilevel .ge. 1) call mtprnt(nx, x)
      fbar = fmin
      do 180 i = 1, nx
        wa(i,mbar) = x(i)
        pbest = x(i)
        fbest = fmin
        step  = dx(i)
 
*---- Find proper initial direction and step.
        do 110 idir = 1, 12
          x(i) = pbest + step
          call fcn(nf, nx, x, fvec, iflag)
          nfcn = nfcn + 1
          if (iflag .eq. 0) then
            f = vdot(nf, fvec, fvec)
            if (f .le. fbest) go to 120
          endif
          if (mod(idir,2) .eq. 0) step = 0.1 * step
          step = - step
  110   continue
        go to 160
 
*---- Improvement found; attempt increasing steps.
  120   continue
        do 130 ns = 1, 3
          pbest = x(i)
          fbest = f
          step = step * 3.0
          x(i) = x(i) + step
          call fcn(nf, nx, x, fvec, iflag)
          nfcn = nfcn + 1
          if (iflag .ne. 0) go to 140
          f = vdot(nf, fvec, fvec)
          if (f .gt. fbest) go to 140
  130   continue
        go to 160
 
*---- Backtrack to best point.
  140   continue
        x(i) = pbest
        f = fbest
 
*---- Store local minimum in i'th direction.
  160   continue
        fsim(i) = f
        do 170 k = 1, nx
          psim(k,i) = x(k)
  170   continue
  180 continue
 
*---- Store initial point as 0'th vertex.
      jh = 0
      call mtrazz(nvar, fbar, wa(1,mbar), fsim, psim, jh, jl)
 
*---- Extract best point.
      do 190 i = 1, nx
        x(i) = psim(i,jl)
  190 continue
 
*---- Print-out after setting up simplex.
      if (ilevel .ge. 2) then
        cstat = 'progress'
        call mtprnt(nx, x)
      endif
      ncycl = 0
 
*==== Start main loop.
  200 continue
      if (edm .lt. tol) then
        cstat = 'converged'
      else if (nfcn .gt. nfcnmx) then
        cstat = 'call limit'
        icall = 1
      else
 
*---- Calculate PBAR and P*.
        do 220 i = 1, nx
          pb = psim(i,0)
          do 210 j = 1, nx
            pb = pb + psim(i,j)
  210     continue
          wa(i,mbar) = (pb - psim(i,jh)) / float(nx)
          wa(i,mstar) = wa(i,mbar) + alpha * (wa(i,mbar) - psim(i,jh))
  220   continue
        call fcn(nf, nx, wa(1,mstar), fvec, iflag)
        nfcn = nfcn + 1
        if (iflag .ne. 0) then
          fstar = two * fsim(jh)
        else
          fstar = vdot(nf, fvec, fvec)
        endif
 
*---- Point P* is better than point PSIM(*,JL).
        jhold = jh
        if (fstar .lt. fsim(jl)) then
 
*---- Try expanded point P**.
          do 230 i = 1, nx
            wa(i,mstst) = wa(i,mbar) + gamma * (wa(i,mstar)-wa(i,mbar))
  230     continue
          call fcn(nf, nx, wa(1,mstst), fvec, iflag)
          nfcn = nfcn + 1
          if (iflag .ne. 0) then
            fstst = two * fsim(jh)
            rho = 0.0
          else
            fstst = vdot(nf, fvec, fvec)
 
*---- Fit a parabola through FSIM(JH), F*, F**; minimum = RHO.
            f1 = (fstar - fsim(jh)) * rho2
            f2 = (fstst - fsim(jh)) * rho1
            rho = 0.5 * (rho2 * f1 - rho1 * f2) / (f1 - f2)
          endif
 
*---- Minimum inexistent ot too close to PBAR;
*     Use P** if it gives improvement; otherwise use P*.
          if (rho .lt. rhomin) then
            if (fstst .lt. fsim(jl)) then
              call mtrazz(nvar, fstst, wa(1,mstst), fsim, psim,
     +        jh, jl)
            else
              call mtrazz(nvar, fstar, wa(1,mstar), fsim, psim,
     +        jh, jl)
            endif
 
*---- Usable minimum found.
          else
            if (rho .gt. rhomax) rho = rhomax
            do 240 i = 1, nx
              wa(i,mrho) = psim(i,jh) + rho * (wa(i,mbar) - psim(i,jh))
  240       continue
            call fcn(nf, nx, wa(1,mrho), fvec, iflag)
            nfcn = nfcn + 1
            if (iflag .ne. 0) then
              frho = two * fsim(jh)
            else
              frho = vdot(nf, fvec, fvec)
            endif
 
*---- Select farthest point which gives decent improvement.
            if (frho .lt. fsim(jl) .and. frho .lt. fstst) then
              call mtrazz(nvar, frho, wa(1,mrho), fsim, psim, jh, jl)
            else if (fstst .lt. fsim(jl)) then
              call mtrazz(nvar, fstst, wa(1,mstst), fsim, psim,
     +        jh, jl)
            else
              call mtrazz(nvar, fstar, wa(1,mstar), fsim, psim,
     +        jh, jl)
            endif
          endif
 
*---- F* is higher than FSIM(JL).
        else
          if (fstar .lt. fsim(jh)) then
            call mtrazz(nvar, fstar, wa(1,mstar), fsim, psim, jh, jl)
          endif
 
*---- If F* is still highest value, try contraction,
*     giving point P** = PWRK(*,3).
          if (jhold .eq. jh) then
            do 250 i = 1, nx
              wa(i,mstst) = wa(i,mbar)
     +                    + beta * (psim(i,jh) - wa(i,mbar))
  250       continue
            call fcn(nf, nx, wa(1,mstst), fvec, iflag)
            nfcn = nfcn + 1
            if (iflag .ne. 0) then
              fstst = two * fsim(jh)
            else
              fstst = vdot(nf, fvec, fvec)
            endif
 
*---- Restart algorithm, if F** is higher; otherwise use it.
            if (fstst .gt. fsim(jh)) then
              cstat = 'failed'
              if (nrstrt .ne. 0) go to 300
              nrstrt = 1
              cstat = 'restart'
              do 260 j = 1, nx
                x(j) = psim(j,jl)
  260         continue
              go to 100
            endif
            call mtrazz(nvar, fstst, wa(1,mstst), fsim, psim, jh, jl)
          endif
        endif
 
*---- New minimum found.
        if (jl .eq. jhold) then
          nrstrt = 0
          ncycl = ncycl + 1
          level = 3
          if (mod(ncycl,10) .eq. 0) level = 2
          if (ilevel .ge. level) then
            cstat = 'progress'
            call mtprnt(nx, x)
          endif
        endif
        go to 200
      endif
 
*==== End main loop: Try central point of simplex.
  300 continue
      do 320 i = 1, nx
        pmin = psim(i,0)
        pmax = psim(i,0)
        pb = psim(i,0)
        do 310 j = 1, nx
          pb = pb + psim(i,j)
  310   continue
        wa(i,mbar) = (pb - psim(i,jh)) / float(nx)
        dx(i) = pmax - pmin
  320 continue
      call fcn(nf, nx, wa(1,mbar), fvec, iflag)
      nfcn = nfcn + 1
      if (iflag .eq. 0) then
        fbar = vdot(nf, fvec, fvec)
        if (fbar .lt. fsim(jl)) then
          call mtrazz(nvar, fbar, wa(1,mbar), fsim, psim, jh, jl)
        endif
      endif
 
*---- Recompute step sizes and extract best point.
      do 340 i = 1, nx
        pmin = psim(i,0)
        pmax = psim(i,0)
        do 330 j = 1, nx
          pmin = min(psim(i,j),pmin)
          pmax = max(psim(i,j),pmax)
  330   continue
        dx(i) = pmax - pmin
        x(i) = psim(i,jl)
  340 continue
      fmin = fsim(jl)
      call mtputi(nx, x)
 
*---- Check for necessity to restart after final change.
      if (nfcn + 3 * nx .lt. nfcnmx  .and.  nrstrt .eq. 0  .and.
     +    fmin .gt. 2.0 * (tol + epsmch)) then
        nrstrt = 1
        cstat = 'restart'
        go to 100
      endif
 
*---- Final print-out.
  400 continue
      if (ilevel .ge. 1) then
        call mtprnt(nx, x)
      endif
 
      end
