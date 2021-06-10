      subroutine mtmig1(fcn, nf, nx, x, dx, fvec, covar, wa)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Minimization by MIGRAD method by Davidon/Fletcher/Powell.          *
*   (Computer Journal 13, 317 (1970).                                  *
* Input:                                                               *
*   FCN       (subr)    Returns value of penalty function.             *
*   NF        (integer) Number of functions.                           *
*   NX        (integer) Number of parameters.                          *
*   X(NX)     (real)    Parameter values. On output, best estimate.    *
*   DX(NX)    (real)    Parameter errors. On output, error estimate.   *
* Output:                                                              *
*   FVEC(NF)  (real)    Vector of function values in best point.       *
* Working arrays:                                                      *
*   COVAR(NX,NX)        Covariance matrix.                             *
*   WA(NX,7)            Working vectors.                               *
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
      integer i,iflag,improv,iter,j,level,mflnu,mg2,mgrd,mgsave,mvg,
     +mxsave,nf,npsdf,nrstrt,nx
      double precision covar,d,delgam,dgi,dx,eps1,eps2,fvec,gdel,gssq,
     +gvg,half,sum,two,vdot,vgi,wa,x
      external          fcn
      logical           eflag
      dimension         x(nx), dx(nx), fvec(nf), covar(nx,nx), wa(nx,7)
 
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
 
      parameter         (two = 2.0d0, half = 0.5d0)
      parameter         (eps1 = 1.0d-3, eps2 = 1.0d-4)
 
      parameter         (mgrd = 1, mg2 = 2, mvg = 3)
      parameter         (mflnu = 5, mgsave = 6, mxsave = 7)
 
*---- Initialize penalty function.
      call fcn(nf, nx, x, fvec, iflag)
      nfcn = nfcn + 1
      if (iflag .ne. 0) then
        msg(1) = 'Matching stopped -- start point seems to be unstable,'
        msg(2) = '(Maybe a "LINE = ..." condition is unstable).'
        call aawarn('MTMIG1', 2, msg)
        cstat = 'unstable'
        go to 500
      endif
      fmin = vdot(nf, fvec, fvec)
      edm = fmin
 
*---- Start MIGRAD algorithm.
      crout = 'MIGRAD'
      cstat = 'start'
      nrstrt = 0
      npsdf = 0
 
*---- Come here to restart algorithm.
  100 continue
      if (istrat .eq. 2  .or.  istrat.gt.2 .and. icovar.lt.2) then
        call mthess(fcn, nf, nx, covar, x, wa(1,mgrd), wa(1,mg2), fvec,
     +              wa(1,mvg))
        npsdf = 0
      else
        call mtderi(fcn, nf, nx, x, wa(1,mgrd), wa(1,mg2), fvec)
        if (icovar .lt. 2) then
          do 120 i = 1, nx
            do 110 j = 1, nx
              covar(i,j) = 0.0
  110       continue
            if (wa(i,mg2) .eq. 0.0) wa(i,mg2) = 1.0
            covar(i,i) = 1.0 / wa(i,mg2)
  120     continue
        endif
      endif
 
*---- Initialize for first iteration.
      improv = 0
      edm = 0.0
      do 160 i = 1, nx
        sum = 0.0
        do 150 j = 1, nx
          sum = sum + covar(i,j) * wa(j,mgrd)
  150   continue
        edm = edm + sum * wa(i,mgrd)
  160 continue
      edm = min(half * edm, fmin)
 
*---- Print after initialization.
      if (ilevel .ge. 1) then
        call mtprnt(nx, x)
      endif
      iter = 0
 
*==== Start main iteration loop: Check for call limit.
  200 if (nfcn .lt. nfcnmx) then
 
*---- Find step size according to Newton's method.
        gdel = 0.0
        gssq = 0.0
        do 220 i = 1, nx
          sum = 0.0
          wa(i,mgsave) = wa(i,mgrd)
          gssq = gssq + wa(i,mgrd)**2
          do 210 j = 1, nx
            sum = sum + covar(i,j) * wa(j,mgrd)
  210     continue
          dx(i) = - sum
          gdel = gdel + dx(i) * wa(i,mgrd)
  220   continue
 
*---- First derivatives all zero?
        if (gssq .eq. 0.0) go to 400
 
*---- If GDEL .GE. 0 matrix is not positive definite.
        if (gdel .ge. 0.0) then
          cstat = 'not posdef'
          if (npsdf .eq. 0) then
            call symsol(covar, nx, eflag)
            call mtpsdf(covar, nx)
            call symsol(covar, nx, eflag)
            npsdf = 1
            go to 200
          else
            nrstrt = nrstrt + 1
            if (nrstrt .gt. istrat) go to 500
            go to 100
          endif
        endif
 
*---- Search for minimum along predicted line.
        call mtline(fcn, nf, nx, x, dx, fvec, wa(1,mxsave), iflag)
 
*---- No improvement found.
        if (iflag .ne. 0) then
          if (edm .lt. eps1 * tol) go to 400
          cstat = 'accuracy limit'
          if (edm .lt. two * epsmch * fmin) go to 500
          if (istrat .eq. 0  .and.  nrstrt .eq. 0) then
            istrat = 1
            nrstrt = 1
            cstat = 'restart'
            go to 100
          endif
          cstat = 'failed'
          go to 500
        endif
 
*---- Find gradient in new point.
        call mtderi(fcn, nf, nx, x, wa(1,mgrd), wa(1,mg2), fvec)
        npsdf = 0
 
*---- Estimated distance to minimum.
  300   continue
        edm = 0.0
        gvg = 0.0
        delgam = 0.0
        do 320 i = 1, nx
          vgi = 0.0
          sum = 0.0
          do 310 j = 1, nx
            vgi = vgi + covar(i,j) * (wa(j,mgrd) - wa(j,mgsave))
            sum = sum + covar(i,j) * wa(j,mgrd)
  310     continue
          wa(i,mvg) = vgi
          dgi = wa(i,mgrd) - wa(i,mgsave)
          gvg = gvg + vgi * dgi
          delgam = delgam + dx(i) * dgi
          edm = edm + sum * wa(i,mgrd)
  320   continue
        edm = min(half * edm, fmin)
 
*---- Test for convergence and print-out.
        if (edm .ge. 0.0  .and.  edm .lt. eps2 * tol) go to 400
        iter = iter + 1
        level = 3
        if (mod(iter,10) .eq. 0) level = 2
        if (ilevel .ge. level) then
          cstat = 'progress'
          call mtprnt(nx, x)
        endif
 
*---- Force positive definiteness.
        if (edm .lt. 0.0  .or. gvg .le. 0.0) then
          cstat = 'not posdef'
          icovar = 0
          if (npsdf .eq. 1) go to 500
          call symsol(covar, nx, eflag)
          call mtpsdf(covar, nx)
          call symsol(covar, nx, eflag)
          npsdf = 1
          go to 300
        endif
 
*---- Update covariance matrix.
        do 330 i = 1, nx
        do 330 j = 1, nx
          d = dx(i) * dx(j) / delgam - wa(i,mvg) * wa(j,mvg) / gvg
          covar(i,j) = covar(i,j) + d
  330   continue
        if (delgam .gt. gvg) then
          do 340 i = 1, nx
            wa(i,mflnu) = dx(i) / delgam - wa(i,mvg) / gvg
  340     continue
          do 350 i = 1, nx
          do 350 j = 1, nx
            d = gvg * wa(i,mflnu) * wa(j,mflnu)
            covar(i,j) = covar(i,j) + d + d
  350     continue
        endif
        improv = improv + 1
        if (improv .ge. nx) icovar = 3
        go to 200
      endif
 
*---- Call limit reached.
      cstat = 'call limit'
      icall = 1
      go to 500
 
*==== End of main iteration loop; Check covariance matrix.
  400 continue
      if (istrat .ge. 2  .or.  (istrat.eq.1 .and. icovar.lt.3)) then
        cstat = 'verify'
        call mthess(fcn, nf, nx, covar, x, wa(1,mgrd), wa(1,mg2), fvec,
     +              wa(1,mvg))
        npsdf = 0
        cstat = 'restart'
        if (edm .gt. eps1 * tol) go to 100
      endif
      cstat = 'converged'
      if (icovar .eq. 2) cstat = 'not posdef'
      if (icovar .lt. 2) cstat = 'uncertain'
 
*---- Common exit point; final print-out.
  500 continue
      call mtputi(nx, x)
      if (ilevel .ge. 1) call mtprnt(nx, x)
 
      end
