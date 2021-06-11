      subroutine mtline(fcn, nf, nx, x, dx, fvec, xsave, iflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Search for minimum along predicted direction.                      *
* Input:                                                               *
*   FCN       (subr)    Returns value of penalty function.             *
*   NF        (integer) Number of functions.                           *
*   NX        (integer) Number of parameters.                          *
*   X(NX)     (real)    Parameter values. On output, best estimate.    *
*   DX(NX)    (real)    Initial direction.                             *
*   FVEC(NF)  (real)    Function values.                               *
*   IFLAG     (integer) Error flag.                                    *
* Working array:                                                       *
*   XSAVE(NX) (real)    Save area for initial point.                   *
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
      integer i,iflag,ipt,maxpt,nf,npts,nvmax,nx
      double precision alpha,c1,c2,den,dx,f3,fval,fvec,fvmin,half,one,
     +overal,ratio,s13,s21,s32,slam,slamax,slambg,slamin,sval,svmin,
     +tol8,tol9,two,undral,vdot,x,xsave
      external          fcn
      dimension         x(nx), dx(nx), fvec(nf), xsave(nx)
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
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
 
      dimension         sval(3), fval(3)
 
      parameter         (alpha = 2.0d0, slambg = 5.0d0)
      parameter         (one = 1.0d0, two = 2.0d0, half = 0.5d0)
      parameter         (maxpt = 12)
 
*---- Initialize.
      overal = 1000.0
      undral = - 100.0
      sval(1) = 0.0
      fval(1) = fmin
      svmin = 0.0
      fvmin = fmin
      npts = 0
 
      slamin = 0.0
      do 10 i = 1, nx
        xsave(i) = x(i)
        if (dx(i) .ne. 0.0) then
          ratio = abs(x(i) / dx(i))
          if (slamin .eq. 0.0  .or.  ratio .lt. slamin) slamin = ratio
        endif
   10 continue
      if (slamin .eq. 0.0) slamin = epsmch
      slamin = slamin * epsmch
      slamax = slambg
      tol8 = 0.05d0
 
*---- Compute function for move by DX.
      slam = one
   20 continue
      sval(2) = slam
      do 30 i = 1, nx
        x(i) = xsave(i) + slam * dx(i)
   30 continue
      call fcn(nf, nx, x, fvec, iflag)
      nfcn = nfcn + 1
      npts = npts + 1
 
*---- If machine becomes unstable, cut step.
      if (iflag .ne. 0) then
        slam = half * slam
        if (slam .gt. slamin) go to 20
        go to 400
      endif
      fval(2) = vdot(nf, fvec, fvec)
      if (fval(2) .lt. fvmin) then
        svmin = sval(2)
        fvmin = fval(2)
      endif
      if (slam .lt. one) go to 400
 
*---- Compute function for move by 1/2 DX.
      slam = half * slam
      sval(3) = slam
      do 50 i = 1, nx
        x(i) = xsave(i) + slam * dx(i)
   50 continue
      call fcn(nf, nx, x, fvec, iflag)
      nfcn = nfcn + 1
      npts = npts + 1
      if (iflag .ne. 0) go to 400
      fval(3) = vdot(nf, fvec, fvec)
      if (fval(3) .lt. fvmin) then
        svmin = sval(3)
        fvmin = fval(3)
      endif
 
*---- Begin iteration.
  200 continue
        slamax = max(slamax, alpha * abs(svmin))
 
*---- Quadratic interpolation using three points.
        s21 = sval(2) - sval(1)
        s32 = sval(3) - sval(2)
        s13 = sval(1) - sval(3)
        den = s21 * s32 * s13
        c2 = (s32 * fval(1) + s13 * fval(2) + s21 * fval(3)) / den
        c1 = ((sval(3) + sval(2)) * s32 * fval(1) +
     +        (sval(1) + sval(3)) * s13 * fval(2) +
     +        (sval(2) + sval(1)) * s21 * fval(3)) / den
        if (c2 .ge. 0.0) then
          slam = svmin + sign(slamax, c1 - two * c2 * svmin)
        else
          slam = c1 / (two * c2)
          if (slam .gt. svmin + slamax) slam = svmin + slamax
          if (slam .le. svmin - slamax) slam = svmin - slamax
        endif
        if (slam .gt. 0.0) then
          if (slam .gt. overal) slam = overal
        else
          if (slam .lt. undral) slam = undral
        endif
 
*---- See if new point coincides with a previous one.
  300   continue
        tol9 = tol8 * max(one, slam)
        do 310 ipt = 1, 3
          if (abs(slam - sval(ipt)) .lt. tol9) go to 400
  310   continue
 
*---- Compute function for interpolated point.
        do 320 i = 1, nx
          x(i) = xsave(i) + slam * dx(i)
  320   continue
        call fcn(nf, nx, x, fvec, iflag)
        nfcn = nfcn + 1
        npts = npts + 1
        if (iflag .ne. 0) go to 400
        f3 = vdot(nf, fvec, fvec)
 
*---- Find worst point of previous three.
        nvmax = 1
        if (fval(2) .gt. fval(nvmax)) nvmax = 2
        if (fval(3) .gt. fval(nvmax)) nvmax = 3
 
*---- If no improvement, cut interval.
        if (f3 .ge. fval(nvmax)) then
          if (npts .ge. maxpt) go to 400
          if (slam .gt. svmin) overal = min(overal, slam - tol8)
          if (slam .le. svmin) undral = max(undral, slam + tol8)
          slam = half * (slam + svmin)
          go to 300
        endif
 
*---- Accept new point; replace previous worst point.
        sval(nvmax) = slam
        fval(nvmax) = f3
        if (f3 .lt. fvmin) then
          svmin = slam
          fvmin = f3
        else
          if (slam .gt. svmin) overal = min(overal, slam - tol8)
          if (slam .lt. svmin) undral = max(undral, slam + tol8)
        endif
      if (npts .lt. maxpt) go to 200
 
*---- Common exit point: Return best point and step used.
  400 continue
      fmin = fvmin
      do 410 i = 1, nx
        dx(i) = svmin * dx(i)
        x(i) = xsave(i) + dx(i)
  410 continue
      call mtputi(nx, x)
 
*---- Return Failure indication.
      iflag = 0
      if (svmin .eq. 0.0) iflag = 2
 
      end
