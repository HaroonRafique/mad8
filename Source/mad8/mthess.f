      subroutine mthess(fcn, nf, nx, covar, x, grd, g2, fvec, wa)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build covariance matrix.                                           *
* Input:                                                               *
*   FCN       (subr)    Returns value of penalty function.             *
*   NF        (integer) Number of functions.                           *
*   NX        (integer) Number of parameters.                          *
*   X(NX)     (real)    Parameter values. On output, best estimate.    *
* Output:                                                              *
*   COVAR(NX,NX)        Covariance matrix.
*   GRD(NX)   (real)    Gradient of penalty function                   *
*                       w.r.t. internal parameter values.              *
*   G2(NX)    (real)    Second derivatives of penalty function         *
*                       w.r.t. internal parameter values.              *
* Working array:                                                       *
*   FVEC(NF)  (real)    Function values.                               *
*   WA(NX,2)  (real)    Working vectors.                               *
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
      integer i,icycle,iflag,j,nf,nx
      double precision covar,eps,f1,f2,fij,fvec,g2,grd,half,one,two,
     +vdot,wa,x,xs1,xs2,xsave,xstep
      external          fcn
      dimension         covar(nx,nx), x(nx), grd(nx), g2(nx), wa(nx,2)
      dimension         fvec(nf)
      logical           eflag
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
 
      parameter         (one = 1.0d0, half = 0.5d0, two = 2.0d0)
 
      eps = sqrt(epsmch)
 
      do 60 i = 1, nx
        xsave = x(i)
        xstep = eps * max(abs(xsave), one)
        do 30 icycle = 1, 10
          x(i) = xsave + xstep
          call fcn(nf, nx, x, fvec, iflag)
          nfcn = nfcn + 1
          if (iflag .eq. 0) then
            f2 = vdot(nf, fvec, fvec)
            x(i) = xsave - xstep
            call fcn(nf, nx, x, fvec, iflag)
            nfcn = nfcn + 1
            if (iflag .eq. 0) then
              f1 = vdot(nf, fvec, fvec)
              go to 40
            endif
          endif
          xstep = half * xstep
   30   continue
        f1 = fmin
        f2 = fmin
   40   continue
        grd(i) = (f2 - f1) / (two * xstep)
        g2(i) = (f2 - two * fmin + f1) / xstep**2
        if (g2(i) .eq. 0.0) g2(i) = one
        x(i) = xsave
        covar(i,i) = g2(i)
        wa(i,1) = f2
        wa(i,2) = xstep
   60 continue
 
*---- Off-diagonal elements.
      do 90 i = 1, nx - 1
        xs1 = x(i)
        x(i) = xs1 + wa(i,2)
        do 80 j = i + 1, nx
          xs2 = x(j)
          x(j) = xs2 + wa(j,2)
          call fcn(nf, nx, x, fvec, iflag)
          nfcn = nfcn + 1
          if (iflag .eq. 0) then
            fij = vdot(nf, fvec, fvec)
            covar(i,j) = (fij+fmin-wa(i,1)-wa(j,1)) / (wa(i,2)*wa(j,2))
            covar(j,i) = covar(i,j)
          else
            covar(i,j) = 0.0
            covar(j,i) = 0.0
          endif
          x(j) = xs2
   80   continue
        x(i) = xs1
   90 continue
 
*---- Restore original point.
      call mtputi(nx, x)
 
*---- Ensure positive definiteness and invert.
      call mtpsdf(covar, nx)
      call symsol(covar, nx, eflag)
 
      end
