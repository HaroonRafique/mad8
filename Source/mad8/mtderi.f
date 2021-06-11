      subroutine mtderi(fcn, nf, nx, x, grd, g2, fvec)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find first derivatives of penalty function.                        *
* Input:                                                               *
*   FCN       (subr)    Returns value of penalty function.             *
*   NF        (integer) Number of functions.                           *
*   NX        (integer) Number of parameters.                          *
*   X(NX)     (real)    Parameter values. On output, best estimate.    *
* Output:                                                              *
*   GRD(*)    (real)    Gradient of penalty function                   *
*                       w.r.t. internal parameter values.              *
*   G2(*)     (real)    Second derivatives of penalty function         *
*                       w.r.t. internal parameter values.              *
* Working array:                                                       *
*   FVEC(NF)  (real)    Function values.                               *
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
      integer i,icycle,iflag,nf,nx
      double precision eps,f1,f2,fvec,g2,grd,half,one,two,vdot,x,xsave,
     +xstep
      external          fcn
      dimension         x(nx), grd(nx), g2(nx), fvec(nf)
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
 
      do 90 i = 1, nx
        xsave = x(i)
        xstep = eps * abs(xsave)
        if (xstep .eq. 0.0) xstep = eps
        do 50 icycle = 1, 10
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
              go to 60
            endif
          endif
          xstep = half * xstep
   50   continue
        f2 = fmin
        f1 = fmin
   60   continue
        grd(i) = (f2 - f1) / (two * xstep)
        g2(i) = (f2 - two * fmin + f1) / xstep**2
        if (g2(i) .eq. 0.0) g2(i) = one
        x(i) = xsave
   90 continue
 
      call mtputi(nx, x)
 
      end
