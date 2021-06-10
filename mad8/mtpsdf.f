      subroutine mtpsdf(covar, nx)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Force covariance matrix to be positive definite.                   *
* Updated:                                                             *
*   COVAR(*,*)        Covariance matrix.                               *
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
      integer i,ieigen,ip,nval,nx
      double precision add,covar,eps,one,pmax,pmin
      dimension  covar(nx,nx)
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      parameter         (one = 1.0d0, eps = 1.0d-3)
 
*---- Allocate working space.
      ip     = iwork
      ieigen = ip + nx * nx
      iwork  = ieigen + nx
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Copy matrix and find eigenvalues.
      call ucopy(covar, dq(ip+1), mwflt * nx * nx)
      call symeig(dq(ip+1), nx, nx, dq(ieigen+1), nval)
 
*---- Enforce positive definiteness.
      pmin = dq(ieigen+1)
      pmax = dq(ieigen+1)
      do 10 i = 1, nx
        if (dq(ieigen+i) .lt. pmin) pmin = dq(ieigen+i)
        if (dq(ieigen+i) .gt. pmax) pmax = dq(ieigen+i)
   10 continue
      pmax = max(abs(pmax), one)
      if (pmin .le. epsmch * pmax) then
        add = eps * pmax - pmin
        do 30 i = 1, nx
          covar(i,i) = covar(i,i) + add
   30   continue
        cstat = 'not posdef'
      endif
 
*---- Release working space.
      iwork = ip
 
      end
