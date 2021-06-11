      subroutine mtrazz(nvrr, fnew, pnew, fsim, psim, jh, jl)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Replace vertex in simplex whose function is highest.               *
*   Return indices of new best and worst points.                       *
* Input:                                                               *
*   NVAR      (integer) Number of parameters.                          *
*   FNEW      (real)    Function value for new vertex.                 *
*   PNEW(*)   (real)    Coordinates of new vertex.                     *
* Updated:                                                             *
*   FSIM(*)   (real)    Function values in (NVAR + 1) vertices.        *
*   PSIM(*,*) (real)    Coordinates of (NVAR + 1) vertices.            *
*   JH        (integer) Index of highest function value.               *
*   JL        (integer) Index of lowest function value.                *
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
      integer i,jh,jl,nvrr
      double precision fnew,fsim,pnew,psim
      dimension         pnew(nvrr), fsim(0:nvrr), psim(nvrr,0:nvrr)
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
 
*---- Replace vertex with highest function value.
      do 10 i = 1, nvrr
        psim(i,jh) = pnew(i)
   10 continue
      fsim(jh) = fnew
 
*---- Find indices of lowest and highest function value.
      jl = 0
      jh = 0
      do 20 i = 1, nvrr
        if (fsim(i) .lt. fsim(jl)) jl = i
        if (fsim(i) .gt. fsim(jh)) jh = i
   20 continue
 
*---- Get best value and estimated distance to minimum.
      fmin = fsim(jl)
      edm = min(10.0 * (fsim(jh) - fmin), fmin)
 
      end
