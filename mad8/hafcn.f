      subroutine hafcn(nf, nx, x, fvec, iflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Calculate the penalty functions to be minimized.                   *
* Input:                                                               *
*   NF        (integer) Number of penalty functions.                   *
*   NX        (integer) Number of parameters.                          *
*   X(NX)     (real)    Parameter values.                              *
* Output:                                                              *
*   FVEC(NF)  (real)    The penalty functions.                         *
*   IFLAG     (integer) Error flag (always set to 0, i.e. OK).         *
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
      integer i,iflag,iprint,nf,nx
      double precision fvec,x,xix2,xix3,xiy2,xiy3
      dimension         x(nx), fvec(nf)
      integer mhfun
      double precision hdes,hfac,hfun,hwei
 
*---- Data for minimization in HARMON.
      parameter         (mhfun = 21)
      common /hafbad/   hdes(mhfun), hfun(mhfun), hwei(mhfun),
     +                  hfac(mhfun)
      save              /hafbad/
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
 
      dimension         xix2(2), xiy2(2), xix3(3), xiy3(3)
 
*---- Set new parameter values.
      call mtputi(nx, x)
 
*---- Clear functions of penalty.
      call uzero(fvec, 1, mwflt * nf)
      iprint = 0
 
*---- First order chromaticity.
      call hachcl(hfun(1), hfun(2))
 
*---- HADDSP must always be run first; results are used by others.
      call haddsp(iprint, hfun(10), hfun(11), hfun(16), hfun(17))
 
*---- Derivatives of beta w.r.t. delta.
      if (hfac(12) .ne. 0.0  .or.  hfac(13) .ne. 0.0  .or.
     +    hfac(18) .ne. 0.0  .or.  hfac(19) .ne. 0.0) then
        call hadbet(iprint, hfun(12), hfun(13), hfun(18), hfun(19))
      endif
 
*---- Resonances.
      if (hfac(14) .ne. 0.0  .or.  hfac(15) .ne. 0.0  .or.
     +    hfac(20) .ne. 0.0  .or.  hfac(21) .ne. 0.0) then
        call hareso(iprint, hfun(14), hfun(15), hfun(20), hfun(21))
      endif
 
*---- Derivatives of tunes w.r.t. delta.
      if (hfac(3) .ne. 0.0  .or.  hfac(4) .ne. 0.0  .or.
     +    hfac(5) .ne. 0.0  .or.  hfac(6) .ne. 0.0) then
        call hadtun(xix2, xiy2, xix3, xiy3)
        hfun(3) = xix2(1) + xix2(2)
        hfun(4) = xiy2(1) + xiy2(2)
        hfun(5) = xix3(1) + xix3(2) + xix3(3)
        hfun(6) = xiy3(1) + xiy3(2) + xiy3(3)
      endif
 
*---- Amplitude-dependence of tunes.
      if (hfac(7) .ne. 0.0  .or.  hfac(8) .ne. 0.0  .or.
     +    hfac(9) .ne. 0.0) then
        call haatun(iprint, hfun(7), hfun(8), hfun(9))
      endif
 
*---- How bad are things?
      do 10 i = 1, mhfun
        fvec(i) = (hfun(i) - hdes(i)) * hfac(i)
   10 continue
      iflag = 0
 
      end
