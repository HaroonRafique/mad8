      subroutine mtfcn(nf, nx, x, fval, iflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute matching functions.                                        *
* Input:                                                               *
*   NF        (integer) Number of functions to be computed.            *
*   NX        (integer) Number of input parameters.                    *
*   X(NX)     (real)    Input parameters.                              *
* Output:                                                              *
*   FVAL(NF)  (real)    Matching functions computed.                   *
*   IFLAG     (integer) Stability flag.                                *
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
      integer iflag,nf,nx
      double precision fval,x
      dimension         x(nx), fval(nf)
 
*---- Store parameter values in data structure.
      call mtputi(nx, x)
 
*---- Compute matching functions.
      call mtcond(.false., nf, fval, iflag)
 
 9999 end
