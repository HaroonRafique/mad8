      subroutine cperrf(xx, yy, wx, wy)
      implicit none
*----------------------------------------------------------------------*
* purpose:
*   uses wwerf, double precision complex error function,
*   written at cern by K. Koelbig.
* input:
*   xx, yy    (double)    real + imag argument
* output:
*   wx, wy    (double)    real + imag function result
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
      complex cmplx
      double precision xx, yy, wx, wy
      complex*16 z1, z2, wwerf
      real*8 d(2)
      equivalence (d, z2)
 
      z1 = cmplx(xx, yy)
      z2 = wwerf(z1)
      wx = d(1)
      wy = d(2)
      end
