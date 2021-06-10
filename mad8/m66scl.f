      subroutine m66scl(scalar, source, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Multiply matrix by scalar.                                         *
* Input:                                                               *
*   SCALAR      (real)  Scale factor.                                  *
*   SOURCE(6,6) (real)  Input matrix.                                  *
* Output:                                                              *
*   TARGET(6,6) (real)  Scaled matrix: TARGET = SCALAR * SOURCE.       *
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
      integer i,j
      double precision scalar,source,target
      dimension         source(6,6), target(6,6)
 
      do 10 i = 1, 6
      do 10 j = 1, 6
        target(i,j) = scalar * source(i,j)
   10 continue
 
      end
