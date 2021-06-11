      subroutine m66mak(f2, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute matrix TARGET corresponding to Lie polynomial F2.          *
*   Original author:    Liam Healy.                                    *
* Input:                                                               *
*   F2          (poly)  Polynomial of order 2.                         *
* Output:                                                              *
*   TARGET(6,6) (real)  Output matrix: TARGET * v = - [J,v].           *
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
      double precision f2,target
      dimension         f2(*), target(6,6)
 
      target(1,1) = - f2(8)
      target(1,2) = - 2.0 * f2(13)
      target(1,3) = - f2(14)
      target(1,4) = - f2(15)
      target(1,5) = - f2(16)
      target(1,6) = - f2(17)
      target(2,1) = 2.0 * f2(7)
      target(2,2) = f2(8)
      target(2,3) = f2(9)
      target(2,4) = f2(10)
      target(2,5) = f2(11)
      target(2,6) = f2(12)
      target(3,1) = - f2(10)
      target(3,2) = - f2(15)
      target(3,3) = - f2(19)
      target(3,4) = - 2.0 * f2(22)
      target(3,5) = - f2(23)
      target(3,6) = - f2(24)
      target(4,1) = f2(9)
      target(4,2) = f2(14)
      target(4,3) = 2.0 * f2(18)
      target(4,4) = f2(19)
      target(4,5) = f2(20)
      target(4,6) = f2(21)
      target(5,1) = - f2(12)
      target(5,2) = - f2(17)
      target(5,3) = - f2(21)
      target(5,4) = - f2(24)
      target(5,5) = - f2(26)
      target(5,6) = - 2.0 * f2(27)
      target(6,1) = f2(11)
      target(6,2) = f2(16)
      target(6,3) = f2(20)
      target(6,4) = f2(23)
      target(6,5) = 2.0 * f2(25)
      target(6,6) = f2(26)
 
      end
