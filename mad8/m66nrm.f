      subroutine m66nrm(fm, res)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Computes the norm of a matrix.                                     *
*   Reference:          L. Collatz,                                    *
*                       Functional Analysis & Numerical Mathematics.   *
*   Source:             MARYLIE, Version 3.0.                          *
* Input:                                                               *
*   FM(6,6)     (real)  Input matrix.                                  *
* Output:                                                              *
*   RES         (real)  Norm of FM: RES = max abs column sum.          *
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
      double precision fm,res,sum
      dimension         fm(6,6)
 
      res = 0.
      do 20 j = 1, 6
        sum = 0.
        do 10 i = 1, 6
          sum = sum + abs(fm(i,j))
   10   continue
        res = max(res,sum)
   20 continue
 
      end
