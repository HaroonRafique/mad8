      function vdot(n, a, b)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Dot product of two vectors.                                        *
* Input:                                                               *
*   A(N)      (real)    First input vector.                            *
*   B(N)      (real)    Second input vector.                           *
*   N         (integer) Length of A and B.                             *
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
      integer i,n
      double precision a,b,sum,vdot
      dimension         a(n), b(n)
 
      sum = 0.0
      do 10 i = 1, n
        sum = sum + a(i) * b(i)
   10 continue
      vdot = sum
 
      end
