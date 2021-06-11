      function vmod(n, a)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Norm of a vector.                                                  *
* Input:                                                               *
*   A(N)      (real)    Input vector.                                  *
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
      double precision a,sum,vmod
      dimension         a(n)
 
      sum = 0.0
      do 10 i = 1, n
        sum = sum + a(i) * a(i)
   10 continue
      vmod = sqrt(sum)
 
      end
