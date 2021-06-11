      function factor(n)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute factorial function of N.                                   *
* Input:                                                               *
*   N         (integer) Number for which N! is to be found.            *
* Result:     (real)    N!                                             *
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
      integer i,ifact,n
      double precision factor
 
      ifact = 1
      do 10 i = 1, n
        ifact = ifact * i
   10 continue
      factor = ifact
 
      end
