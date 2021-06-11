      function user2(a, b)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate random number from user-defined distribution.             *
* Input:                                                               *
*   A, B      (real)    Two arguments for the distribution.            *
* Result:                                                              *
*   The user-generated random number.                                  *
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
      double precision a,b,user2
 
*---- Replace next line by code to generate the distribution.
      user2 = 0.
 
      end
