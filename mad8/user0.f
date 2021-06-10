      function user0()
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate random number from user-defined distribution.             *
* Input:                                                               *
*   No arguments.                                                      *
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
      double precision user0
 
*---- Replace next line by code to generate the distribution.
      user0 = 0.
 
      end
