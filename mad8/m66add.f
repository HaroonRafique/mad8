      subroutine m66add(term1, term2, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Add two matrices.                                                  *
* Input:                                                               *
*   TERM1(6,6)  (real)  First term.                                    *
*   TERM2(6,6)  (real)  Second term.                                   *
* Output:                                                              *
*   TARGET(6,6) (real)  Sum: TARGET = TERM1 + TERM2.                   *
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
      double precision target,term1,term2
      dimension         term1(6,6), term2(6,6), target(6,6)
 
      do 10 i = 1, 6
      do 10 j = 1, 6
        target(i,j) = term1(i,j) + term2(i,j)
   10 continue
 
      end
