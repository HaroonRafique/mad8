      subroutine m66mpy(fact1, fact2, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Multiply two matrices.                                             *
*   TARGET may coincide with one of the factors.                       *
* Input:                                                               *
*   FACT1(6,6)  (real)  First factor.                                  *
*   FACT2(6,6)  (real)  Second factor.                                 *
* Output:                                                              *
*   TARGET(6,6) (real)  Product matrix: TARGET = FACT1 * FACT2.        *
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
      integer i,j,k
      double precision fact1,fact2,target,temp
      dimension         fact1(6,6), fact2(6,6), target(6,6)
 
      dimension         temp(6,6)
 
      do 30 k = 1, 6
        do 10 i = 1, 6
          temp(i,k) = 0.
   10   continue
        do 20 j = 1, 6
        do 20 i = 1, 6
          temp(i,k) = temp(i,k) + fact1(i,j) * fact2(j,k)
   20   continue
   30 continue
 
      do 40 k = 1, 6
      do 40 i = 1, 6
        target(i,k) = temp(i,k)
   40 continue
 
      end
