      subroutine m66trm(fact1, fact2, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Multiply the transpose of a matrix with another matrix.            *
*   TARGET must not coincide with either factor.                       *
* Input:                                                               *
*   FACT1(6,6)  (real)  First factor (will be transposed).             *
*   FACT2(6,6)  (real)  Second factor.                                 *
* Output:                                                              *
*   TARGET(6,6) (real)  Product: TARGET = tr(FACT1) * FACT2.           *
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
      double precision fact1,fact2,target
      dimension         fact1(6,6), fact2(6,6), target(6,6)
 
      do 30 j = 1, 6
        do 10 i = 1, 6
          target(i,j) = 0.
   10   continue
        do 20 k = 1, 6
        do 20 i = 1, 6
          target(i,j) = target(i,j) + fact1(k,i) * fact2(k,j)
   20   continue
   30 continue
      end
