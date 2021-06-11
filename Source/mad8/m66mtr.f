      subroutine m66mtr(fact1, fact2, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Multiply a matrix with the transpose of another matrix.            *
*   TARGET must not coincide with either factor.                       *
* Input:                                                               *
*   FACT1(6,6)  (real)  First factor.                                  *
*   FACT2(6,6)  (real)  Second factor (will be transposed).            *
* Output:                                                              *
*   TARGET(6,6) (real)  Product: TARGET = FACT1 * tr(FACT2).           *
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
 
      do 10 j = 1, 6
      do 10 i = 1, 6
        target(i,j) = 0.
   10 continue
      do 30 j = 1, 6
        do 20 k = 1, 6
        do 20 i = 1, 6
          target(i,j) = target(i,j) + fact1(i,k) * fact2(j,k)
   20   continue
   30 continue
 
      end
