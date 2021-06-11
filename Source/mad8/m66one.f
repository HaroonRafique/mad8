      subroutine m66one(target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set matrix to unity.                                               *
* Output:                                                              *
*   TARGET(6,6) (real)  Unit matrix: TARGET = I.                       *
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
      double precision target
      dimension         target(6,6)
 
      do 20 j = 1, 6
        do 10 i = 1, 6
          target(i,j) = 0.0
   10   continue
        target(j,j) = 1.0
   20 continue
 
      end
