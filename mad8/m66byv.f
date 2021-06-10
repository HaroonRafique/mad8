      subroutine m66byv(amat, avec, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Multiply matrix times vector.                                      *
* Input:                                                               *
*   AMAT(6,6)   (real)  Input matrix.                                  *
*   AVEC(6)     (real)  Input vector.                                  *
* Output:                                                              *
*   TARGET(6)   (real)  Output vector: TARGET = AMAT * AVEC.           *
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
      double precision amat,avec,target,temp
      dimension         amat(6,6), avec(6), target(6)
 
      dimension         temp(6)
 
      do 20 i = 1, 6
        temp(i) = 0.0
        do 10 j = 1, 6
          temp(i) = temp(i) + amat(i,j) * avec(j)
   10   continue
   20 continue
 
      call ucopy(temp, target, 6*mwflt)
 
      end
