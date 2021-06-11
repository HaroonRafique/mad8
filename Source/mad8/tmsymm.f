      subroutine tmsymm(t)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Symmetrize second-order array T.                                   *
* Input:                                                               *
*   T(6,6,6)  (real)    Array to be symmetrized.                       *
* Output:                                                              *
*   T(6,6,6)  (real)    Symmetrized array.                             *
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
      integer i,k,l
      double precision t
      dimension         t(6,6,6)
 
      do 20 k = 1, 5
      do 20 l = k+1, 6
        do 10 i = 1, 6
          t(i,l,k) = t(i,k,l)
   10   continue
   20 continue
 
      end
