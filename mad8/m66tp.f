      subroutine m66tp(source, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Transpose a matrix.
*   TARGET and SOURCE may overlap.                                     *
* Input:                                                               *
*   SOURCE(6,6) (real)  Input matrix.                                  *
* Output:                                                              *
*   TARGET(6,6) (real)  Transposed matrix: TARGET = tr(SOURCE).        *
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
      double precision source,target,temp
      dimension        source(6,6), target(6,6)
 
      dimension        temp(6,6)
 
      do 10 i = 1, 6
      do 10 j = 1, 6
        temp(j,i) = source(i,j)
   10 continue
      call m66cpy(temp, target)
 
      end
