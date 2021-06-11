      subroutine m66ref(source, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Reflect symplectic first order transform.                          *
* Input:                                                               *
*   SOURCE(6,6) (real)  Input matrix.                                  *
* Output:                                                              *
*   TARGET(6,6) (real)  Reflected matrix.                              *
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
      integer i
      double precision source,target,temp
      dimension         source(6,6), target(6,6), temp(6,6)
 
*---- TEMP = transpose(SOURCE) * J * signs.
      do 10 i = 1, 6
        temp(i,1) =   source(2,i)
        temp(i,2) =   source(1,i)
        temp(i,3) =   source(4,i)
        temp(i,4) =   source(3,i)
        temp(i,5) = - source(6,i)
        temp(i,6) = - source(5,i)
   10 continue
 
*---- TARGET = signs * transpose(J) * TEMP.
      do 20 i = 1, 6
        target(1,i) =   temp(2,i)
        target(2,i) =   temp(1,i)
        target(3,i) =   temp(4,i)
        target(4,i) =   temp(3,i)
        target(5,i) = - temp(6,i)
        target(6,i) = - temp(5,i)
   20 continue
 
      end
