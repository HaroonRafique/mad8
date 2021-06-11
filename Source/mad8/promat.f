      subroutine promat(a,b,c)
      implicit none
* promat **  a=b*c  ************************************
*  matric a=b*c (6,6) matrix
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
      double precision a,b,c
      dimension a(6,6),b(6,6),c(6,6)
      do 200 i=1,6
      do 200 j=1,6
      a(i,j)=0
      do 100 k=1,6
 100  a(i,j)=a(i,j)+b(i,k)*c(k,j)
 200  continue
      return
      end
