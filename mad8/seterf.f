      subroutine seterf
      implicit none
c******************** seterf **********************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,j,mx,my,nx,ny
      double precision dx,dy,x,y
      complex*16 z_,cwerff
      complex*8 w_
      parameter (mx=39,my=30)
      common/erfcmm/ w_(0:3,0:mx,0:my)
 
      complex*16 i_,c_1,c_2, c_3, c_4, c_5
      data i_/(0d0,1d0)/,c_1/(-1.5d0,1.5d0)/,c_2/(0.5d0,-0.5d0)/,
     %      c_3/(0d0,-2d0)/,c_4/(1.5d0,0.5d0)/,c_5/(-1.5d0,0.5d0)/
      dx=0.1d0
      dy=0.1d0
      nx=39
      ny=30
      do 100 i=0,nx
      x=dx*i
      do 100 j=0,ny
      y=dy*j
      z_=cmplx(x,y)
      w_(0,i,j)=cwerff(z_)
  100 continue
      do 200 i=0,nx-1
      do 200 j=0,ny-1
      w_(1,i,j)=c_1*w_(0,i,j)+c_2*w_(0,i+1,j+1)+w_(0,i,j+1)
     %          -i_*w_(0,i+1,j)
      w_(2,i,j)=c_3*w_(0,i,j)+c_4*w_(0,i+1,j)+i_*w_(0,i+1,j+1)
     %          +c_5*w_(0,i,j+1)
      w_(3,i,j)=c_2*(w_(0,i,j+1)-w_(0,i+1,j)
     %                           +i_*(w_(0,i,j)-w_(0,i+1,j+1)))
  200 continue
      return
      end
