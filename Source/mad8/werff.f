      function werff(z_)
      implicit none
c******************** werff ****************************************
      integer ix,iy,mx,my
      real x,y,z2i,z2r
      parameter (mx=39,my=30)
      complex*8 w_,dz_,z_1
      common/erfcmm/w_(0:3,0:mx,0:my)
      complex*16 werff,z_, zloc
      double precision zequ(2)
      equivalence (zloc, zequ)
      real*4 c1, c2, c3, c4, c5, c6, c7, c8, c9, c10
      data
     a c1/0.4613135e0/,c2/0.1901635e0/,c3/0.09999216e0/,c4/1.7844927e0/,
     b c5/2.883894e-3/,c6/5.5253437e0/,c7/0.5124242e0/, c8/0.2752551e0/,
     c c9/0.05176536e0/,c10/2.724745e0/
*      x=real(z_)
*      y=imag(z_)
      zloc = z_
      x = zequ(1)
      y = zequ(2)
      if(x.ge.3.8999.or.y.ge.2.9999) goto 20
      x=10.0*x
      y=10.0*y
      ix=int(x)
      iy=int(y)
      dz_=cmplx(x-float(ix),y-float(iy))
      werff=((dz_*w_(3,ix,iy)+w_(2,ix,iy))*dz_+w_(1,ix,iy))*dz_
     %     +w_(0,ix,iy)
      return
   20 z2r=x**2-y**2
      z2i=2.0*x*y
      z_1=cmplx(-y,x)
      if(x.ge.6.0.or.y.ge.6.0) goto 40
      werff=z_1*(c1/cmplx(z2r-c2,z2i)+c3/cmplx(z2r-c4,z2i)
     %          +c5/cmplx(z2r-c6,z2i))
      return
   40 werff=z_1*(c7/cmplx(z2r-c8,z2i)+c9/cmplx(z2r-c10,z2i))
      return
      end
