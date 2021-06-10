      function cwerff(z_)
      implicit none
c******************** cwerff ****************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer n,nc,nu
      double precision alamda,c,const,fn,h,h2,r1,r2,rs1,rs2,
     +s,s1,s2,t1,t2,x,xx,y,yy
      complex*16 z_,cwerff
      logical b
      data const/1.12837 91670 9551d0/
      xx=dble(z_)
      yy=imag(z_)
      x=abs(xx)
      y=abs(yy)
      if(y .lt. 4.29d0 .and. x .lt. 5.33d0) go to 1
      h=0.
      nc=0
      nu=8
      alamda=0.
      b=.true.
      go to 2
    1 s=(1d0-y/4.29d0)*sqrt(1d0-x**2/28.41d0)
      h=1.6d0*s
      h2=2d0*h
      nc=6+int(23d0*s)
      nu=9+int(21d0*s)
      alamda=h2**nc
      b= alamda .eq. 0d0
    2 r1=0d0
      r2=0d0
      s1=0d0
      s2=0d0
      n=nu+1
    3 n=n-1
      fn=n+1
      t1=y+h+fn*r1
      t2=x-fn*r2
      c=0.5d0/(t1**2+t2**2)
      r1=c*t1
      r2=c*t2
      if(h .le. 0.0 .or. n .gt. nc) go to 4
      t1=alamda+s1
      s1=r1*t1-r2*s2
      s2=r2*t1+r1*s2
      alamda=alamda/h2
    4 if(n .gt. 0) go to 3
      if(b) go to 6
      rs1=s1
      rs2=s2
      go to 7
    6 rs1=r1
      rs2=r2
    7 rs1=const*rs1
      if(y .eq. 0d0) rs1=exp(-x**2)
      cwerff=cmplx(rs1,const*rs2)
      if(yy .lt. 0d0) go to 8
      if(xx .lt. 0d0) cwerff=conjg(cwerff)
      return
    8 cwerff=2d0*exp(-cmplx(x,y)**2)-cwerff
      if(xx .gt. 0d0) cwerff=conjg(cwerff)
      return
      end
