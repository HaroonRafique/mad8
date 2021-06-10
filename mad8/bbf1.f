      subroutine bbf1(sepx,sepy,sigxx,sigyy,bbfx,bbfy,bbgx,bbgy)
      implicit none
***bbf1   bbf without using table *************************************
* gives transverse (f_x and f_y) and longitudinal(g_x and g_y)
* beam-beam kicks except for the kinematical term (nr_e/\gamma)
* sigxx is \sigma
***********************************************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      double precision arg1x,arg1y,arg2x,arg2y,bbfx,bbfy,bbgx,bbgy,
     +comfac,const,expfac,fac,fac2,sepx,sepy,sigxx,sigxy,
     +sigyy,sqrpi2,wx1,wx2,wy1,wy2,x
       complex*16 z_,w_1,cwerff
      data sqrpi2/3.54490 77018 11032d0/
      x=sepx**2/sigxx+sepy**2/sigyy
      fac2=2.d0*abs(sigxx-sigyy)
      fac=sqrt(fac2)
      const=sqrpi2/fac
      sigxy=sqrt(sigxx/sigyy)
      arg1x=abs(sepx/fac)
      arg1y=abs(sepy/fac)
      z_=cmplx(arg1x,arg1y)
      w_1=cwerff(z_)
      wy1=dble(w_1)
      wx1=imag(w_1)
      if(x.lt.100.d0) then
       expfac=exp(-x*0.5d0)
       arg2x=arg1x/sigxy
       arg2y=arg1y*sigxy
      z_=cmplx(arg2x,arg2y)
      w_1=cwerff(z_)
      wy2=dble(w_1)
      wx2=imag(w_1)
       bbfx=const*(wx1-expfac*wx2)
       bbfy=const*(wy1-expfac*wy2)
         if(sepx.lt.0) bbfx=-bbfx
         if(sepy.lt.0) bbfy=-bbfy
       comfac=sepx*bbfx+sepy*bbfy
       bbgx=-(comfac+2*(expfac/sigxy -1))/fac2
       bbgy= (comfac+2*(expfac*sigxy -1))/fac2
 
      else
       bbfx=const*wx1
       bbfy=const*wy1
        if(sepx.lt.0) bbfx=-bbfx
        if(sepy.lt.0) bbfy=-bbfy
       comfac=sepx*bbfx+sepy*bbfy
       bbgx=-(comfac-2)/fac2
       bbgy= -bbgx
      endif
c      write(6,*) comfac
      return
      end
