      subroutine boosti(sphi,cphi,np,track)
      implicit none
* boosti **************inverse boost *****************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,np
      double precision a1,cphi,det,h1,h1d,h1x,h1y,h1z,sphi,sqr1a,track,
     +x1
      dimension track(6,np)
      do 1000 i=1,np
      a1=(track(2,i)**2+track(4,i)**2)/(1+track(6,i))**2
      sqr1a=sqrt(1-a1)
      h1d=(1+track(6,i))*sqr1a
      h1=(track(6,i)+1)*(1-sqr1a)
      h1x=track(2,i)/h1d
      h1y=track(4,i)/h1d
      h1z=1-1/sqr1a
      det=1+sphi*(h1x-sphi*h1z)
      x1=(track(1,i)-sphi*track(5,i))/det
      track(5,i)=cphi*((1+h1x*sphi)*track(5,i)-h1z*sphi*track(1,i))/det
      track(3,i)=track(3,i)-h1y*sphi*x1
      track(1,i) = x1
      track(6,i)=track(6,i)+sphi*track(2,i)
      track(2,i)=(track(2,i)+sphi*h1)*cphi
      track(4,i)=track(4,i)*cphi
*--- checked
 1000 continue
      return
      end
