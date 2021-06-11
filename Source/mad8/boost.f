      subroutine boost(sphi,cphi,tphi,np,track)
      implicit none
* boost boost operation ********************************************
c    p,q,e are all normalized by p0
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
      double precision a,a1,cphi,h,h1x,h1y,h1z,hd1,sphi,sqr1a,tphi,
     +track,x1
      dimension track(6,*)
      do 1000 i=1,np
      a=(track(2,i)**2+track(4,i)**2)/(1+track(6,i))**2
      sqr1a=sqrt(1-a)
      h=(track(6,i)+1)*(1-sqr1a)
      track(2,i)=(track(2,i)-tphi*h)/cphi
      track(4,i)=track(4,i)/cphi
      track(6,i)=track(6,i)-sphi*track(2,i)
      a1=(track(2,i)**2+track(4,i)**2)/(1+track(6,i))**2
      sqr1a=sqrt(1-a1)
      hd1=(1+track(6,i))*sqr1a
      h1x=track(2,i)/hd1
      h1y=track(4,i)/hd1
      h1z=1-1/sqr1a
      x1=tphi*track(5,i)+(1+sphi*h1x)*track(1,i)
      track(3,i)=track(3,i)+sphi*h1y*track(1,i)
      track(5,i)=track(5,i)/cphi-sphi*h1z*track(1,i)
      track(1,i)=x1
*--- checked
 1000 continue
      return
      end
