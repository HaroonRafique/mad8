      subroutine sbc(xstar,ystar,zstar,bbf,olum,
     &  sigxxs,sigpps,sigyys,sigqqs,f,np,nsli,track)
      implicit none
***sbc ***synchro-beam for headon collision**********************
*  call bbf  (table)
* olum is a luminosity
*****************************************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,jsli,np,nsli
      double precision bbfx,bbfy,bbgx,bbgy,f,g,gfunc,olum,s,sepx,sepy,
     +sigpps,sigqqs,sigxxs,sigyys,sumsxsy,sx,sy,track,xstar,ystar,zstar
      external bbf
      dimension track(6,*)
      dimension sigxxs(nsli),sigpps(nsli),sigyys(nsli),sigqqs(nsli)
      dimension xstar(nsli),ystar(nsli),zstar(nsli)
c          write(6,*) 'welcome to sbc'
      olum=0.0
      do 2000 jsli=1,nsli
 
      do 1000 i=1,np
      s=(track(5,i)-zstar(jsli))/2
      sx=sigxxs(jsli)+sigpps(jsli)*s*s
      sy=sigyys(jsli)+sigqqs(jsli)*s*s
      sumsxsy = sx + sy
      if (abs((sx - sy) / sumsxsy) .lt. 1.d-3)  then
        sx = 0.5005 * sumsxsy
        sy = 0.4995 * sumsxsy
      endif
      sepx=track(1,i)+track(2,i)*s-xstar(jsli)
      sepy=track(3,i)+track(4,i)*s-ystar(jsli)
c           write(6,*) 'before gfunc'
c           write(6,*) i,sx,sy,sepx,sepy
 
      olum=olum+
     $  gfunc( sqrt(sx),sqrt(sy),sepx,sepy)
*--------1---------2--------3--------4---------5---------6---------7--
c           write(6,*) 'before bbf'
       if(sx.gt.sy) then
      call bbf(sepx,sepy,sx,sy,bbfx,bbfy,bbgx,bbgy)
       else
      call bbf(sepy,sepx,sy,sx,bbfy,bbfx,bbgy,bbgx)
       endif
      bbfx=f*bbfx
      bbfy=f*bbfy
      bbgx=f*bbgx
      bbgy=f*bbgy
      g=s*(sigpps(jsli)*bbgx+sigqqs(jsli)*bbgy)
      track(6,i)=track(6,i)-bbfx*(track(2,i)-bbfx/2)/2-
     &           bbfy*(track(4,i)-bbfy/2)/2-g
      track(1,i)=track(1,i)+s*bbfx
      track(2,i)=track(2,i)-bbfx
      track(3,i)=track(3,i)+s*bbfy
      track(4,i)=track(4,i)-bbfy
 
 1000 continue
 2000 continue
c              write(6,*) 'bye bye from sbc'
      olum=olum/(nsli*np)
      return
      end
