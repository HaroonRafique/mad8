      function gfunc(sigx,sigy,bx,by)
      implicit none
* gfunc **************************************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      double precision bx,by,func,gfunc,pi2,q,sigx,sigy,xx,yy
      data pi2 / 6.283185307179587d0/
      func(q)=exp(-q**2/2)
c         write(6,*) sigx,sigy,bx,by
      xx=bx/sigx
      yy=by/sigy
      if(xx.lt.6d0.and.yy.lt.6d0) then
          gfunc=func(xx)*func(yy)/(pi2*sigx*sigy)
      else
          gfunc=0
      endif
      return
      end
