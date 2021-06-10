      subroutine stsld(xstar,ystar,zstar,phi,
     &         sigzs,siges,emitxs,betaxs,alphxs,
     &         emitys,betays,alphys,
     &         etaxs,etapxs,etays,etapys,
     &         sigzzs,sigxxs,sigpps,sigyys,sigqqs,nsli)
      implicit none
********stsld********************************************
*   makes a data of the strong slice
*   sigxx  is the <x**2> at headon frame.
*   sigzs is the sqrt<z**2> at lab. frame. (input)
*   at present, sigxxl etc do not depend on zstar
*   alphxs,alphys are not used for the moment
*********************************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,nsli
      double precision alphxs,alphys,betaxs,betays,bord,bord1,border,
     +emitxs,emitys,etapxs,etapys,etaxs,etays,gauinv,phi,pi,siges,
     +sigpps,sigqqs,sigxxs,sigyys,sigz,sigzs,sigzz,sigzzs,xstar,ystar,
     +yy,zstar
      dimension sigxxs(nsli),sigpps(nsli),sigyys(nsli),sigqqs(nsli)
      dimension xstar(nsli),ystar(nsli),zstar(nsli)
      data border /8d0/
      data   pi / 3.141592653589793d0/
        sigz=sigzs/cos(phi)
        sigzz=sigz**2
* define zstar,xstar,ystar
*  bord is border zstar is the barycenter of region divided two borders.
         bord=+border
         do 101 i=nsli,1,-1
         yy=1d0/nsli*(i-1)
c            write(6,*) bord
         if(i.ne.1) bord1=dble(gauinv(yy))
         if(i.eq.1) bord1=-border
         zstar(i)=(exp(-bord**2/2)-exp(-bord1**2/2))
     &         /sqrt(2d0*pi)*nsli
         bord=bord1
 
         zstar(i)=zstar(i)*sigz
         xstar(i)=zstar(i)*sin(phi)
         ystar(i)=0
         sigxxs(i)=emitxs*betaxs+(etaxs*siges)**2
         sigpps(i)=(emitxs/betaxs+(etapxs*siges)**2)/cos(phi)**2
         sigyys(i)=emitys*betays+(etays*siges)**2
         sigqqs(i)=(emitys/betays+(etapys*siges)**2)/cos(phi)**2
 
c            write(6,*) i,zstar(i)
  101    continue
      return
      end
