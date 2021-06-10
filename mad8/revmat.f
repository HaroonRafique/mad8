      subroutine revmat(etax,etapx,etay,etapy,betax,betay,alphax,
     &     alphay, emitx,emity,sigz,sige,d,dinv)
      implicit none
* revmat ***********************************************
* d=b*h  dinv=hinv*binv
********************************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,j
      double precision alphax,alphay,b,betax,betay,betaz,binv,c,cinv,d,
     +dinv,dsqrt,emitx,emity,etapx,etapy,etax,etay,h,hinv,sige,sigz,
     +work
      dimension work(6,6)
      dimension b(6,6),binv(6,6),h(6,6),hinv(6,6),c(6,6),cinv(6,6)
      dimension d(6,6),dinv(6,6)
* revolution for normal modes
      do 10 i=1,6
      do 10 j=1,6
      d(i,j)=0
      dinv(i,j)=0
      b(i,j)=0
      binv(i,j)=0
      h(i,j)=0
      hinv(i,j)=0
      c(i,j)=0
      cinv(i,j)=0
  10  continue
      betaz=sigz/sige
* parameters for d
      b(1,1)=1/dsqrt(betax)
      b(2,1)=alphax/dsqrt(betax)
      b(2,2)=dsqrt(betax)
      b(3,3)=1/dsqrt(betay)
      b(4,3)=alphay/dsqrt(betay)
      b(4,4)=dsqrt(betay)
      b(5,5)=1/dsqrt(betaz)
      b(6,6)=dsqrt(betaz)
 
      c(1,1)=1d0
      c(2,2)=1d0
      c(3,3)=1d0
      c(4,4)=1d0
      c(5,5)=1d0
      c(6,6)=1d0
      cinv(1,1)=1d0
      cinv(2,2)=1d0
      cinv(3,3)=1d0
      cinv(4,4)=1d0
      cinv(5,5)=1d0
      cinv(6,6)=1d0
 
c      call promat(work,cinv,c)
c      call shomat(work)
 
      do 20 i=1,6
 20   h(i,i)=1d0
      h(1,6)=-etax
      h(2,6)=-etapx
      h(3,6)=-etay
      h(4,6)=-etapy
      h(5,1)=etapx
      h(5,2)=-etax
      h(5,3)=etapy
      h(5,4)=-etay
c         call shomat(h)
c      call promat(d,b,h)
 
      do 30 i=1,6
 30   hinv(i,i)=1d0
      hinv(1,6)=etax
      hinv(2,6)=etapx
      hinv(3,6)=etay
      hinv(4,6)=etapy
      hinv(5,1)=-etapx
      hinv(5,2)=etax
      hinv(5,3)=-etapy
      hinv(5,4)=etay
      binv(1,1)=dsqrt(betax)
      binv(2,1)=-alphax/dsqrt(betax)
      binv(2,2)=1/dsqrt(betax)
      binv(3,3)=dsqrt(betay)
      binv(4,3)=-alphay/dsqrt(betay)
      binv(4,4)=1/dsqrt(betay)
      binv(5,5)=dsqrt(betaz)
      binv(6,6)=1/dsqrt(betaz)
 
      call promat(work,c,h)
      call promat(d,b,work)
      call promat(work,hinv,cinv)
      call promat(dinv,work,binv)
 
      return
      end
