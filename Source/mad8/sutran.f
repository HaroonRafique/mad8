      subroutine sutran(w, v, we)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Transform rotation W and displacement V from entrance to exit.     *
* Input:                                                               *
*   W(3,3)    (real)    Rotation matrix w.r.t. input system.           *
*   V(3)      (real)    Displacement w.r.t. input system.              *
*   WE(3,3)   (real)    Rotation matrix due to element.                *
* Output:                                                              *
*   W(3,3)    (real)    Rotation matrix w.r.t. output system.          *
*   V(3)      (real)    Displacement w.r.t. output system.             *
*----------------------------------------------------------------------*
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,k
      double precision v,vt,w,we,wt
      dimension         w(3,3), v(3), we(3,3)
 
      dimension         wt(3,3), vt(3)
 
*---- VT := transpose(WE) * V;
*     WT := transpose(WE) * W;
      do 20 i = 1, 3
        vt(i) = we(1,i)*v(1) + we(2,i)*v(2) + we(3,i)*v(3)
        do 10 k = 1, 3
          wt(i,k) = we(1,i)*w(1,k) + we(2,i)*w(2,k) + we(3,i)*w(3,k)
   10   continue
   20 continue
 
*---- V := VT       [= transpose(WE) * V];
*     W := WT * WE  [= transpose(WE) * W * WE];
      do 40 i = 1, 3
        v(i) = vt(i)
        do 30 k = 1, 3
          w(i,k) = wt(i,1)*we(1,k) + wt(i,2)*we(2,k) + wt(i,3)*we(3,k)
   30   continue
   40 continue
 
      end
