      subroutine sutrak(v, w, ve, we)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Update global position.                                            *
* Input:                                                               *
*   V(3)      (real)    Global displacement before element.            *
*   W(3,3)    (real)    Global rotation matrix before element.         *
*   VE(3)     (real)    Displacement due to element.                   *
*   WE(3,3)   (real)    Rotation due to element.                       *
* Output:                                                              *
*   V(3)      (real)    Global displacement after element.             *
*   W(3,3)    (real)    Global rotation matrix after element.          *
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
      integer i
      double precision v,ve,w,we,wt1,wt2,wt3
      dimension         v(3), w(3,3), ve(3), we(3,3)
 
      do 10 i = 1, 3
        v(i) = v(i) + w(i,1)*ve(1) + w(i,2)*ve(2) + w(i,3)*ve(3)
        wt1 = w(i,1)*we(1,1) + w(i,2)*we(2,1) + w(i,3)*we(3,1)
        wt2 = w(i,1)*we(1,2) + w(i,2)*we(2,2) + w(i,3)*we(3,2)
        wt3 = w(i,1)*we(1,3) + w(i,2)*we(2,3) + w(i,3)*we(3,3)
        w(i,1) = wt1
        w(i,2) = wt2
        w(i,3) = wt3
   10 continue
 
      end
