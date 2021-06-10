      subroutine sucopy(vs, ws, vd, wd)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Copy rotation matrix and position vector.                          *
* Input:                                                               *
*   VS(3)     (real)    Position vector.                               *
*   WS(3,3)   (real)    Rotation matrix.                               *
* Output:                                                              *
*   VD(3)     (real)    Position vector.                               *
*   WD(3,3)   (real)    Rotation matrix.                               *
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
      double precision vd,vs,wd,ws
      dimension         vs(3), ws(3,3), vd(3), wd(3,3)
 
      do 90 k = 1, 3
        vd(k) = vs(k)
        do 80 i = 1, 3
          wd(i,k) = ws(i,k)
   80   continue
   90 continue
 
      end
