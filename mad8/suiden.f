      subroutine suiden(v, w)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set up identity transform for survey.                              *
* Output:                                                              *
*   V(3)      (real)    Displacement = 0.                              *
*   W(3,3)    (real)    Rotation = identity.                           *
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
      double precision v,w
      dimension         v(3), w(3,3)
 
      do 10 i = 1, 3
        v(i) = 0.
        w(i,1) = 0.
        w(i,2) = 0.
        w(i,3) = 0.
        w(i,i) = 1.
   10 continue
 
      end
