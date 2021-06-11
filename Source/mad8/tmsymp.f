      subroutine tmsymp(r)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Symplectify a 6 by 6 matrix R.                                     *
*   Algorithm described in the doctoral thesis by Liam Healey.         *
* Input:                                                               *
*   R(6,6)    (real)    Matrix to be symplectified.                    *
* Output:                                                              *
*   R(6,6)    (real)    The symplectified matrix.                      *
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
      integer i,j
      double precision a,b,r,v
      dimension         r(6,6)
 
      dimension         a(6,6), b(6,6), v(6,6)
      logical           eflag
 
      do 20 i = 1, 6
        do 10 j = 1, 6
          a(i,j) = - r(i,j)
          b(i,j) = + r(i,j)
   10   continue
        a(i,i) = a(i,i) + 1.0
        b(i,i) = b(i,i) + 1.0
   20 continue
 
      call m66div(a, b, v, eflag)
      call m66inv(v, a)
 
      do 40 i = 1, 6
        do 30 j = 1, 6
          a(i,j) = (a(i,j) - v(i,j)) / 2.0
          b(i,j) = - a(i,j)
   30   continue
        b(i,i) = b(i,i) + 1.0
        a(i,i) = a(i,i) + 1.0
   40 continue
 
      call m66div(a, b, r, eflag)
 
      end
