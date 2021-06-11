      subroutine tmrefl(rs, ts, rd, td)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Reflect a TRANSPORT map.                                           *
* Input:                                                               *
*   RS(6,6), TS(6,6,6)  Source map.                                    *
* Output:                                                              *
*   RD(6,6), TD(6,6,6)  Destination map.                               *
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
      integer i,j,k
      double precision rd,rs,s,sij,td,ts
      dimension         rs(6,6), ts(6,6,6), rd(6,6), td(6,6,6)
      dimension         s(6)
      data  s         / 1.0d0, -1.0d0, +1.0d0, -1.0d0, -1.0d0, +1.0d0 /
 
      call tminv(rs, ts, rd, td)
      do 20 i = 1, 6
      do 20 j = 1, 6
        sij = s(i) * s(j)
        rd(i,j) = rd(i,j) * sij
        do 10 k = 1, 6
          td(i,j,k) = td(i,j,k) * sij * s(k)
   10   continue
   20 continue
 
      end
