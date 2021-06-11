      subroutine tminv(rs, ts, rd, td)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Invert a TRANSPORT map.                                            *
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
      integer i,i1,i2,i3,k
      double precision rd,rs,t,td,tr,trr,ts
      dimension         rs(6,6), ts(6,6,6), rd(6,6), td(6,6,6)
      dimension         tr(6), trr(6,6,6)
 
*---- Invert first order part.
      call m66inv(rs, rd)
 
*---- Invert second order part.
      do 50 i1 = 1, 6
      do 50 i2 = 1, 6
        do 20 i = 1, 6
          t = 0.0
          do 10 k = 1, 6
            t = t + ts(i1,i,k) * rd(k,i2)
   10     continue
          tr(i) = t
   20   continue
        do 40 i3 = i2, 6
          t = 0.0
          do 30 i = 1, 6
            t = t + tr(i) * rd(i,i3)
   30     continue
          trr(i1,i2,i3) = t
   40   continue
   50 continue
 
      do 70 i1 = 1, 6
      do 70 i2 = 1, 6
      do 70 i3 = i2, 6
        t = 0.0
        do 60 i = 1, 6
          t = t - rd(i1,i) * trr(i,i2,i3)
   60   continue
        td(i1,i2,i3) = t
        td(i1,i3,i2) = t
   70 continue
 
      end
