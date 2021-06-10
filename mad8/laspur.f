      subroutine laspur(nord, fp, fm, gp, gm, tp, tm, min, max)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generic purifying routine for the static resonance case.           *
* Source:     MARYLIE, version 3.0 (routine GSPUR).                    *
* Input:                                                               *
*   NORD      (integer) Order of the map F (at most 4).                *
*   FP, FM    (map)     Original map to be purified (unchanged).       *
*   MIN, MAX  (integer) Index range to be purified.                    *
* Output:                                                              *
*   GP, GM    (map)     Purified map.                                  *
*   TP, TM    (map)     Purifying map, i. e. G = T*F*T**(-1).          *
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
      integer i,isave,it1p,it2p,k,k1,k2,max,min,n,nord,nx,ny
      double precision ax,ay,bx,by,cth,det,detmin,fm,fp,gm,gp,sth,tm,tp
      dimension         fp(*), fm(6,6), gp(*), gm(6,6), tp(*), tm(6,6)
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      parameter         (detmin = 1.0d-12)
      dimension         ax(-4:4), bx(-4:4), ay(-4:4), by(-4:4)
      integer           isrexp(0:2,209)
 
      data (isrexp(i,1),   i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,2),   i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,3),   i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,4),   i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,5),   i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,6),   i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,7),   i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,8),   i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,9),   i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,10),  i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,11),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,12),  i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,13),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,14),  i = 0, 2) / 1, 2, 0 /
      data (isrexp(i,15),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,16),  i = 0, 2) / 1, 0, 2 /
      data (isrexp(i,17),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,18),  i = 0, 2) / 1, 1, 1 /
      data (isrexp(i,19),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,20),  i = 0, 2) / 1, 1, -1 /
      data (isrexp(i,21),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,22),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,23),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,24),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,25),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,26),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,27),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,28),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,29),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,30),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,31),  i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,32),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,33),  i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,34),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,35),  i = 0, 2) / 1, 2, 0 /
      data (isrexp(i,36),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,37),  i = 0, 2) / 1, 0, 2 /
      data (isrexp(i,38),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,39),  i = 0, 2) / 1, 1, 1 /
      data (isrexp(i,40),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,41),  i = 0, 2) / 1, 1, -1 /
      data (isrexp(i,42),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,43),  i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,44),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,45),  i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,46),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,47),  i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,48),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,49),  i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,50),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,51),  i = 0, 2) / 1, 3, 0 /
      data (isrexp(i,52),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,53),  i = 0, 2) / 1, 0, 3 /
      data (isrexp(i,54),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,55),  i = 0, 2) / 1, 2, 1 /
      data (isrexp(i,56),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,57),  i = 0, 2) / 1, 1, 2 /
      data (isrexp(i,58),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,59),  i = 0, 2) / 1, 2, -1 /
      data (isrexp(i,60),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,61),  i = 0, 2) / 1, -1, 2 /
      data (isrexp(i,62),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,63),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,64),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,65),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,66),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,67),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,68),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,69),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,70),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,71),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,72),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,73),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,74),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,75),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,76),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,77),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,78),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,79),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,80),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,81),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,82),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,83),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,84),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,85),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,86),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,87),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,88),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,89),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,90),  i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,91),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,92),  i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,93),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,94),  i = 0, 2) / 1, 2, 0 /
      data (isrexp(i,95),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,96),  i = 0, 2) / 1, 0, 2 /
      data (isrexp(i,97),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,98),  i = 0, 2) / 1, 1, 1 /
      data (isrexp(i,99),  i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,100), i = 0, 2) / 1, 1, -1 /
      data (isrexp(i,101), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,102), i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,103), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,104), i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,105), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,106), i = 0, 2) / 1, 1, 0 /
      data (isrexp(i,107), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,108), i = 0, 2) / 1, 0, 1 /
      data (isrexp(i,109), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,110), i = 0, 2) / 1, 3, 0 /
      data (isrexp(i,111), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,112), i = 0, 2) / 1, 0, 3 /
      data (isrexp(i,113), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,114), i = 0, 2) / 1, 2, 1 /
      data (isrexp(i,115), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,116), i = 0, 2) / 1, 1, 2 /
      data (isrexp(i,117), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,118), i = 0, 2) / 1, 2, -1 /
      data (isrexp(i,119), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,120), i = 0, 2) / 1, -1, 2 /
      data (isrexp(i,121), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,122), i = 0, 2) / 1, 2, 0 /
      data (isrexp(i,123), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,124), i = 0, 2) / 1, 0, 2 /
      data (isrexp(i,125), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,126), i = 0, 2) / 1, 2, 0 /
      data (isrexp(i,127), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,128), i = 0, 2) / 1, 0, 2 /
      data (isrexp(i,129), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,130), i = 0, 2) / 1, 1, 1 /
      data (isrexp(i,131), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,132), i = 0, 2) / 1, 1, 1 /
      data (isrexp(i,133), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,134), i = 0, 2) / 1, 1, -1 /
      data (isrexp(i,135), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,136), i = 0, 2) / 1, -1, 1 /
      data (isrexp(i,137), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,138), i = 0, 2) / 1, 4, 0 /
      data (isrexp(i,139), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,140), i = 0, 2) / 1, 0, 4 /
      data (isrexp(i,141), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,142), i = 0, 2) / 1, 3, 1 /
      data (isrexp(i,143), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,144), i = 0, 2) / 1, 1, 3 /
      data (isrexp(i,145), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,146), i = 0, 2) / 1, 3, -1 /
      data (isrexp(i,147), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,148), i = 0, 2) / 1, -1, 3 /
      data (isrexp(i,149), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,150), i = 0, 2) / 1, 2, 2 /
      data (isrexp(i,151), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,152), i = 0, 2) / 1, 2, -2 /
      data (isrexp(i,153), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,154), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,155), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,156), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,157), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,158), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,159), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,160), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,161), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,162), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,163), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,164), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,165), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,166), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,167), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,168), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,169), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,170), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,171), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,172), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,173), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,174), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,175), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,176), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,177), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,178), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,179), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,180), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,181), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,182), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,183), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,184), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,185), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,186), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,187), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,188), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,189), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,190), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,191), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,192), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,193), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,194), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,195), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,196), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,197), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,198), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,199), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,200), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,201), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,202), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,203), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,204), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,205), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,206), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,207), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,208), i = 0, 2) / 0, 0, 0 /
      data (isrexp(i,209), i = 0, 2) / 0, 0, 0 /
 
*---- Allocate working space.
      isave = iwork
      it1p  = iwork
      it2p  = it1p + 209
      iwork = it2p + 209
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Set up multiple angle arrays.
      ax(0) = 1.0
      bx(0) = 0.0
      ay(0) = 1.0
      by(0) = 0.0
      ax(1) = fm(1,1)
      bx(1) = fm(1,2)
      ay(1) = fm(3,3)
      by(1) = fm(3,4)
 
      do 10 n = 2, 4
        ax(n) = ax(1) * ax(n-1) - bx(1) * bx(n-1)
        bx(n) = bx(1) * ax(n-1) + ax(1) * bx(n-1)
   10 continue
 
      do 20 n = 2, 4
        ay(n) = ay(1) * ay(n-1) - by(1) * by(n-1)
        by(n) = by(1) * ay(n-1) + ay(1) * by(n-1)
   20 continue
 
      do 30 n = 1, 4
        ax(-n) =   ax(n)
        bx(-n) = - bx(n)
        ay(-n) =   ay(n)
        by(-n) = - by(n)
   30 continue
 
*---- Resonance decompose map.
      call lasc2r(nord, fp, dq(it1p+1))
 
*---- Set up map to remove offensive terms of index MIN->MAX.
      call pa6clr(dq(it2p+1), -nord)
      do 100 k = min, max
        if (isrexp(0,k) .ne. 0) then
 
*---- Compute cth = cos(theta) and sth=sin(theta) for
*     THETA = SREXP(1,K)*WX + SREXP(2,K)*WY
          nx = isrexp(1,k)
          ny = isrexp(2,k)
          cth = ax(nx) * ay(ny) - bx(nx) * by(ny)
          sth = bx(nx) * ay(ny) + ax(nx) * by(ny)
 
*---- Carry out rest of calculation.
          det = 2.0 * (1.0 - cth)
          k1 = k
          k2 = k + 1
          if (abs(det) .gt. detmin) then
            dq(it2p+k1) =
     +        ((1.0 - cth) * dq(it1p+k1) + sth * dq(it1p+k2)) / det
            dq(it2p+k2) =
     +        ((1.0 - cth) * dq(it1p+k2) - sth * dq(it1p+k1)) / det
          else
            write (msg, 910) k1, k2, det
  910       format('DET(',i3,',',i3,') = ',f16.8,' not removed.')
            call aawarn('LADPUR', 1, msg)
          endif
        endif
  100 continue
 
*---- Transform map to Cartesian basis; the result is the map T.
      call lmone(nord, tp, tm)
      call lasr2c(nord, dq(it2p+1), tp)
 
*---- Remove offensive terms (MIN->MAX).
      call lmsand(nord, tp, tm, fp, fm, gp, gm)
 
*---- Drop working storage.
      iwork = isave
 
      end
