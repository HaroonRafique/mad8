      subroutine ladpur(nord, fp, fm, gp, gm, tp, tm, min, max)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generic purifying routine for the dynamic resonance case.          *
* Source:     MARYLIE, version 3.0 (routine GDPUR).                    *
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
      integer i,isave,it1p,it2p,k,k1,k2,max,min,n,nord,nt,nx,ny
      double precision at,ax,ay,bt,bx,by,cth,det,detmin,fm,fp,gm,gp,sth,
     +tm,tp
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
      dimension         at(-4:4), bt(-4:4)
      integer           idrexp(0:3,209)
 
      data (idrexp(i,1),   i = 0, 3) / 1, 1, 0, 0 /
      data (idrexp(i,2),   i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,3),   i = 0, 3) / 1, 0, 1, 0 /
      data (idrexp(i,4),   i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,5),   i = 0, 3) / 1, 0, 0, 1 /
      data (idrexp(i,6),   i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,7),   i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,8),   i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,9),   i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,10),  i = 0, 3) / 1, 0, 0, 2 /
      data (idrexp(i,11),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,12),  i = 0, 3) / 1, 1, 0, 1 /
      data (idrexp(i,13),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,14),  i = 0, 3) / 1, 1, 0, -1 /
      data (idrexp(i,15),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,16),  i = 0, 3) / 1, 0, 1, 1 /
      data (idrexp(i,17),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,18),  i = 0, 3) / 1, 0, 1, -1 /
      data (idrexp(i,19),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,20),  i = 0, 3) / 1, 2, 0, 0 /
      data (idrexp(i,21),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,22),  i = 0, 3) / 1, 0, 2, 0 /
      data (idrexp(i,23),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,24),  i = 0, 3) / 1, 1, 1, 0 /
      data (idrexp(i,25),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,26),  i = 0, 3) / 1, 1, -1, 0 /
      data (idrexp(i,27),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,28),  i = 0, 3) / 1, 0, 0, 1 /
      data (idrexp(i,29),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,30),  i = 0, 3) / 1, 0, 0, 1 /
      data (idrexp(i,31),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,32),  i = 0, 3) / 1, 0, 0, 3 /
      data (idrexp(i,33),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,34),  i = 0, 3) / 1, 0, 0, 1 /
      data (idrexp(i,35),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,36),  i = 0, 3) / 1, 1, 0, 0 /
      data (idrexp(i,37),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,38),  i = 0, 3) / 1, 0, 1, 0 /
      data (idrexp(i,39),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,40),  i = 0, 3) / 1, 1, 0, 2 /
      data (idrexp(i,41),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,42),  i = 0, 3) / 1, 1, 0, -2 /
      data (idrexp(i,43),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,44),  i = 0, 3) / 1, 0, 1, 2 /
      data (idrexp(i,45),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,46),  i = 0, 3) / 1, 0, 1, -2 /
      data (idrexp(i,47),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,48),  i = 0, 3) / 1, 2, 0, 1 /
      data (idrexp(i,49),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,50),  i = 0, 3) / 1, 2, 0, -1 /
      data (idrexp(i,51),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,52),  i = 0, 3) / 1, 0, 2, 1 /
      data (idrexp(i,53),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,54),  i = 0, 3) / 1, 0, 2, -1 /
      data (idrexp(i,55),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,56),  i = 0, 3) / 1, 1, 1, 1 /
      data (idrexp(i,57),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,58),  i = 0, 3) / 1, 1, 1, -1 /
      data (idrexp(i,59),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,60),  i = 0, 3) / 1, 1, -1, 1 /
      data (idrexp(i,61),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,62),  i = 0, 3) / 1, 1, -1, -1 /
      data (idrexp(i,63),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,64),  i = 0, 3) / 1, 1, 0, 0 /
      data (idrexp(i,65),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,66),  i = 0, 3) / 1, 0, 1, 0 /
      data (idrexp(i,67),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,68),  i = 0, 3) / 1, 1, 0, 0 /
      data (idrexp(i,69),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,70),  i = 0, 3) / 1, 0, 1, 0 /
      data (idrexp(i,71),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,72),  i = 0, 3) / 1, 3, 0, 0 /
      data (idrexp(i,73),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,74),  i = 0, 3) / 1, 0, 3, 0 /
      data (idrexp(i,75),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,76),  i = 0, 3) / 1, 2, 1, 0 /
      data (idrexp(i,77),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,78),  i = 0, 3) / 1, 1, 2, 0 /
      data (idrexp(i,79),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,80),  i = 0, 3) / 1, 2, -1, 0 /
      data (idrexp(i,81),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,82),  i = 0, 3) / 1, 1, -2, 0 /
      data (idrexp(i,83),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,84),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,85),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,86),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,87),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,88),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,89),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,90),  i = 0, 3) / 1, 0, 0, 2 /
      data (idrexp(i,91),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,92),  i = 0, 3) / 1, 0, 0, 2 /
      data (idrexp(i,93),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,94),  i = 0, 3) / 1, 0, 0, 4 /
      data (idrexp(i,95),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,96),  i = 0, 3) / 1, 0, 0, 2 /
      data (idrexp(i,97),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,98),  i = 0, 3) / 1, 1, 0, 3 /
      data (idrexp(i,99),  i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,100), i = 0, 3) / 1, 1, 0, -3 /
      data (idrexp(i,101), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,102), i = 0, 3) / 1, 0, 1, 3 /
      data (idrexp(i,103), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,104), i = 0, 3) / 1, 0, 1, -3 /
      data (idrexp(i,105), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,106), i = 0, 3) / 1, 1, 0, 1 /
      data (idrexp(i,107), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,108), i = 0, 3) / 1, 1, 0, -1 /
      data (idrexp(i,109), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,110), i = 0, 3) / 1, 0, 1, 1 /
      data (idrexp(i,111), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,112), i = 0, 3) / 1, 0, 1, -1 /
      data (idrexp(i,113), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,114), i = 0, 3) / 1, 2, 0, 0 /
      data (idrexp(i,115), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,116), i = 0, 3) / 1, 0, 2, 0 /
      data (idrexp(i,117), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,118), i = 0, 3) / 1, 2, 0, 2 /
      data (idrexp(i,119), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,120), i = 0, 3) / 1, 2, 0, -2 /
      data (idrexp(i,121), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,122), i = 0, 3) / 1, 0, 2, 2 /
      data (idrexp(i,123), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,124), i = 0, 3) / 1, 0, 2, -2 /
      data (idrexp(i,125), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,126), i = 0, 3) / 1, 1, 1, 0 /
      data (idrexp(i,127), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,128), i = 0, 3) / 1, 1, -1, 0 /
      data (idrexp(i,129), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,130), i = 0, 3) / 1, 1, 1, 2 /
      data (idrexp(i,131), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,132), i = 0, 3) / 1, 1, 1, -2 /
      data (idrexp(i,133), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,134), i = 0, 3) / 1, 1, -1, 2 /
      data (idrexp(i,135), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,136), i = 0, 3) / 1, 1, -1, -2 /
      data (idrexp(i,137), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,138), i = 0, 3) / 1, 1, 0, 1 /
      data (idrexp(i,139), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,140), i = 0, 3) / 1, 1, 0, -1 /
      data (idrexp(i,141), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,142), i = 0, 3) / 1, 0, 1, 1 /
      data (idrexp(i,143), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,144), i = 0, 3) / 1, 0, 1, -1 /
      data (idrexp(i,145), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,146), i = 0, 3) / 1, 1, 0, 1 /
      data (idrexp(i,147), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,148), i = 0, 3) / 1, 1, 0, -1 /
      data (idrexp(i,149), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,150), i = 0, 3) / 1, 0, 1, 1 /
      data (idrexp(i,151), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,152), i = 0, 3) / 1, 0, 1, -1 /
      data (idrexp(i,153), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,154), i = 0, 3) / 1, 3, 0, 1 /
      data (idrexp(i,155), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,156), i = 0, 3) / 1, 3, 0, -1 /
      data (idrexp(i,157), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,158), i = 0, 3) / 1, 0, 3, 1 /
      data (idrexp(i,159), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,160), i = 0, 3) / 1, 0, 3, -1 /
      data (idrexp(i,161), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,162), i = 0, 3) / 1, 2, 1, 1 /
      data (idrexp(i,163), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,164), i = 0, 3) / 1, 2, 1, -1 /
      data (idrexp(i,165), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,166), i = 0, 3) / 1, 1, 2, 1 /
      data (idrexp(i,167), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,168), i = 0, 3) / 1, 1, 2, -1 /
      data (idrexp(i,169), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,170), i = 0, 3) / 1, 2, -1, 1 /
      data (idrexp(i,171), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,172), i = 0, 3) / 1, 2, -1, -1 /
      data (idrexp(i,173), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,174), i = 0, 3) / 1, 1, -2, -1 /
      data (idrexp(i,175), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,176), i = 0, 3) / 1, 1, -2, 1 /
      data (idrexp(i,177), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,178), i = 0, 3) / 1, 2, 0, 0 /
      data (idrexp(i,179), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,180), i = 0, 3) / 1, 0, 2, 0 /
      data (idrexp(i,181), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,182), i = 0, 3) / 1, 2, 0, 0 /
      data (idrexp(i,183), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,184), i = 0, 3) / 1, 0, 2, 0 /
      data (idrexp(i,185), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,186), i = 0, 3) / 1, 1, 1, 0 /
      data (idrexp(i,187), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,188), i = 0, 3) / 1, 1, 1, 0 /
      data (idrexp(i,189), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,190), i = 0, 3) / 1, 1, -1, 0 /
      data (idrexp(i,191), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,192), i = 0, 3) / 1, 1, -1, 0 /
      data (idrexp(i,193), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,194), i = 0, 3) / 1, 4, 0, 0 /
      data (idrexp(i,195), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,196), i = 0, 3) / 1, 0, 4, 0 /
      data (idrexp(i,197), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,198), i = 0, 3) / 1, 3, 1, 0 /
      data (idrexp(i,199), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,200), i = 0, 3) / 1, 1, 3, 0 /
      data (idrexp(i,201), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,202), i = 0, 3) / 1, 3, -1, 0 /
      data (idrexp(i,203), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,204), i = 0, 3) / 1, 1, -3, 0 /
      data (idrexp(i,205), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,206), i = 0, 3) / 1, 2, 2, 0 /
      data (idrexp(i,207), i = 0, 3) / 0, 0, 0, 0 /
      data (idrexp(i,208), i = 0, 3) / 1, 2, -2, 0 /
      data (idrexp(i,209), i = 0, 3) / 0, 0, 0, 0 /
 
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
      at(0) = 1.0
      bt(0) = 0.0
      ax(1) = fm(1,1)
      bx(1) = fm(1,2)
      ay(1) = fm(3,3)
      by(1) = fm(3,4)
      at(1) = fm(5,5)
      bt(1) = fm(5,6)
      do 10 n = 2, 4
        ax(n) = ax(1) * ax(n-1) - bx(1) * bx(n-1)
        bx(n) = bx(1) * ax(n-1) + ax(1) * bx(n-1)
   10 continue
      do 20 n = 2, 4
        ay(n) = ay(1) * ay(n-1) - by(1) * by(n-1)
        by(n) = by(1) * ay(n-1) + ay(1) * by(n-1)
   20 continue
      do 30 n = 2, 4
        at(n) = at(1) * at(n-1) - bt(1) * bt(n-1)
        bt(n) = bt(1) * at(n-1) + at(1) * bt(n-1)
   30 continue
      do 40 n = 1, 4
        ax(-n) =   ax(n)
        bx(-n) = - bx(n)
        ay(-n) =   ay(n)
        by(-n) = - by(n)
        at(-n) =   at(n)
        bt(-n) = - bt(n)
   40 continue
 
*---- Resonance decompose map.
      call ladc2r(nord, fp, dq(it1p+1))
 
*---- Set up map to remove offensive terms of index MIN->MAX.
      call pa6clr(dq(it2p+1), -nord)
      do 100 k = min, max
        if (idrexp(0,k) .ne. 0) then
 
*---- Compute CTH = COS(THETA) and STH=SIN(THETA) for
*     THETA = DREXP(1,K)*WX + DREXP(2,K)*WY + DREXP(3,K)*WT.
          nx = idrexp(1,k)
          ny = idrexp(2,k)
          nt = idrexp(3,k)
          cth = (ax(nx)*ay(ny) - bx(nx)*by(ny)) * at(nt)
     +        - (ax(nx)*by(ny) + bx(nx)*ay(ny)) * bt(nt)
          sth = (ax(nx)*ay(ny) - bx(nx)*by(ny)) * bt(nt)
     +        + (ax(nx)*by(ny) + bx(nx)*ay(ny)) * at(nt)
 
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
 
*---- Transfom map T2 to Cartesian basis; the result is the map T.
      call lmone(nord, tp, tm)
      call ladr2c(nord, dq(it2p+1), tp)
 
*---- Remove offensive terms (MIN->MAX).
      call lmsand(nord, tp, tm, fp, fm, gp, gm)
 
*---- Drop working storage.
      iwork = isave
 
      end
