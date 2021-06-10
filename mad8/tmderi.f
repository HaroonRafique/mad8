      subroutine tmderi(tt, disp, rtp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Derivative of linear transfer matrix w.r.t. delta(p)/p.            *
* Input:                                                               *
*   TT(6,6,6) (real)    Second-order terms.                            *
*   DISP(6)   (real)    Dispersion.                                    *
* Output:                                                              *
*   RTP(6,6)  (real)    Derivative matrix.                             *
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
      double precision disp,rtp,temp,tt
      dimension         tt(6,6,6), disp(6), rtp(6,6)
 
      do 30 i = 1, 6
        do 20 k = 1, 6
          temp = 0.0
          do 10 j = 1, 6
            temp = temp + tt(i,j,k) * disp(j)
   10     continue
          rtp(i,k) = 2.0 * temp
   20   continue
   30 continue
 
      end
