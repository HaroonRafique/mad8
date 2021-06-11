      subroutine plgacn(ncc, window, act, xreal, yreal, np, usex,
     +xwpos, xpos, ypos, ilb)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find suitable position for the curve annotation                    *
* Input:                                                               *
*   NCC      (integer)  current curve count (1,2, etc.)                *
*   WINDOW   (real)     array containing the window to use             *
*   ACT      (real)     window in NDC                                  *
*   XREAL    (real)     x values of curve                              *
*   YREAL    (real)     y values of curve                              *
*   NP       (integer)  no. of points to plot                          *
*   USEX     (real)     user character height expansion                *
* Output:                                                              *
*   XWPOS    (real)     x position of label in world coords.           *
*   XPOS     (real)     x pos. of label in NDC                         *
*   YPOS     (real)     y pos. of label in NDC                         *
*   ILB      (integer)  number of point behind label, or 0 if no       *
*                       label possible                                 *
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
      integer maux,maxitp,maxppt,mdsv,mint,mksmax,mntmax,mnvar,mpanno,
     +mpascl,mpbars,mpbtit,mpcolr,mpfelm,mpfont,mpfram,mplscl,mplscw,
     +mpmax,mpmin,mpmxvr,mpname,mpparn,mppcrn,mpsclf,mpspli,
     +mpsscl,mpstyl,mpsymb,mpsymf,mptscl,mpttit,mpvaxr,mpxsiz,mpysiz,
     +mqadd,mtbv,mtitl,musrv,mxdep,mxipar,mxlabl,mxqbnk,mxqcnd
 
      real              pflmax
 
      parameter         (mpparn = 11, mppcrn = 170)
      parameter         (mpmxvr = 5,  mxipar = 8, mtitl  = 128)
      parameter         (mxlabl = 40, pflmax = 1.e20)
      parameter         (mtbv = 6, mdsv = 3, musrv = 3)
      parameter         (maxppt = 20000, mnvar = 74, mxdep = 2)
      parameter         (mint = 10, maux = mint + 1, maxitp = 5000)
      parameter         (mxqcnd = 10, mxqbnk = 1000, mqadd = 100000)
      parameter         (mntmax = 20, mksmax = 10)
 
      parameter         (mpfont = 1, mpxsiz = 3, mpysiz = 4)
      parameter         (mplscl = 6, mptscl = 8, mpascl = 5)
      parameter         (mplscw = 2, mpsscl = 7, mpfelm = 9)
      parameter         (mpfram = 2, mpmin  = 1, mpmax  = 2)
      parameter         (mpsclf = 3, mpvaxr = 4, mpname = 5)
      parameter         (mpstyl = 1, mpspli = 2, mpbars = 3)
      parameter         (mpsymf = 4, mpcolr = 5, mpsymb = 6)
      parameter         (mpanno = 7)
      parameter         (mpttit = mpname + mtitl / mcwrd)
      parameter         (mpbtit = mpttit + mtitl / mcwrd)
 
*--- preceding parameters: see LPMAIN description (routine PLPLOT)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer i,iapos,ilb,iposx,iposy,iucomp,iy,j,mpost,mposx,mposy,ncc,
     +np
      double precision xadd,yadd
      parameter            (mposx = 8, mposy = 3,
     +                      mpost = mposx * mposy)
 
      real                 window(*), act(*), xreal(*), yreal(*)
      real                 xwpos, xpos, ypos, usex
      real                 ywpos, xmax, xmin, xdiff, ydiff, d, t, eps
      real                 xdiag(2,2), ydiag(2,2)
 
      integer              kapos(mposx, mposy)
      save kapos
 
*--- reset position array if first curve in frame
      if (ncc .eq. 1)  call vzero(kapos, mpost)
      xdiff = window(2) - window(1)
      ydiff = window(4) - window(3)
      eps = 1.e-6 * max(xdiff, ydiff)
      xmax  = xreal(1)
      xmin  = xmax
      do 10  i = 2, np
        xmin = min(xmin, xreal(i))
        xmax = max(xmax, xreal(i))
   10 continue
   20 continue
*--- find first unoccupied position
      iapos  = iucomp(0, kapos, mpost)
      if (iapos .eq. 0)  then
        ilb = 0
      else
        iposx  = mod (iapos-1, mposx) + 1
        iposy  = (iapos-1) / mposx + 1
        kapos(iposx,iposy) = -1
*--- annot. pos. in NDC
        xpos = act(1) +
     +  0.125 * usex * (iposx - .5) * (act(2) - act(1))
        ypos = act(4) -
     +  usex * (0.05 * (act(4) - act(3)) + 0.03 * (iposy - 1))
*---- annot. position in world coord.
        xwpos = window(1) + xpos * xdiff
*--- get next if outside x values of curve
        if (xwpos .le. xmin .or. xwpos .gt. xmax)  goto 20
        ywpos = window(3) + ypos * ydiff
*--- get endpoint of both diagonals of box
        xadd = 0.0625 * xdiff
        yadd = 0.03  * ydiff
        xdiag(1,1) = xwpos - xadd
        xdiag(2,1) = xwpos + xadd
        xdiag(1,2) = xwpos - xadd
        xdiag(2,2) = xwpos + xadd
        ydiag(1,1) = ywpos
        ydiag(2,1) = ywpos + yadd
        ydiag(1,2) = ywpos + yadd
        ydiag(2,2) = ywpos
*--- make sure no part of curve cuts these lines (curve approx. by
*    straight line segments)
        do 30  i = 2, np
          if (xwpos .gt. xreal(i-1) .and. xwpos .le. xreal(i)) ilb = i
          do 40  j = 1, 2
            d = (xdiag(2,j) - xdiag(1,j)) * (yreal(i-1) - yreal(i))
     +        - (ydiag(2,j) - ydiag(1,j)) * (xreal(i-1) - xreal(i))
            if (abs(d) .lt. eps)  goto 40
            t = (xreal(i-1) - xdiag(1,j)) * (yreal(i-1) - yreal(i))
     +        - (yreal(i-1) - ydiag(1,j)) * (xreal(i-1) - xreal(i))
            t = t / d
            if (t .lt. 0. .or. t .gt. 1.)  goto 40
            t = (xdiag(2,j) - xdiag(1,j)) * (yreal(i-1) - ydiag(1,j))
     +        - (ydiag(2,j) - ydiag(1,j)) * (xreal(i-1) - xdiag(1,j))
            t = t / d
            if (t .ge. 0. .and. t .le. 1.)  goto 20
   40     continue
   30   continue
      endif
      if (ilb .gt. 0)  then
        do 50  iy = 1, mposy
   50   kapos(iposx, iy) = 1
      endif
      do 60  i = 1, mpost
   60 kapos(i,1) = max(0, kapos(i,1))
      end
