        subroutine pliact(kact, np, indx, x, y, ac, kf, kl)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return first and last point of curve inside active window          *
* Input:                                                               *
*   KACT        (int)   starting point for check                       *
*   NP          (int)   number of points in XVAL, YVAL                 *
*   INDX        (int)   order of values (possibly sorted)              *
*   X           (real)  x values                                       *
*   Y           (real)  y values                                       *
*   AC          (real)  active window in WC                            *
* Output:                                                              *
*   KF          (int)   first point inside, or 0                       *
*   KL          (int)   last  point inside, or 0                       *
*                                                                      *
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
      integer i,kact,kf,kl,np
      double precision toleps,xtol,ytol
      real x(*), y(*), ac(4)
      integer indx(*)
      parameter (toleps = 1.e-5)
 
      xtol = toleps * (ac(2) - ac(1))
      ytol = toleps * (ac(4) - ac(3))
      kf = 0
      kl = 0
      do 10  i = kact, np
        if(x(indx(i)) + xtol .lt. ac(1))  goto 10
        if(x(indx(i)) - xtol .gt. ac(2))  goto 10
        if(y(indx(i)) + ytol .lt. ac(3))  goto 10
        if(y(indx(i)) - ytol .gt. ac(4))  goto 10
        kf = i
        goto 20
   10 continue
*--- no point inside
      goto 999
   20 continue
      do 30  i = kf, np
        if(x(indx(i)) + xtol .lt. ac(1))  goto 40
        if(x(indx(i)) - xtol .gt. ac(2))  goto 40
        if(y(indx(i)) + ytol .lt. ac(3))  goto 40
        if(y(indx(i)) - ytol .gt. ac(4))  goto 40
   30 continue
   40 kl = i - 1
  999 end
