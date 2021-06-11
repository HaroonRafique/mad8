      subroutine ccperrf(xx, yy, wx, wy)
      implicit none
*----------------------------------------------------------------------*
* purpose:
*   modification of wwerf, double precision complex error function,
*   written at cern by K. Koelbig.
* input:
*   xx, yy    (double)    real + imag argument
* output:
*   wx, wy    (double)    real + imag function result
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
      double precision xx, yy, wx, wy
      double precision cc, one, two, xlim, ylim
      parameter         (cc     = 1.12837 91670 9551d0)
      parameter         (one    = 1.d0)
      parameter         (two    = 2.d0)
      parameter         (xlim   = 5.33d0)
      parameter         (ylim   = 4.29d0)
      double precision  x, y, q, h, xl, xh, yh, tx, ty, tn, sx, sy,
     +saux
      double precision  rx(33), ry(33)
      integer n, nc, nu
 
      x = abs(xx)
      y = abs(yy)
 
      if (y .lt. ylim  .and.  x .lt. xlim) then
        q  = (one - y / ylim) * sqrt(one - (x/xlim)**2)
        h  = one / (3.2d0 * q)
        nc = 7 + int(23.0*q)
        xl = h**(1 - nc)
        xh = y + 0.5d0/h
        yh = x
        nu = 10 + int(21.0*q)
        rx(nu+1) = 0.
        ry(nu+1) = 0.
 
        do 10 n = nu, 1, -1
          tx = xh + n * rx(n+1)
          ty = yh - n * ry(n+1)
          tn = tx*tx + ty*ty
          rx(n) = 0.5d0 * tx / tn
          ry(n) = 0.5d0 * ty / tn
   10   continue
 
        sx = 0.
        sy = 0.
 
        do 20 n = nc, 1, -1
          saux = sx + xl
          sx = rx(n) * saux - ry(n) * sy
          sy = rx(n) * sy + ry(n) * saux
          xl = h * xl
   20   continue
 
        wx = cc * sx
        wy = cc * sy
      else
        xh = y
        yh = x
        rx(1) = 0.
        ry(1) = 0.
 
        do 30 n = 9, 1, -1
          tx = xh + n * rx(1)
          ty = yh - n * ry(1)
          tn = tx*tx + ty*ty
          rx(1) = 0.5d0 * tx / tn
          ry(1) = 0.5d0 * ty / tn
   30   continue
 
        wx = cc * rx(1)
        wy = cc * ry(1)
      endif
 
*      if(y .eq. 0.) wx = exp(-x**2)
      if(yy .lt. 0.) then
        wx =   two * exp(y*y-x*x) * cos(two*x*y) - wx
        wy = - two * exp(y*y-x*x) * sin(two*x*y) - wy
        if(xx .gt. 0.) wy = -wy
      else
        if(xx .lt. 0.) wy = -wy
      endif
 
      end
