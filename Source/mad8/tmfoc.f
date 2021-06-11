      subroutine tmfoc(el, sk1, c, s, d, f)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute linear focussing functions.                                *
* Input:                                                               *
*   EL        (real)    Length.                                        *
*   SK1       (real)    Quadrupole strength.                           *
* Output:                                                              *
*   C         (real)    Cosine-like function.             c(k,l)       *
*   S         (real)    Sine-like function.               s(k,l)       *
*   D         (real)    Dispersion function.              d(k,l)       *
*   F         (real)    Integral of dispersion function.  f(k,l)       *
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
      double precision c,d,el,f,qk,qkl,qkl2,s,sk1
 
      qk = sqrt(abs(sk1))
      qkl = qk * el
      qkl2 = sk1 * el**2
 
      if (abs(qkl2) .le. 1.0e-2) then
        c = (1.0 - qkl2 * (1.0 - qkl2 / 12.0) /  2.0)
        s = (1.0 - qkl2 * (1.0 - qkl2 / 20.0) /  6.0) * el
        d = (1.0 - qkl2 * (1.0 - qkl2 / 30.0) / 12.0) * el**2 / 2.0
        f = (1.0 - qkl2 * (1.0 - qkl2 / 42.0) / 20.0) * el**3 / 6.0
      else
        if (qkl2 .gt. 0.0) then
          c = cos(qkl)
          s = sin(qkl) / qk
        else
          c = cosh(qkl)
          s = sinh(qkl) / qk
        endif
        d = (1.0 - c) / sk1
        f = (el  - s) / sk1
      endif
 
      end
