      subroutine tmtilt(fsec, tilt, ek, r, t)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Apply TILT to a TRANSPORT map.                                     *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   TILT      (real)    Roll angle.                                    *
*   EK(6)     (real)    Element kick, unrotated.                       *
*   R(6,6)    (real)    Transfer matrix, unrotated.                    *
*   T(6,6,6)  (real)    Second order terms, unrotated.                 *
* Output:                                                              *
*   EK(6)     (real)    Element kick, rotated.                         *
*   R(6,6)    (real)    Transfer matrix, rotated.                      *
*   T(6,6,6)  (real)    Second order terms, rotated.                   *
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
      double precision c,ek,r,r1j,r2j,ri1,ri2,s,t,t1jk,t2jk,ti1k,ti2k,
     +tij1,tij2,tilt,xx
      logical           fsec
      dimension         ek(6), r(6,6), t(6,6,6)
 
      c = cos(tilt)
      s = sin(tilt)
 
*---- Rotate at entrance.
      do 40 i = 1, 6
        ri1 = r(i,1)
        r(i,1) = ri1 * c - r(i,3) * s
        r(i,3) = ri1 * s + r(i,3) * c
        ri2 = r(i,2)
        r(i,2) = ri2 * c - r(i,4) * s
        r(i,4) = ri2 * s + r(i,4) * c
 
        if (fsec) then
          do 10 k = 1, 6
            ti1k = t(i,1,k)
            t(i,1,k) = ti1k * c - t(i,3,k) * s
            t(i,3,k) = ti1k * s + t(i,3,k) * c
            ti2k = t(i,2,k)
            t(i,2,k) = ti2k * c - t(i,4,k) * s
            t(i,4,k) = ti2k * s + t(i,4,k) * c
   10     continue
          do 20 j = 1, 6
            tij1 = t(i,j,1)
            t(i,j,1) = tij1 * c - t(i,j,3) * s
            t(i,j,3) = tij1 * s + t(i,j,3) * c
            tij2 = t(i,j,2)
            t(i,j,2) = tij2 * c - t(i,j,4) * s
            t(i,j,4) = tij2 * s + t(i,j,4) * c
   20     continue
        endif
   40 continue
 
*---- Rotate kick.
      xx = ek(1)
      ek(1) = xx * c - ek(3) * s
      ek(3) = xx * s + ek(3) * c
      xx = ek(2)
      ek(2) = xx * c - ek(4) * s
      ek(4) = xx * s + ek(4) * c
 
*---- Rotate at exit.
      do 90 j = 1, 6
        r1j = r(1,j)
        r(1,j) = c * r1j - s * r(3,j)
        r(3,j) = s * r1j + c * r(3,j)
        r2j = r(2,j)
        r(2,j) = c * r2j - s * r(4,j)
        r(4,j) = s * r2j + c * r(4,j)
        if (fsec) then
          do 60 k = 1, 6
            t1jk = t(1,j,k)
            t(1,j,k) = c * t1jk - s * t(3,j,k)
            t(3,j,k) = s * t1jk + c * t(3,j,k)
            t2jk = t(2,j,k)
            t(2,j,k) = c * t2jk - s * t(4,j,k)
            t(4,j,k) = s * t2jk + c * t(4,j,k)
   60     continue
        endif
   90 continue
 
      end
