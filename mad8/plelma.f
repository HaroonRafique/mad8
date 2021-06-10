      subroutine plelma(itp, temp, dstp, am)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return 6x6 element matrix                                          *
* Input:                                                               *
*   ITP         (int)   type: 1 bend, 2 quad, else drift               *
*   TEMP        (real)  length, angle, k1                              *
*   DSTP        (real)  sublength                                      *
* Output:                                                              *
*   AM          (real   matrix                                         *
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
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      integer i,itp,j
      double precision am,cangle,d,dstp,elak1,elang,eleng,eps,f,fpr,one,
     +rhoi,rk2,sangle,temp,two,twopi,zero
      parameter         (zero = 0.d0, eps = 1.d-5)
      parameter         (one = 1.d0, two = 2.d0, twopi = two * pi)
 
      dimension         temp(*), am(6,6)
 
      logical           drift
 
      do i = 1, 6
        do j = 1,6
          am(j,i) = zero
        enddo
        am(i,i) = one
      enddo
*--- treat quads with non-zero tilt, and bends with non-zero tilt
*    different from pi/2 as drifts
      drift = itp .eq. 0 .or. (temp(5) .ne. zero
     +.and. (itp .ne. 1 .or. abs(pi / two - abs(temp(5))) .gt. eps))
      if (drift)  then
        do 20 i = 1, 4
   20   am(i,i) = one
        am(1,2) = dstp
        am(3,4) = dstp
      else
        if (temp(5) .eq. zero)  then
          fpr = one
        else
          fpr = -one
        endif
        eleng = temp(1)
        elang = temp(2)
        elak1 = temp(3)
        do 30 i = 0, 2, 2
          rhoi = ((one + fpr) / two) * elang / eleng
          rk2  = rhoi**2 + fpr * elak1
          call tmfoc(dstp, rk2, cangle, sangle, d, f)
          am(i+1,i+1) = cangle
          am(i+2,i+2) = cangle
          am(i+1,i+2) = sangle
          am(i+2,i+1) = -rk2 * sangle
          am(i+1,6)   = rhoi * d
          am(i+2,6)   = am(i+1,i+2) * rhoi
          fpr = -fpr
   30   continue
      endif
      end
