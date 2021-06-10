      subroutine fft(data, nn, isign)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Computes the FFT                                                   *
* Author: numerical receipes,  pg. 395                                 *
*   DATA:     is a real array with the signal on input                 *
*             with the Fourier transform on output.                    *
*   N:        is the number of data: must be a power of 2              *
*   ISIGN=1:  direct Fourier transform                                 *
*   ISIGN=-1: inverse Fourier transform                                *
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
      integer i,isign,istep,j,m,mmax,n,nn
      double precision data,half,one,tempi,tempr,theta,two,twopi,wi,wpi,
     +wpr,wr,wtemp,zero
      dimension data(*)
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter (zero = 0.0d0, half = 0.5d0, one = 1.0d0, two = 2.0d0,
     +           twopi = 2.0d0 * pi)
 
*---- Rearrange the data points.
      n = 2 * nn
      j = 1
      do 11 i = 1, n, 2
        if(j .gt. i) then
          tempr = data(j)
          tempi = data(j+1)
          data(j) = data(i)
          data(j+1) = data(i+1)
          data(i) = tempr
          data(i+1) = tempi
        endif
        m = n / 2
   1    if (m .ge. 2  .and.  j .gt. m) then
          j = j - m
          m = m / 2
          go to 1
        endif
        j = j + m
  11    continue
      mmax = 2
   2  if (n .gt. mmax) then
        istep = 2 * mmax
        theta = twopi / (isign * mmax)
        wpr = - two * sin(half * theta)**2
        wpi = sin(theta)
        wr = one
        wi = zero
        do 13 m = 1, mmax, 2
          do 12 i = m, n, istep
            j = i + mmax
            tempr = wr * data(j)   - wi * data(j+1)
            tempi = wr * data(j+1) + wi * data(j)
            data(j)   = data(i)   - tempr
            data(j+1) = data(i+1) - tempi
            data(i)   = data(i)   + tempr
            data(i+1) = data(i+1) + tempi
  12      continue
          wtemp = wr
          wr = wr * wpr - wi    * wpi + wr
          wi = wi * wpr + wtemp * wpi + wi
  13    continue
        mmax = istep
        go to 2
      endif
 
      end
