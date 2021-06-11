      double precision function slopexy(vectorx, vectory, nturn)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Computes slope a for linear fit of Y = a X + b.                    *
* VECTORX  is the array of abscissas X,                                *
* VECTORX  is the array of ordinates Y to interpolate,                 *
* NTURN    is their dimension.                                         *
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
      integer mf,nturn
      double precision vectorx,vectory,x2mean,xmean,xymean,ymean
 
      dimension vectorx(*), vectory(*)
 
      xmean  = 0.d0
      ymean  = 0.d0
      x2mean = 0.d0
      xymean = 0.d0
 
      do mf = 1, nturn
        xmean  = xmean + vectorx(mf)
        ymean  = ymean + vectory(mf)
        x2mean = x2mean + vectorx(mf)**2
        xymean = xymean + vectorx(mf) * vectory(mf)
      enddo
 
      xmean  = xmean / nturn
      ymean  = ymean / nturn
      x2mean = x2mean / nturn
      xymean = xymean / nturn
 
      slopexy = (xymean - xmean * ymean) / (x2mean - xmean**2)
 
      end
