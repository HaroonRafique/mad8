      subroutine emct2i(corr, sigma)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Convert sigma matrix from TRANSPORT to internal form.              *
* Input:                                                               *
*   CORR(6,6) (real)    Standard deviations and lower correlations.    *
* Output:                                                              *
*   SIGMA(6,6)(real)    Beam matrix in internal form.                  *
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
      integer j,k
      double precision corr,sigma
      dimension         corr(6,6), sigma(6,6)
 
      do 20 k = 1, 5
        do 10 j = k + 1, 6
          sigma(j,k) = corr(j,j) * corr(k,k) * corr(j,k)
          sigma(k,j) = sigma(j,k)
   10   continue
   20 continue
 
      do 30 j = 1, 6
        sigma(j,j) = corr(j,j)**2
   30 continue
 
      end
