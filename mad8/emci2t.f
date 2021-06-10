      subroutine emci2t(sigma, corr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Convert beam matrix internal form to TRANSPORT form.               *
* Input:                                                               *
*   SIGMA(6,6)(real)    Beam matrix in internal form.                  *
* Output:                                                              *
*   CORR(6,6) (real)    Standard deviations and lower correlations.    *
*----------------------------------------------------------------------*
* Modified: 12-MAR-1999, M. Woodley (SLAC)                             *
*   Change TOL from 1.0D-10 to 1.0D-24                                 *
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
      double precision corr,sigma,tol
      dimension         sigma(6,6), corr(6,6)
 
      parameter         (tol = 1.0d-24)
      do 10 j = 1, 6
        corr(j,j) = sqrt(abs(sigma(j,j)))
   10 continue
 
      do 30 k = 1, 5
        do 20 j = k + 1, 6
          if (corr(j,j) * corr(k,k) .gt. tol) then
            corr(j,k) = sigma(j,k) / (corr(j,j) * corr(k,k))
          else
            corr(j,k) = 0.0
          endif
          corr(k,j) = corr(j,k)
   20   continue
   30 continue
 
      end
