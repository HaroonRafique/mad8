      logical function m66sta(amat)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Check effect of a matrix on momentum.                              *
* Input:                                                               *
*   AMAT(6,6)   (real)  Input matrix.                                  *
* Result:                                                              *
*   .TRUE.              For static case     (constant p).              *
*   .FALSE.             For dynamic case    (variable p).              *
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
      integer j
      double precision amat,tol
      dimension         amat(6,6)
 
      parameter         (tol = 1.d-12)
 
      m66sta = abs(amat(6,6) - 1.0) .le. tol
      do 90 j = 1, 5
        m66sta = m66sta .and. abs(amat(6,j)) .le. tol
   90 continue
 
      end
