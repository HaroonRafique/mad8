      subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,wa1,wa2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Given an M by N matrix A, an N by N nonsingular diagonal           *
*   matrix D, an M-vector B, and a positive number DELTA,              *
*   the problem is to determine a value for the parameter              *
*   PAR such that if X solves the system                               *
*                                                                      *
*           A*X = B,     SQRT(PAR)*D*X = 0,                            *
*                                                                      *
*   in the least squares sense, and DXNORM is the Euclidean            *
*   norm of D*X, then either PAR is zero and                           *
*                                                                      *
*           (DXNORM-DELTA) .LE. 0.1*DELTA,                             *
*                                                                      *
*   or PAR is positive and                                             *
*                                                                      *
*           ABS(DXNORM-DELTA) .LE. 0.1*DELTA.                          *
*                                                                      *
*   This subroutine completes the solution of the problem              *
*   if it is provided with the necessary information from the          *
*   QR factorization, with column pivoting, of A. That is, if          *
*   A*P = Q*R, where P is a permutation matrix, Q has orthogonal       *
*   columns, and R is an upper triangular matrix with diagonal         *
*   elements of nonincreasing magnitude, then LMPAR expects            *
*   the full upper triangle of R, the permutation matrix P,            *
*   and the first N components of (Q transpose)*B. On output           *
*   LMPAR also provides an upper triangular matrix S such that         *
*                                                                      *
*            T   T                   T                                 *
*           P *(A *A + PAR*D*D)*P = S *S.                              *
*                                                                      *
*   S is employed within LMPAR and may be of separate interest.        *
*                                                                      *
*   Only a few iterations are generally needed for convergence         *
*   of the algorithm. If, however, the limit of 10 iterations          *
*   is reached, then the output PAR will contain the best              *
*   value obtained so far.                                             *
*                                                                      *
*       N is a positive integer input variable set to the order of R.  *
*                                                                      *
*       R is an N by N array. On input the full upper triangle         *
*         must contain the full upper triangle of the matrix R.        *
*         on output the full upper triangle is unaltered, and the      *
*         strict lower triangle contains the strict upper triangle     *
*         (transposed) of the upper triangular matrix S.               *
*                                                                      *
*       LDR is a positive integer input variable not less than N       *
*         which specifies the leading dimension of the array R.        *
*                                                                      *
*       IPVT is an integer input array of length N which defines the   *
*         permutation matrix P such that A*P = Q*R. column J of P      *
*         is column IPVT(J) of the identity matrix.                    *
*                                                                      *
*       DIAG is an input array of length N which must contain the      *
*         diagonal elements of the matrix D.                           *
*                                                                      *
*       QTB is an input array of length N which must contain the first *
*         N elements of the vector (Q transpose)*B.                    *
*                                                                      *
*       DELTA is a positive input variable which specifies an upper    *
*         bound on the Euclidean norm of D*X.                          *
*                                                                      *
*       PAR is a nonnegative variable. On input PAR contains an        *
*         initial estimate of the Levenberg-Marquardt parameter.       *
*         on output PAR contains the final estimate.                   *
*                                                                      *
*       X is an output array of length N which contains the least      *
*         squares solution of the system A*X = B, SQRT(PAR)*D*X = 0,   *
*         for the output PAR.                                          *
*                                                                      *
*       SDIAG is an output array of length N which contains the        *
*         diagonal elements of the upper triangular matrix S.          *
*                                                                      *
*       WA1 and WA2 are work arrays of length N.                       *
* Source:                                                              *
*   Argonne National Laboratory. MINPACK Project. March 1980.          *
*   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More.             *
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
      integer i,iter,j,k,l,ldr,n,nsing
      double precision delta,diag,dxnorm,fp,gnorm,p001,p1,par,parc,parl,
     +paru,qtb,r,sdiag,sum,temp,vmod,wa1,wa2,x,zero
      dimension         r(ldr,n), diag(n), qtb(n), x(n), sdiag(n),
     +                  wa1(n), wa2(n)
      integer           ipvt(n)
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
 
      parameter         (p1     = 0.1d0)
      parameter         (p001   = 0.001d0)
      parameter         (zero   = 0.0d0)
 
*---- Compute and store in X the Gauss-Newton direction. If the
*     Jacobian is rank-deficient, obtain a least squares solution.
      nsing = n
      do 10 j = 1, n
         wa1(j) = qtb(j)
         if (r(j,j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa1(j) = zero
   10 continue
      do 40 k = 1, nsing
         j = nsing - k + 1
         wa1(j) = wa1(j)/r(j,j)
         temp = wa1(j)
         do 20 i = 1, j - 1
            wa1(i) = wa1(i) - r(i,j)*temp
   20    continue
   40 continue
      do 60 j = 1, n
         l = ipvt(j)
         x(l) = wa1(j)
   60 continue
 
*---- Initialize the iteration counter.
*     Evaluate the function at the origin, and test
*     for acceptance of the Gauss-Newton direction.
      iter = 0
      do 70 j = 1, n
         wa2(j) = diag(j)*x(j)
   70 continue
      dxnorm = vmod(n, wa2)
      fp = dxnorm - delta
      if (fp .le. p1*delta) go to 220
 
*---- If the Jacobian is not rank deficient, the Newton
*     step provides a lower bound, PARL, for the zero of
*     the function. Otherwise set this bound to zero.
      parl = zero
      if (nsing .ge. n) then
         do 80 j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l)*(wa2(l)/dxnorm)
   80    continue
         do 110 j = 1, n
            sum = zero
            do 90 i = 1, j - 1
               sum = sum + r(i,j)*wa1(i)
   90       continue
            wa1(j) = (wa1(j) - sum)/r(j,j)
  110    continue
         temp = vmod(n, wa1)
         parl = ((fp/delta)/temp)/temp
      endif
 
*---- Calculate an upper bound, PARU, for the zero of the function.
      do 140 j = 1, n
         sum = zero
         do 130 i = 1, j
            sum = sum + r(i,j)*qtb(i)
  130    continue
         l = ipvt(j)
         wa1(j) = sum/diag(l)
  140 continue
      gnorm = vmod(n, wa1)
      paru = gnorm/delta
      if (paru .eq. zero) paru = fltmin/min(delta,p1)
 
*---- If the input PAR lies outside of the interval (PARL,PARU),
*     set PAR to the closer endpoint.
      par = max(par,parl)
      par = min(par,paru)
      if (par .eq. zero) par = gnorm/dxnorm
 
*---- Beginning of an iteration.
  150 continue
         iter = iter + 1
 
*---- Evaluate the function at the current value of PAR.
         if (par .eq. zero) par = max(fltmin,p001*paru)
         temp = sqrt(par)
         do 160 j = 1, n
            wa1(j) = temp*diag(j)
  160    continue
         call qrsolv(n,r,ldr,ipvt,wa1,qtb,x,sdiag,wa2)
         do 170 j = 1, n
            wa2(j) = diag(j)*x(j)
  170    continue
         dxnorm = vmod(n, wa2)
         temp = fp
         fp = dxnorm - delta
 
*---- If the function is small enough, accept the current value
*     of PAR. also test for the exceptional cases where PARL
*     is zero or the number of iterations has reached 10.
         if (abs(fp) .le. p1*delta
     +       .or. parl .eq. zero .and. fp .le. temp
     +            .and. temp .lt. zero .or. iter .eq. 10) go to 220
 
*---- Compute the Newton correction.
         do 180 j = 1, n
            l = ipvt(j)
            wa1(j) = diag(l)*(wa2(l)/dxnorm)
  180    continue
         do 210 j = 1, n
            wa1(j) = wa1(j)/sdiag(j)
            temp = wa1(j)
            do 190 i = j + 1, n
               wa1(i) = wa1(i) - r(i,j)*temp
  190       continue
  210    continue
         temp = vmod(n, wa1)
         parc = ((fp/delta)/temp)/temp
 
*---- Depending on the sign of the function, update PARL or PARU.
         if (fp .gt. zero) parl = max(parl,par)
         if (fp .lt. zero) paru = min(paru,par)
 
*---- Compute an improved estimate for PAR.
         par = max(parl,par+parc)
 
*---- End of an iteration.
         go to 150
  220 continue
 
*---- Termination.
      if (iter .eq. 0) par = zero
 
      end
