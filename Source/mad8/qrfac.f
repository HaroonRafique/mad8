      subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   This subroutine uses Householder transformations with column       *
*   pivoting (optional) to compute a QR factorization of the           *
*   M by N matrix a. That is, QRFAC determines an orthogonal           *
*   matrix Q, a permutation matrix P, and an upper trapezoidal         *
*   matrix R with diagonal elements of nonincreasing magnitude,        *
*   such that A*P = Q*R. The Householder transformation for            *
*   column K, K = 1,2,...,min(M,N), is of the form                     *
*                                                                      *
*                           T                                          *
*           I - (1/U(K))*U*U                                           *
*                                                                      *
*   where U has zeros in the first K-1 positions. The form of          *
*   this transformation and the method of pivoting first               *
*   appeared in the corresponding LINPACK subroutine.                  *
*                                                                      *
*       M is a positive integer input variable set to the number       *
*         of rows of A.                                                *
*                                                                      *
*       N is a positive integer input variable set to the number       *
*         of columns of A.                                             *
*                                                                      *
*       A is an M by N array. On input A contains the matrix for       *
*         which the QR factorization is to be computed. On output      *
*         The strict upper trapezoidal part of A contains the strict   *
*         upper trapezoidal part of R, and the lower trapezoidal       *
*         part of A contains a factored form of Q (the non-trivial     *
*         elements of the U vectors described above).                  *
*                                                                      *
*       LDA is a positive integer input variable not less than M       *
*         which specifies the leading dimension of the array A.        *
*                                                                      *
*       PIVOT is a logical input variable. If PIVOT is set true,       *
*         then column pivoting is enforced. If PIVOT is set false,     *
*         then no column pivoting is done.                             *
*                                                                      *
*       IPVT is an integer output array of length LIPVT. Ipvt          *
*         defines the permutation matrix P such that a*p = Q*r.        *
*         column J of P is column IPVT(J) of the identity matrix.      *
*         if PIVOT is false, IPVT is not referenced.                   *
*                                                                      *
*       LIPVT is a positive integer input variable. If PIVOT is false, *
*         then LIPVT may be as small as 1. If PIVOT is true, then      *
*         LIPVT must be at least N.                                    *
*                                                                      *
*       RDIAG is an output array of length N which contains the        *
*         diagonal elements of R.                                      *
*                                                                      *
*       ACNORM is an output array of length N which contains the       *
*         norms of the corresponding columns of the input matrix A.    *
*         If this information is not needed, then ACNORM can coincide  *
*         with RDIAG.                                                  *
*                                                                      *
*       WA is a work array of length N. If PIVOT is false, then WA     *
*         can coincide with RDIAG.                                     *
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
      integer i,j,k,kmax,lda,lipvt,m,minmn,n
      double precision a,acnorm,ajnorm,one,p05,rdiag,sum,temp,vmod,wa,
     +zero
      integer           ipvt(lipvt)
      logical           pivot
      dimension         a(lda,n), rdiag(n), acnorm(n), wa(n)
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
 
      parameter         (one    = 1.0d0)
      parameter         (p05    = 0.05d0)
      parameter         (zero   = 0.0d0)
 
*---- Compute the initial column norms and initialize several arrays.
      do 10 j = 1, n
         acnorm(j) = vmod(m, a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
   10 continue
 
*---- Reduce A to R with Householder transformations.
      minmn = min(m,n)
      do 110 j = 1, minmn
         if (pivot) then
 
*---- Bring the column of largest norm into the pivot position.
            kmax = j
            do 20 k = j, n
               if (rdiag(k) .gt. rdiag(kmax)) kmax = k
   20       continue
            if (kmax .ne. j) then
               do 30 i = 1, m
                  temp = a(i,j)
                  a(i,j) = a(i,kmax)
                  a(i,kmax) = temp
   30          continue
               rdiag(kmax) = rdiag(j)
               wa(kmax) = wa(j)
               k = ipvt(j)
               ipvt(j) = ipvt(kmax)
               ipvt(kmax) = k
            endif
         endif
 
*---- Compute the Householder transformation to reduce the
*     J-th column of A to a multiple of the J-th unit vector.
         ajnorm = vmod(m-j+1, a(j,j))
         if (ajnorm .ne. zero) then
            if (a(j,j) .lt. zero) ajnorm = -ajnorm
            do 50 i = j, m
               a(i,j) = a(i,j)/ajnorm
   50       continue
            a(j,j) = a(j,j) + one
 
*---- Apply the transformation to the remaining columns
*     and update the norms.
            do 90 k = j + 1, n
               sum = zero
               do 60 i = j, m
                  sum = sum + a(i,j)*a(i,k)
   60          continue
               temp = sum/a(j,j)
               do 70 i = j, m
                  a(i,k) = a(i,k) - temp*a(i,j)
   70          continue
               if (pivot .and. rdiag(k) .ne. zero) then
                  temp = a(j,k)/rdiag(k)
                  rdiag(k) = rdiag(k)*sqrt(max(zero,one-temp**2))
                  if (p05*(rdiag(k)/wa(k))**2 .le. epsmch) then
                     rdiag(k) = vmod(m-j, a(j+1,k))
                     wa(k) = rdiag(k)
                  endif
               endif
   90       continue
         endif
         rdiag(j) = -ajnorm
  110 continue
 
      end
