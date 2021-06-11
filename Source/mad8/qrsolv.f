      subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Given an M by N matrix A, an N by N diagonal matrix D,             *
*   and an M-vector B, the problem is to determine an X which          *
*   solves the system                                                  *
*                                                                      *
*           A*X = B,     D*X = 0,                                      *
*                                                                      *
*   in the least squares sense.                                        *
*                                                                      *
*   This subroutine completes the solution of the problem              *
*   if it is provided with the necessary information from the          *
*   QR factorization, with column pivoting, of A. That is, if          *
*   A*P = Q*R, where P is a permutation matrix, Q has orthogonal       *
*   columns, and R is an upper triangular matrix with diagonal         *
*   elements of nonincreasing magnitude, then QRSOLV expects           *
*   the full upper triangle of R, the permutation matrix P,            *
*   and the first N components of (Q transpose)*B. The system          *
*   A*X = B, D*X = 0, is then equivalent to                            *
*                                                                      *
*                  T      T                                            *
*           R*Z = Q *B,  P *D*P*Z = 0,                                 *
*                                                                      *
*   where X = P*Z. If this system does not have full rank,             *
*   then a least squares solution is obtained. On output QRSOLV        *
*   also provides an upper triangular matrix S such that               *
*                                                                      *
*            T   T               T                                     *
*           P *(A *A + D*D)*P = S *S.                                  *
*                                                                      *
*     S is computed within QRSOLV and may be of separate interest.     *
*                                                                      *
*       N is a positive integer input variable set to the order of R.  *
*                                                                      *
*       R is an N by N array. On input the full upper triangle         *
*         must contain the full upper triangle of the matrix R.        *
*         On output the full upper triangle is unaltered, and the      *
*         strict lower triangle contains the strict upper triangle     *
*         (transposed) of the upper triangular matrix S.               *
*                                                                      *
*       LDR is a positive integer input variable not less than N       *
*         which specifies the leading dimension of the array R.        *
*                                                                      *
*       IPVT is an integer input array of length N which defines the   *
*         permutation matrix P such that A*P = Q*R. Column J of P      *
*         is column IPVT(J) of the identity matrix.                    *
*                                                                      *
*       DIAG is an input array of length N which must contain the      *
*         diagonal elements of the matrix D.                           *
*                                                                      *
*       QTB is an input array of length N which must contain the first *
*         N elements of the vector (Q transpose)*B.                    *
*                                                                      *
*       X is an output array of length N which contains the least      *
*         squares solution of the system A*X = B, D*X = 0.             *
*                                                                      *
*       SDIAG is an output array of length N which contains the        *
*         diagonal elements of the upper triangular matrix S.          *
*                                                                      *
*       WA is a work array of length N.                                *
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
      integer i,j,k,l,ldr,n,nsing
      double precision cos,cotan,diag,p25,p5,qtb,qtbpj,r,sdiag,sin,sum,
     +tan,temp,wa,x,zero
      integer           ipvt(n)
      dimension         r(ldr,n), diag(n), qtb(n), x(n), sdiag(n), wa(n)
 
      parameter         (p5     = 0.5d0)
      parameter         (p25    = 0.25d0)
      parameter         (zero   = 0.0d0)
 
*---- Copy R and (Q transpose)*B to preserve input and initialize S.
*     In particular, save the diagonal elements of R in X.
      do 20 j = 1, n
         do 10 i = j, n
            r(i,j) = r(j,i)
   10    continue
         x(j) = r(j,j)
         wa(j) = qtb(j)
   20 continue
 
*---- Eliminate the diagonal matrix D using a Givens rotation.
      do 100 j = 1, n
 
*---- Prepare the row of D to be eliminated, locating the
*     diagonal element using P from the QR factorization.
         l = ipvt(j)
         if (diag(l) .ne. zero) then
            do 30 k = j, n
               sdiag(k) = zero
   30       continue
            sdiag(j) = diag(l)
 
*---- The transformations to eliminate the row of D
*     modify only a single element of (Q transpose)*B
*     beyond the first N, which is initially zero.
            qtbpj = zero
            do 80 k = j, n
 
*---- Determine a Givens rotation which eliminates the
*     appropriate element in the current row of D.
               if (sdiag(k) .ne. zero) then
                  if (abs(r(k,k)) .lt. abs(sdiag(k))) then
                     cotan = r(k,k)/sdiag(k)
                     sin = p5/sqrt(p25+p25*cotan**2)
                     cos = sin*cotan
                  else
                     tan = sdiag(k)/r(k,k)
                     cos = p5/sqrt(p25+p25*tan**2)
                     sin = cos*tan
                  endif
 
*---- Compute the modified diagonal element of R and
*     the modified element of ((Q transpose)*b,0).
                  r(k,k) = cos*r(k,k) + sin*sdiag(k)
                  temp = cos*wa(k) + sin*qtbpj
                  qtbpj = -sin*wa(k) + cos*qtbpj
                  wa(k) = temp
 
*---- Accumulate the tranformation in the row of S.
                  do 60 i = k + 1, n
                     temp = cos*r(i,k) + sin*sdiag(i)
                     sdiag(i) = -sin*r(i,k) + cos*sdiag(i)
                     r(i,k) = temp
   60             continue
               endif
   80       continue
         endif
 
*---- Store the diagonal element of S and restore
*     the corresponding diagonal element of R.
         sdiag(j) = r(j,j)
         r(j,j) = x(j)
  100 continue
 
*---- Solve the triangular system for z. If the system is
*     singular, then obtain a least squares solution.
      nsing = n
      do 110 j = 1, n
         if (sdiag(j) .eq. zero .and. nsing .eq. n) nsing = j - 1
         if (nsing .lt. n) wa(j) = zero
  110 continue
      do 140 j = nsing, 1, - 1
         sum = zero
         do 120 i = j + 1, nsing
            sum = sum + r(i,j)*wa(i)
  120       continue
         wa(j) = (wa(j) - sum)/sdiag(j)
  140 continue
 
*---- Permute the components of Z back to components of X.
      do 160 j = 1, n
         l = ipvt(j)
         x(l) = wa(j)
  160 continue
 
      end
