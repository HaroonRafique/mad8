      subroutine orthes(ndim, n, ilow, iupp, a, d)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Converts an unsymmetric real matrix, A, to upper Hessenberg form   *
*   applying successive orthogonal transformations.                    *
*                                                                      *
*   Translation of the ALGOL procedure ORTHES in:                      *
*   Handbook Series Linear Algebra,                                    *
*   Num. Math. 12, 349-368 (1968) by R. S. Martin and J. H. Wilkinson. *
* Input:                                                               *
*   N         (integer) Order of the matrix A.                         *
*   ILOW,IUPP (integer) Determine a submatrix, set by BALANC.          *
*                       May be set to 1 and N respectively.            *
*   A(NDIM,N) (real)    Input matrix.                                  *
* Output:                                                              *
*   A(NDIM,N) (real)    The matrix A, converted to upper Hessenberg.   *
*                       The lower triangle contains information        *
*                       about the orthogonal transformations.          *
*   D(N)      (real)    Further information.                           *
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
      integer i,ilow,iupp,j,m,n,ndim
      double precision a,d,f,g,h,scale
      dimension         a(ndim,n), d(n)
 
      do 90 m = ilow + 1, iupp - 1
        h = 0.0
        d(m) = 0.0
 
*---- Find scale factor.
        scale = 0.0
        do 10 i = m, iupp
          scale = scale + abs(a(i,m-1))
   10   continue
        if (scale .ne. 0.0) then
          do 20 i = iupp, m, - 1
            d(i) = a(i,m-1) / scale
            h = h + d(i) * d(i)
   20     continue
 
          g = sign(sqrt(h),d(m))
          h = h + d(m) * g
          d(m) = d(m) + g
 
*---- Form (I - (u*uT) / h) * A.
          do 50 j = m, n
            f = 0.0
            do 30 i = iupp, m, - 1
              f = f + d(i) * a(i,j)
   30       continue
            f = f / h
            do 40 i = m, iupp
              a(i,j) = a(i,j) - f * d(i)
   40       continue
 
   50     continue
 
*---- Form (I - (u*uT) / h) * A * (I - (u*uT) / h).
          do 80 i = 1, iupp
            f = 0.0
            do 60 j = iupp, m, - 1
              f = f + d(j) * a(i,j)
   60       continue
            f = f / h
            do 70 j = m, iupp
              a(i,j) = a(i,j) - f * d(j)
   70       continue
   80     continue
 
          d(m) = scale * d(m)
          a(m,m-1) = - scale * g
        endif
   90 continue
 
      end
