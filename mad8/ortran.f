      subroutine ortran(ndim, n, ilow, iupp, h, d, v)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Accumulate the orthogonal similarity transformation used by        *
*   ORTHES to reduce a general real matrix A to upper Hessenberg form. *
*                                                                      *
*   Translation of the ALGOL procedure ORTRANS in:                     *
*   Handbook Series Linear Algebra,                                    *
*   Num. Math. 16, 181-204 (1970) by G. Peters and J. H. Wilkinson.    *
* Input:                                                               *
*   N         (integer) Order of the matrices A and V.                 *
*   ILOW,IUPP (integer) Determine a sub-matrix set by BALANC.          *
*                       May be set to 1 and N respectively.            *
*   H(NDIM,N) (real)    The matrix resulting from running ORTHES.      *
*   D(N)      (real)    Further information about the transformation.  *
* Output:                                                              *
*   V(NDIM,N) (real)    The accumulated transformation.                *
*   D(N)      (real)    Destroyed.                                     *
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
      integer i,ilow,iupp,j,k,m,n,ndim
      double precision d,h,v,x,y
      dimension         h(ndim,n), d(n), v(ndim,n)
 
*---- Initialize V to identity matrix.
      do 20 i = 1, n
        do 10 j = 1, n
          v(i,j) = 0.0
   10   continue
        v(i,i) = 1.0
   20 continue
 
*---- Accumulate transformations.
      do 90 k = iupp - 2, ilow, - 1
        m = k + 1
        y = h(m,k)
        if (y .ne. 0.0) then
          y = y * d(m)
 
          do 30 i = k + 2, iupp
            d(i) = h(i,k)
   30     continue
*
          do 60 j = m, iupp
            x = 0.0
            do 40 i = m, iupp
              x = x + d(i) * v(i,j)
   40       continue
            x = x / y
            do 50 i = m, iupp
              v(i,j) = v(i,j) + x * d(i)
   50       continue
   60     continue
        endif
   90 continue
 
      end
