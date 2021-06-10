      subroutine htlsq(a, b, m, n, epsm, iter, x, ipiv, r, sqr, dot)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   This subroutine applies successive Householder transformations     *
*   to A and B in order to minimize the norm of  R = A*X - B.          *
* Input:                                                               *
*   A(M,N)    (real)    Input matrix (distroyed by HTLSQ).             *
*   B(M)      (real)    Right hand side (distroyed by HTLSQ).          *
*   EPSM      (real)    Tolerance.                                     *
*   ITER      (real)    Limit for number of transformations.           *
* Output:                                                              *
*   X(N)      (real)    Result vector.                                 *
* Working space:                                                       *
*   IPIV(N)   (integer) Permutation vector.                            *
*   R(M)      (real)    Residual vector.                               *
*   SQR(N)    (real)    Working space.                                 *
*   DOT(N)    (real)    Working space.                                 *
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
      integer i,itemp,iter,itmax,j,k,kpivot,m,n
      double precision a,b,beta,dot,eps1,epsm,gg,h,hh,pivot,pivott,r,
     +sigma,sqr,sqrmin,sum,temp,x
      dimension         a(m,n), b(m), x(n), r(m), sqr(n), dot(n)
      integer           ipiv(n)
 
*---- Find scalar products SQR(K)=A(K)*A(K) and DOT(K)=A(K)*B.
      sum = 0.0
      do 90 k = 1, n
        ipiv(k) = k
        hh = 0.0
        gg = 0.0
        do 80 i = 1, m
          hh = hh + a(i,k) * a(i,k)
          gg = gg + a(i,k) * b(i)
   80   continue
        sum = sum + hh
        sqr(k) = hh
        dot(k) = gg
   90 continue
      sqrmin = 1.e-8 * sum / n
 
*==== Begin of iteration loop.
      itmax = min(n,iter)
      if (itmax .eq. 0) itmax = n
      do 500 k = 1, itmax
 
*---- Search for largest change.
        pivot  = 0.0
        kpivot = 0
        do 110 j = k, n
          if (sqr(j) .gt. sqrmin) then
            pivott = dot(j) * dot(j) / sqr(j)
            if (pivott .gt. pivot) then
              kpivot = j
              pivot  = pivott
            endif
          endif
  110   continue
 
*---- If no suitable pivot found, stop.
        if (kpivot .eq. 0) then
          iter = k - 1
          go to 600
        endif
 
*---- Move pivot column to position.
        if (kpivot .gt. k) then
          temp        = sqr(k)
          sqr(k)      = sqr(kpivot)
          sqr(kpivot) = temp
          temp        = dot(k)
          dot(k)      = dot(kpivot)
          dot(kpivot) = temp
          itemp        = ipiv(k)
          ipiv(k)      = ipiv(kpivot)
          ipiv(kpivot) = itemp
          do 120 i = 1, m
            temp        = a(i,k)
            a(i,k)      = a(i,kpivot)
            a(i,kpivot) = temp
  120     continue
        endif
 
*---- Find beta, sigma, and vector U(K).
        hh = 0.0
        do 130 i = k, m
          hh = hh + a(i,k) * a(i,k)
  130   continue
        sigma = sign(sqrt(hh), a(k,k))
        sqr(k) = - sigma
        a(k,k) = a(k,k) + sigma
        beta = 1.0 / (a(k,k) * sigma)
 
*---- Transform remaining columns of A.
        do 250 j = k + 1, n
          hh = 0.0
          do 210 i = k, m
            hh = hh + a(i,k) * a(i,j)
  210     continue
          h = beta * hh
          do 220 i = k, m
            a(i,j) = a(i,j) - a(i,k) * h
  220     continue
  250   continue
 
*---- Transform vector B.
        hh = 0.0
        do 260 i = k, m
          hh = hh + a(i,k) * b(i)
  260   continue
        h = beta * hh
        do 270 i = k, m
          b(i) = b(i) - a(i,k) * h
  270   continue
 
*---- Update scalar products SQR(J)=A(J)*A(J) and DOT(J)=A(J)*B.
        do 310 j = k + 1, n
          sqr(j) = sqr(j) - a(k,j) * a(k,j)
          dot(j) = dot(j) - a(k,j) * b(k)
  310   continue
 
*---- Recalculate solution vector X.
        x(k) = b(k) / sqr(k)
        do 350 i = k - 1, 1, - 1
          x(i) = b(i)
          do 320 j = i + 1, k
            x(i) = x(i) - a(i,j) * x(j)
  320     continue
          x(i) = x(i) / sqr(i)
  350   continue
 
*---- Find original residual vector by backward transformation.
        do 410 i = 1, m
          r(i) = b(i)
  410   continue
        do 450 j = k, 1, - 1
          r(j) = 0.0
          hh = 0.0
          do 420 i = j, m
            hh = hh + a(i,j) * r(i)
  420     continue
          h = hh / (sqr(j) * a(j,j))
          do 430 i = j, m
            r(i) = r(i) + a(i,j) * h
  430     continue
  450   continue
 
*---- Check for convergence.
        eps1 = r(1)**2
        do 460 i = 2, m
          eps1 = eps1 + r(i)**2
  460   continue
        eps1 = sqrt(eps1/max(m,1))
        if (eps1 .le. epsm) then
          iter = k
          go to 600
        endif
  500 continue
 
*==== End of iteration loop.
      iter = itmax
  600 continue
 
*---- Re-order corrector strengths.
      do 610 k = 1, n
        sqr(k) = x(k)
        x(k) = 0.0
  610 continue
      do 620 k = 1, iter
        x(ipiv(k)) = sqr(k)
  620 continue
 
      end
