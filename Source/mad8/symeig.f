      subroutine symeig(a, nd, n, eigen, nval)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Eigenvalues of a real symmetric matrix in ascending order.         *
* Input:                                                               *
*   A(ND,ND)  (real)    Symmetric input matrix; destroyed by call.     *
*   N         (integer) Rank of matrix.                                *
* Output:                                                              *
*   EIGEN(*)  (real)    Eigenvalues of A in descending order.          *
*   NVAL      (integer) Number of eigenvalues found.                   *
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
      integer i,it,itmax,iw,j,k,l,m,n,nd,nval
      double precision a,b,c,eigen,eps,f,g,h,p,r,s
      dimension         a(nd,nd), eigen(nd)
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      parameter         (eps = 1.0d-20)
      parameter         (itmax = 15)
 
*---- Allocate working space.
      iw = iwork
      iwork = iw + nd
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Matrix is 1 * 1.
      nval = n
      if (n .le. 0) go to 300
      if (n .eq. 1) then
        eigen(1) = a(1,1)
        go to 300
      endif
 
*---- Matrix is 2 * 2.
      if (n .eq. 2) then
        f = a(1,1) + a(2,2)
        g = sqrt((a(1,1) - a(2,2))**2 + 4.0 * a(2,1)**2)
        eigen(1) = (f - g) / 2.0
        eigen(2) = (f + g) / 2.0
        go to 300
      endif
 
*---- N is at least 3, reduce to tridiagonal form.
      do 90 i = n, 3, -1
        g = 0.0
        do 10 k = 1, i-2
          g = g + a(i,k)**2
   10   continue
        eigen(i) = a(i,i)
        if (g .eq. 0.0) then
          dq(iw+i) = a(i,i-1)
        else
          h = g + a(i,i-1)**2
          dq(iw+i) = sign(sqrt(h),a(i,i-1))
          h = h + a(i,i-1) * dq(iw+i)
          a(i,i-1) = a(i,i-1) + dq(iw+i)
          f = 0.0
          do 50 j = 1, i-1
            g = 0.0
            do 40 k = 1, i-1
              if (k .le. j) then
                g = g + a(j,k) * a(i,k)
              else
                g = g + a(k,j) * a(i,k)
              endif
   40       continue
            dq(iw+j) = g / h
            f = f + dq(iw+j) * a(i,j)
   50     continue
          do 70 j = 1, i-1
            dq(iw+j) = dq(iw+j) - (f / (h + h)) * a(i,j)
            do 60 k = 1, j
              a(j,k) = a(j,k) - a(i,j) * dq(iw+k) - dq(iw+j) * a(i,k)
   60       continue
   70     continue
        endif
   90 continue
      dq(iw+2) = a(2,1)
      dq(iw+1) = 0.0
      eigen(2) = a(2,2)
      eigen(1) = a(1,1)
 
*---- Iterate on tridiagonal matrix.
      do 110 i = 2, n
        dq(iw+i-1) = dq(iw+i)
  110 continue
 
      dq(iw+n) = 0.0
      f = 0.0
      b = 0.0
      do 200 l = 1, n
        b = max(eps*(abs(eigen(l))+abs(dq(iw+l))),b)
        do 120 m = l, n
          if (abs(dq(iw+m)) .le. b) go to 130
  120   continue
        m = n
  130   if (m .ne. l) then
          do 160 it = 1, itmax
            p = (eigen(l+1) - eigen(l)) / (2.0 * dq(iw+l))
            if (abs(p) .gt. 1.0e10) then
              r = abs(p)
            else
              r = sqrt(p*p+1.0)
            endif
            h = eigen(l) - dq(iw+l) / (p + sign(r,p))
            do 140 i = l, n
              eigen(i) = eigen(i) - h
  140       continue
            f = f + h
            p = eigen(m)
            c = 1.0
            s = 0.0
            do 150 i = m-1, l, -1
              g = c * dq(iw+i)
              h = c * p
              r = sqrt(dq(iw+i)**2+p**2)
              dq(iw+i+1) = s * r
              s = dq(iw+i) / r
              c = p / r
              p = c * eigen(i) - s * g
              eigen(i+1) = h + s * (c * g + s * eigen(i))
  150       continue
            dq(iw+l) = s * p
            eigen(l) = c * p
            if (abs(dq(iw+l)) .le. b) go to 170
  160     continue
          nval = l - 1
          go to 300
        endif
  170   p = eigen(l) + f
        do 180 i = l, 2, -1
          if (p .ge. eigen(i-1)) go to 190
          eigen(i) = eigen(i-1)
  180   continue
        i = 1
  190   eigen(i) = p
  200 continue
 
*---- Release working space.
  300 continue
      iwork = iw
 
      end
