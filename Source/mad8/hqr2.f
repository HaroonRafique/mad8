      subroutine hqr2(ndim, n, ilow, iupp, h, wr, wi, vecs, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Finds eigenvalues and eigenvectors of an unsymmetric real matrix,  *
*   A which has been reduced to upper Hessenberg form, H, by the       *
*   subroutine ORTHES. The orthogonal transformations must be placed   *
*   in the array VECS by subroutine ORTRAN.                            *
*                                                                      *
*   Translation of the ALGOL procedure HQR2 in:                        *
*   Handbook Series Linear Algebra,                                    *
*   Num. Math. 16, 181 - 204 (1970) by G. Peters and J. H. Wilkinson.  *
* Input:                                                               *
*   N         (integer) Order of the Hessenberg matrix H.              *
*   ILOW,IUPP (integer)                                                *
*   H(NDIM,N) (real)    The Hessenberg matrix produced by ORTHES.      *
*   VECS(NDIM,N) (real) A square matrix of order N containing the      *
*                       similarity transformation from A to H          *
* Output:                                                              *
*   H(NDIM,N) (real)    Modified.                                      *
*   WR(N)     (real)    Real parts of eigenvalues of H (or A).         *
*   WI(N)     (real)    Imaginary parts of eigenvalues of H (or A).    *
*   VECS(NDIM,N) (real) The unnormalized eigenvectors of A.            *
*                       Complex vectors are stored as pairs of reals.  *
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
      integer i,ien,ierr,ilow,its,iupp,j,k,l,m,n,na,ndim
      double precision den,h,hnorm,p,q,r,ra,s,sa,t,temp,tempi,tempr,
     +vecs,vi,vr,w,wi,wr,x,y,z
      dimension         h(ndim,n), wr(n), wi(n), vecs(ndim,n)
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
 
      ierr = 0
 
*---- Store isolated roots.
      do 10 i = 1, n
        if (i .lt. ilow  .or.  i .gt. iupp) then
          wr(i) = h(i,i)
          wi(i) = 0.0
        endif
   10 continue
 
      ien = iupp
      t = 0.0
 
*---- Next eigenvalue.
   60 if (ien .ge. ilow) then
        its = 0
        na = ien - 1
 
*---- Next iteration; look for single small sub-diagonal element.
   70   continue
          do 80 l = ien, ilow + 1, -1
            if (abs(h(l,l-1)) .le.
     +          epsmch * (abs(h(l-1,l-1)) + abs(h(l,l)))) go to 100
   80     continue
          l = ilow
  100     continue
          x = h(ien,ien)
          if (l .eq. ien) go to 270
          y = h(na,na)
          w = h(ien,na) * h(na,ien)
          if (l .eq. na) go to 280
          if (its .eq. 30) then
            ierr = ien
            go to 9999
          endif
 
*---- Form exceptional shift.
          if (its .eq. 10  .or.  its .eq. 20) then
            t = t + x
            do 120 i = ilow, ien
              h(i,i) = h(i,i) - x
  120       continue
            s = abs(h(ien,na)) + abs(h(na,ien-2))
            x = 0.75 * s
            y = x
            w = - 0.4375 * s * s
          endif
          its = its + 1
 
*---- Look for two consecutive small sub-diagonal elements.
          do 140 m = ien - 2, l, - 1
            z = h(m,m)
            r = x - z
            s = y - z
            p = (r * s - w) / h(m+1,m) + h(m,m+1)
            q = h(m+1,m+1) - z - r - s
            r = h(m+2,m+1)
            s = abs(p) + abs(q) + abs(r)
            p = p / s
            q = q / s
            r = r / s
            if (m .eq. l) go to 150
            if (abs(h(m,m-1)) * (abs(q) + abs(r)) .le. epsmch * abs(p)
     x       * (abs(h(m-1,m-1)) + abs(z) + abs(h(m+1,m+1)))) go to 150
  140     continue
 
  150     continue
          h(m+2,m) = 0.0
          do 160 i = m + 3, ien
            h(i,i-2) = 0.0
            h(i,i-3) = 0.0
  160     continue
 
*---- Double QR step involving rows L to IEN and columns M to IEN.
          do 260 k = m, na
            if (k .ne. m) then
              p = h(k,k-1)
              q = h(k+1,k-1)
              if (k .ne. na) then
                r = h(k+2,k-1)
              else
                r = 0.0
              endif
              x = abs(p) + abs(q) + abs(r)
              if (x .eq. 0.0) go to 260
              p = p / x
              q = q / x
              r = r / x
            endif
            s = sign(sqrt(p**2+q**2+r**2),p)
            if (k .ne. m) then
              h(k,k-1) = - s * x
            else if (l .ne. m) then
              h(k,k-1) = - h(k,k-1)
            endif
            p = p + s
            x = p / s
            y = q / s
            z = r / s
            q = q / p
            r = r / p
 
*---- Row modification.
            do 210 j = k, n
              p = h(k,j) + q * h(k+1,j)
              if (k .ne. na) then
                p = p + r * h(k+2,j)
                h(k+2,j) = h(k+2,j) - p * z
              endif
              h(k+1,j) = h(k+1,j) - p * y
              h(k,j) = h(k,j) - p * x
  210       continue
 
*---- Column modification.
            j = min(ien,k+3)
            do 230 i = 1, j
              p = x * h(i,k) + y * h(i,k+1)
              if (k .ne. na) then
                p = p + z * h(i,k+2)
                h(i,k+2) = h(i,k+2) - p * r
              endif
              h(i,k+1) = h(i,k+1) - p * q
              h(i,k) = h(i,k) - p
  230       continue
 
*---- Accumulate transformations.
            do 250 i = ilow, iupp
              p = x * vecs(i,k) + y * vecs(i,k+1)
              if (k .ne. na) then
                p = p + z * vecs(i,k+2)
                vecs(i,k+2) = vecs(i,k+2) - p * r
              endif
              vecs(i,k+1) = vecs(i,k+1) - p * q
              vecs(i,k) = vecs(i,k) - p
  250       continue
  260     continue
 
*---- Go to next iteration.
        go to 70
 
*==== One real root found.
  270   h(ien,ien) = x + t
        wr(ien) = h(ien,ien)
        wi(ien) = 0.0
        ien = na
        go to 60
 
*==== Two roots (real pair or complex conjugate) found.
  280   p = (y - x) / 2.0
        q = p**2 + w
        z = sqrt(abs(q))
        x = x + t
        h(ien,ien) = x
        h(na,na) = y + t
 
*---- Real pair.
        if (q .gt. 0.0) then
          z = p + sign(z,p)
          wr(na) = x + z
          wr(ien) = x - w / z
          wi(na) = 0.0
          wi(ien) = 0.0
          x = h(ien,na)
          r = sqrt(x**2+z**2)
          p = x / r
          q = z / r
 
*---- Row modification.
          do 290 j = na, n
            z = h(na,j)
            h(na,j) = q * z + p * h(ien,j)
            h(ien,j) = q * h(ien,j) - p * z
  290     continue
 
*---- Column modification.
          do 300 i = 1, ien
            z = h(i,na)
            h(i,na) = q * z + p * h(i,ien)
            h(i,ien) = q * h(i,ien) - p * z
  300     continue
 
*---- Accumulate transformations.
          do 310 i = ilow, iupp
            z = vecs(i,na)
            vecs(i,na) = q * z + p * vecs(i,ien)
            vecs(i,ien) = q * vecs(i,ien) - p * z
  310     continue
 
*---- Complex pair.
        else
          wr(na) = x + p
          wr(ien) = x + p
          wi(na) = z
          wi(ien) = -z
        endif
 
*----- Go to next root.
        ien = ien - 2
        go to 60
      endif
 
*==== Compute matrix norm.
      hnorm = 0.0
      k = 1
      do 520 i = 1, n
        do 510 j = k, n
          hnorm = hnorm + abs(h(i,j))
  510   continue
        k = i
  520 continue
 
*==== Back substitution.
      do 690 ien = n, 1, -1
        p = wr(ien)
        q = wi(ien)
        na = ien - 1
 
*---- Real vector.
        if (q .eq. 0.0) then
          m = ien
          h(ien,ien) = 1.0
          do 640 i = na, 1, -1
            w = h(i,i) - p
            r = h(i,ien)
            do 610 j = m, na
              r = r + h(i,j) * h(j,ien)
  610       continue
            if (wi(i) .lt. 0.0) then
              z = w
              s = r
            else
              m = i
              if (wi(i) .eq. 0.0) then
                temp = w
                if (w .eq. 0.0) temp = epsmch * hnorm
                h(i,ien) = - r / temp
              else
                x = h(i,i+1)
                y = h(i+1,i)
                q = (wr(i) - p)**2 + wi(i)**2
                t = (x * s - z * r) / q
                h(i,ien) = t
                if (abs(x) .gt. abs(z)) then
                  h(i+1,ien) = - (r + w * t) / x
                else
                  h(i+1,ien) = - (s + y * t) / z
                endif
              endif
            endif
  640     continue
 
*---- Complex vector associated with lamda = P - i * Q.
        else if (q .lt. 0.0) then
          m = na
          if (abs(h(ien,na)) .gt. abs(h(na,ien))) then
            h(na,na) = - (h(ien,ien) - p) / h(ien,na)
            h(na,ien) = - q / h(ien,na)
          else
            den = (h(na,na) - p)**2 + q**2
            h(na,na) = - h(na,ien) * (h(na,na) - p) / den
            h(na,ien) = h(na,ien) * q / den
          endif
          h(ien,na) = 1.0
          h(ien,ien) = 0.0
          do 680 i = ien - 2, 1, - 1
            w = h(i,i) - p
            ra = h(i,ien)
            sa = 0.0
            do 660 j = m, na
              ra = ra + h(i,j) * h(j,na)
              sa = sa + h(i,j) * h(j,ien)
  660       continue
            if (wi(i) .lt. 0.0) then
              z = w
              r = ra
              s = sa
            else
              m = i
              if (wi(i) .eq. 0.0) then
                den = w**2 + q**2
                h(i,na) = - (ra * w + sa * q) / den
                h(i,ien) = (ra * q - sa * w) / den
              else
                x = h(i,i+1)
                y = h(i+1,i)
                vr = (wr(i) - p)**2 + wi(i)**2 - q**2
                vi = 2.0 * (wr(i) - p) * q
                if (vr .eq. 0.0  .and.  vi .eq. 0.0) then
                  vr = epsmch * hnorm
     +               * (abs(w) + abs(q) + abs(x) + abs(y) + abs(z))
                endif
                tempr = x * r - z * ra + q * sa
                tempi = x * s - z * sa - q * ra
                den = vr**2 + vi**2
                h(i,na) = (tempr * vr + tempi * vi) / den
                h(i,ien) = (tempi * vr - tempr * vi) / den
                if (abs(x) .gt. abs(z) + abs(q)) then
                  h(i+1,na) = (- ra - w * h(i,na) + q * h(i,ien)) / x
                  h(i+1,ien) = (- sa - w * h(i,ien) - q * h(i,na)) / x
                else
                  tempr = - r - y * h(i,na)
                  tempi = - s - y * h(i,ien)
                  den = z**2 + q**2
                  h(i+1,na) = (tempr * z + tempi * q) / den
                  h(i+1,ien) = (tempi * z - tempr * q) / den
                endif
              endif
            endif
  680     continue
        endif
  690 continue
 
*==== Vectors of isolated roots.
      do 720 i = 1, n
        if (i .lt. ilow  .or.  i .gt. iupp) then
          do 710 j = i, n
            vecs(i,j) = h(i,j)
  710     continue
        endif
  720 continue
 
*==== Multiply by transformation matrix to give eigenvectors of the
*     original full matrix.
      do 790 j = n, ilow, - 1
        m = min(j,iupp)
        if (wi(j) .lt. 0.0) then
          l = j - 1
          do 740 i = ilow, iupp
            y = 0.0
            z = 0.0
            do 730 k = ilow, m
              y = y + vecs(i,k) * h(k,l)
              z = z + vecs(i,k) * h(k,j)
  730       continue
            vecs(i,l) = y
            vecs(i,j) = z
  740     continue
        else if (wi(j) .eq. 0.0) then
          do 760 i = ilow, iupp
            z = 0.0
            do 750 k = ilow, m
              z = z + vecs(i,k) * h(k,j)
  750       continue
            vecs(i,j) = z
  760     continue
        endif
  790 continue
 
 9999 end
