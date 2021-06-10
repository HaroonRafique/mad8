      subroutine laseig(fm, reeig, aieig, am)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return eigenvalues and eigenvectors of a 4x4 matrix.               *
* Input:                                                               *
*   FM(6,6)   (real)    Matrix to be transformed.                      *
* Output:                                                              *
*   REEIG(6)  (real)    Real parts of eigenvalues.                     *
*   AIEIG(6)  (real)    Imaginary parts of eigenvalues.                *
*   AM(6,6)   (real)    Transforming matrix, contains eigenvectors.    *
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
      integer i,ihi,ilo,info,ipind,iqind,j,k,mdim,nn
      double precision aieig,aival,am,big,c,d,dx,dy,fm,pb,reeig,reval,s,
     +tm
      dimension         fm(6,6), reeig(6), aieig(6), am(6,6)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
 
      parameter         (ilo = 1, ihi = 4, mdim = 6, nn = 4)
      dimension         d(6), reval(6), aival(6), tm(6,6)
      integer           kpnt(6)
 
*---- Compute eigenvalues and vectors.
      call m66cpy(fm, tm)
      call m66one(am)
      call orthes(mdim, nn, ilo, ihi, tm, d)
      call ortran(mdim,  nn, ilo, ihi, tm, d, am)
      call hqr2(mdim, nn, ilo, ihi, tm, reval, aival, am, info)
      if (info .ne. 0) then
        write (msg, 910) ((fm(i,k), k = 1, 6), i = 1, 6)
  910   format('Unable to find eigenvalues for matrix:'/
     +         (6f12.6))
        call aafail('LADEIG', 7, msg)
        go to 999
      endif
 
*---- Normalize the eigenvectors.
      do 30 k = 1, 5, 2
        pb = 0.0
        do 10 ipind = 2, 6, 2
          iqind = ipind - 1
          pb = pb + am(iqind,k) * am(ipind,k+1)
     +            - am(ipind,k) * am(iqind,k+1)
   10   continue
        s = sqrt(abs(pb))
        if (pb .lt. 0.0) then
          aival(k) = - aival(k)
          aival(k+1) = - aival(k+1)
        endif
        do 20 i = 1, 6
          am(i,k)   = am(i,k) / s
          am(i,k+1) = am(i,k+1) * (s / pb)
   20   continue
   30 continue
 
*---- Sort these eigenvectors.
      call m66cpy(am, tm)
 
*---- Find the eigenvectors with the largest vertical component.
      big = 0.0
      kpnt(3) = 1
      do 40 i = 1, 3, 2
        c = tm(3,i)**2 + tm(3,i+1)**2 + tm(4,i)**2 + tm(4,i+1)**2
        if (c .gt. big) then
          big = c
          kpnt(3) = i
        endif
  40  continue
 
*---- Find the remaining vector.
      do 50 i = 1, 3, 2
        if (i .ne. kpnt(3)) kpnt(1) = i
   50 continue
 
*---- Reorder vectors.
      do 70 i = 1, 3, 2
        k = kpnt(i)
        reeig(i) = reval(k)
        aieig(i) = aival(k)
        reeig(i+1) = reval(k+1)
        aieig(i+1) = aival(k+1)
        do 60 j = 1, 6
          am(j,i) = tm(j,k)
          am(j,i+1) = tm(j,k+1)
   60   continue
   70 continue
      reeig(5) = 1.0
      aieig(5) = 0.0
      reeig(6) = 1.0
      aieig(6) = 0.0
 
*---- Rephase the result.
      call m66one(tm)
      dx = sqrt(am(1,1)**2 + am(1,2)**2)
      tm(1,1) = am(1,1) / dx
      tm(2,1) = am(1,2) / dx
      tm(1,2) = - tm(2,1)
      tm(2,2) = tm(1,1)
      dy = sqrt(am(3,3)**2 + am(3,4)**2)
      tm(3,3) = am(3,3) / dy
      tm(4,3) = am(3,4) / dy
      tm(3,4) = - tm(4,3)
      tm(4,4) = tm(3,3)
      call m66mpy(am, tm, am)
 
  999 end
