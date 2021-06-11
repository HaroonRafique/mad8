      subroutine solver(augmat, ndim, mdim, irank)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Solve the linear equation  A * X = B.                              *
* Input:                                                               *
*   AUGMAT(n,n+m)       A(n,n), augmented by B(n,m).                   *
*   NDIM, MDIM          n, m.                                          *
* Output:                                                              *
*   AUGMAT(n,n+m)       Identity(n,n), augmented by X(n,m).            *
*   IRANK               Rank of A.                                     *
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
      integer ic,ip,ir,irank,it,mdim,nc,ndim,nr
      double precision augmat,h,pivot
      dimension         augmat(ndim,ndim+mdim)
 
      nr = ndim
      nc = ndim + mdim
 
      do 100 it = 1, nr
        pivot = 0.
        ip = 0
        do 10 ir = it, nr
          if (abs(augmat(ir,it)) .ge. abs(pivot)) then
            pivot = augmat(ir,it)
            ip = ir
          endif
   10   continue
 
        if (pivot .eq. 0.0) go to 9999
        irank = it
 
        do 30 ic = 1, nc
          augmat(ip,ic) = augmat(ip,ic) / pivot
   30   continue
 
        if (ip .ne. it) then
          do 50 ic = 1, nc
            h = augmat(ip,ic)
            augmat(ip,ic) = augmat(it,ic)
            augmat(it,ic) = h
   50     continue
        endif
 
        do 70 ir = 1, nr
          if (ir .ne. it) then
            h = augmat(ir,it)
            do 60 ic = 1, nc
              augmat(ir,ic) = augmat(ir,ic) - h * augmat(it,ic)
   60       continue
          endif
   70   continue
  100 continue
 
      irank = ndim
 
 9999 end
