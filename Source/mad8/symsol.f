      subroutine symsol(a, n, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Invert symmetric matrix.                                           *
* Input:                                                               *
*   A(*,*)    (real)    Matrix to be inverted.                         *
*   N         (integer) Actual size of A.                              *
* Output:                                                              *
*   A(*,*)    (real)    Inverted matrix.                               *
*   EFLAG     (logical) Error flag.                                    *
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
      integer i,ip,is,iw,j,k,n
      double precision a,si
      dimension         a(n,n)
      logical           eflag
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
 
*---- Allocate working space.
      is = iwork
      ip = is + n
      iw = ip + n
      iwork = iw + n
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Scale upper triangle.
      eflag = .true.
      do 10 i = 1, n
        si = a(i,i)
        if (si .le. 0.0) go to 100
        dq(is+i) = 1.0 / sqrt(si)
   10 continue
      do 20 i = 1, n
      do 20 j = i, n
        a(i,j) = a(i,j) * dq(is+i) * dq(is+j)
   20 continue
 
*---- Invert upper triangle.
      do 50 i = 1, n
        if (a(i,i) .eq. 0.0) go to 100
        dq(ip+i) = 1.0
        dq(iw+i) = 1.0 / a(i,i)
        a(i,i) = 0.0
        do 30 j = 1, n
          if (j .lt. i) then
            dq(ip+j) = a(j,i)
            dq(iw+j) = dq(ip+j) * dq(iw+i)
            a(j,i) = 0.0
          else if (j .gt. i) then
            dq(ip+j) = a(i,j)
            dq(iw+j) = - dq(ip+j) * dq(iw+i)
            a(i,j) = 0.0
          endif
   30   continue
        do 40 j = 1, n
        do 40 k = j, n
          a(j,k) = a(j,k) + dq(ip+j) * dq(iw+k)
   40   continue
   50 continue
 
*---- Rescale upper triangle and symmetrize.
      do 60 i = 1, n
      do 60 j = i, n
        a(i,j) = a(i,j) * dq(is+i) * dq(is+j)
        a(j,i) = a(i,j)
   60 continue
      eflag = .false.
 
*---- Release working space.
  100 continue
      iwork = is
 
      end
