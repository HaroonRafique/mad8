      subroutine lmfixp(fp, fm, ap, am, tp, tm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find the fixed point of a map.                                     *
* Source:     MARYLIE, version 3.0 (routine FXPT).                     *
* Author:     Alex Dragt, October 1985.                                *
* Input:                                                               *
*   FP, FM    (map)     Initial map (unchanged by subroutine).         *
* Output:                                                              *
*   AP, AM    (map)     Map about fixed point.                         *
*   TP, TM    (map)     Transformation to the fixed point.             *
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
      integer i,irank,isave,ittp,j,nord
      double precision am,ap,bmat,fm,fp,tm,tmpm,tp,ttm
      dimension         fp(*), fm(6,6), ap(*), am(6,6), tp(*), tm(6,6)
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
 
      parameter         (nord = 4)
 
      dimension         bmat(4,5), tmpm(6,6), ttm(6,6)
 
*---- Allocate working space.
      isave = iwork
      ittp  = iwork
      iwork = ittp + 209
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Compute AN1.
*     Set up BMAT = 4x4 block of (FM - I), and BVEC = FM(I,6).
      do 20 i = 1, 4
        do 10 j = 1, 4
          bmat(i,j) = fm(i,j)
   10   continue
        bmat(i,i) = bmat(i,i) - 1.0
        bmat(i,5) = fm(i,6)
   20 continue
 
*---- Compute ALPHV.
      call solver(bmat, 4, 1, irank)
 
*---- Compute T1.
      call lmone(nord, tp, tm)
      do 30 i = 1, 4
        tm(i,6) = - bmat(i,5)
   30 continue
      tm(5,1) =   bmat(2,5)
      tm(5,2) = - bmat(1,5)
      tm(5,3) =   bmat(4,5)
      tm(5,4) = - bmat(3,5)
 
*---- Store T1.
      call lmcopy(nord, tp, tm, dq(ittp+1), ttm)
 
*---- Compute T1*M*T1INV where M is initial map.
      call lmsand(nord, tp, tm, fp, fm, ap, am)
 
*---- Compute AN2.
*     Set up BMAT.
      call m66inv(am, tmpm)
      do 50 i = 1, 4
        do 40 j = 1, 4
          bmat(i,j) = tmpm(j,i)
   40   continue
        bmat(i,i) = bmat(i,i) - 1.0
   50 continue
 
*---- Set up BVEC.
      bmat(1,5) = - ap(48)
      bmat(2,5) = - ap(63)
      bmat(3,5) = - ap(73)
      bmat(4,5) = - ap(79)
 
*---- Compute ALPHV.
      call solver(bmat, 4, 1, irank)
 
*---- Compute T2.
      call lmone(nord, tp, tm)
      tp(48) = bmat(1,5)
      tp(63) = bmat(2,5)
      tp(73) = bmat(3,5)
      tp(79) = bmat(4,5)
 
*---- Compute and store T2*T1
      call lmcat(nord, tp, tm, dq(ittp+1), ttm, dq(ittp+1), ttm)
 
*---- Compute T2*AN1*T2INV.
      call lmsand(nord, tp, tm, ap, am, ap, am)
 
*---- Compute AN3.
*     Set up BMAT.
      do 70 i = 1, 4
        do 60 j = 1, 4
          bmat(i,j) = tmpm(j,i)
   60   continue
        bmat(i,i) = bmat(i,i) - 1.0
   70 continue
 
*---- Set up BVEC.
      bmat(1,5) = - ap(139)
      bmat(2,5) = - ap(174)
      bmat(3,5) = - ap(194)
      bmat(4,5) = - ap(204)
 
*---- Compute ALPHV.
      call solver(bmat, 4, 1, irank)
 
*---- Compute T3.
      call lmone(nord, tp, tm)
      tp(139) = bmat(1,5)
      tp(174) = bmat(2,5)
      tp(194) = bmat(3,5)
      tp(204) = bmat(4,5)
 
*---- Compute and store T3*T2*T1.
      call lmcat(nord, tp, tm, dq(ittp+1), ttm, dq(ittp+1), ttm)
 
*---- Compute T3*AN2*T3INV.
      call lmsand(nord, tp, tm, ap, am, ap, am)
 
*---- Store T3*T2*T1 in T.
      call lmcopy(nord, dq(ittp+1), ttm, tp, tm)
 
*---- Drop working storage.
      iwork = isave
 
      end
