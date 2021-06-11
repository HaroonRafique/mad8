      subroutine lmcanx(nord, gp, df, ndim)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Establish standard rep of transfer map for ray traces.             *
* Input:                                                               *
*   NORD      (integer) Order of the map.                              *
*   GP(*)     (poly)    Lie-algebraic representation of non-linearity. *
* Output:                                                              *
*   DF(*,6)   (poly)    Derivatives of generating function.            *
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
      integer i,iaux,ifp,iqind,isave,itrm,jord,jqind,kqind,ndim,nord
      double precision by2,by24,by6,df,gp,three
      dimension         gp(*), df(ndim,6)
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      parameter         (three = 3.0d0)
      parameter         (by2   = 1.0d0 / 2.0d0)
      parameter         (by6   = 1.0d0 / 6.0d0)
      parameter         (by24  = 1.0d0 / 24.0d0)
 
*---- Allocate working space.
      isave = iwork
      iaux  = iwork
      ifp   = iaux + itop6(min(nord,4))
      itrm  = ifp  + itop6(nord)
      iwork = itrm + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Orders 1 and 2 (identity transformation).
      do 10 i = 1, 27
        dq(ifp+i) = 0.0
   10 continue
      dq(ifp+8)  = 1.0
      dq(ifp+19) = 1.0
      dq(ifp+26) = 1.0
 
*---- Copy generator terms.
      do 20 i = 28, itop6(nord)
        dq(ifp+i) = - gp(i)
   20 continue
 
*---- Order 4 terms.
      if (nord .ge. 4) then
 
*---- Derivatives of array GP: DF(*,i) = dGP/dZi.
        do 30 i = 1, 6
        do 30 jord = 3, nord-1
          call pa6dif(gp, i, jord, df(1,i))
   30   continue
        call pa6clr(dq(itrm+1), 4)
        do 190 iqind = 1, 5, 2
          call pa6prd(df(1,iqind), 2, df(1,iqind+1), 2, dq(itrm+1))
  190   continue
        call pa6sum(by2, dq(itrm+1), 4, dq(ifp+1))
      endif
 
*---- Order 5 terms.
      if(nord .ge. 5) then
 
*---- Terms g4 x g3.
        do 210 iqind = 1, 5, 2
          call pa6prd(df(1,iqind), 3, df(1,iqind+1), 2, dq(ifp+1))
  210   continue
 
*---- Terms g3 x g3 x g3.
        call pa6clr(dq(itrm+1), 5)
        do 290 iqind = 1, 5, 2
 
*---- Simple sum over g3 x t4.
          call pa6dif(dq(itrm+1), iqind+1, 4, dq(iaux+1))
          call pa6prd(df(1,iqind), 2, dq(iaux+1), 3, dq(itrm+1))
 
*---- Double sum over g3 x g3 x g3.
          call pa6clr(dq(iaux+1), 3)
          do 280 jqind = 1, 5, 2
            call pa6dif(df(1,iqind), jqind, 2, dq(iaux+1))
            call pa6prd(dq(iaux+1), 1, df(1,jqind+1), 2, dq(iaux+1))
  280     continue
          call pa6prd(dq(iaux+1), 3, df(1,iqind+1), 2, dq(itrm+1))
  290   continue
 
*---- Add in t(5) term.
        call pa6sum(- by6, dq(itrm+1), 5, dq(ifp+1))
      endif
 
*---- Order 6 terms.
      if (nord .ge. 6) then
 
*---- Term g5 x g3.
        do 310 iqind = 1, 5, 2
          call pa6prd(df(1,iqind), 4, df(1,iqind+1), 2, dq(ifp+1))
  310   continue
 
*---- Terms g4 x g4.
        call pa6clr(dq(itrm+1), 6)
        do 320 iqind = 1, 5, 2
          call pa6prd(df(1,iqind), 3, df(1,iqind+1), 3, dq(itrm+1))
  320   continue
        call pa6sum(by2, dq(itrm+1), 6, dq(ifp+1))
 
*---- Terms g4 x g3 x g3.
        call pa6clr(dq(itrm+1), 6)
        do 340 iqind = 1, 5, 2
 
*---- Simple sum over g4 x t4.
          call pa6dif(dq(itrm+1), iqind+1, 4, dq(iaux+1))
          call pa6prd(df(1,iqind), 3, dq(iaux+1), 3, dq(itrm+1))
 
*---- Double sum over g4 x g3 x g3.
          call pa6clr(dq(iaux+1), 4)
          do 330 jqind = 1, 5, 2
            call pa6dif(df(1,iqind), jqind, 3, dq(iaux+1))
            call pa6prd(dq(iaux+1), 2, df(1,jqind+1), 2, dq(iaux+1))
  330     continue
          call pa6prd(dq(iaux+1), 4, df(1,iqind+1), 2, dq(itrm+1))
  340   continue
 
*---- Add in u(6) term.
        call pa6sum(- by2, dq(itrm+1), 6, dq(ifp+1))
 
*---- Terms g3 x g3 x g3 x g3.
        call pa6clr(dq(itrm+1), 6)
        do 390 iqind = 1, 5, 2
 
*---- Simple sum over g3 x t5.
          call pa6dif(dq(itrm+1), iqind+1, 5, dq(iaux+1))
          call pa6prd(df(1,iqind), 2, dq(iaux+1), 4, dq(itrm+1))
 
*---- Double sum over g3 x g3 x t4.
          call pa6clr(dq(iaux+1), 4)
          do 380 jqind = 1, 5, 2
            call pa6dif(df(1,iqind), jqind, 2, dq(iaux+1))
 
*---- Triple sum over g3 x g3 x g3 x g3.
            call pa6clr(dq(iaux+1), 2)
            do 370 kqind = 1, 5, 2
              call pa6sum(dq(iaux+kqind), df(1,kqind+1), 2, dq(iaux+1))
  370       continue
            call pa6prd(dq(iaux+1), 2, df(1,jqind+1), 2, dq(iaux+1))
 
*---- Finish double sum (including factor 3).
            call pa6scl(three, dq(iaux+1), 1, dq(iaux+1))
            call pa6dif(dq(itrm+1), jqind+1, 4, dq(iaux+1))
            call pa6prd(dq(iaux+1), 1, dq(iaux+1), 3, dq(iaux+1))
  380     continue
          call pa6prd(dq(iaux+1), 4, df(1,iqind+1), 2, dq(itrm+1))
  390   continue
 
*---- Add in t6 term.
        call pa6sum(by24, dq(itrm+1), 6, dq(ifp+1))
      endif
 
*---- Compute the derivatives of the generating function F
*     (standard representation of the canonical transformation).
      do 400 i = 1, 6
        call pa6dif(dq(ifp+1), i, -nord, df(1,i))
  400 continue
 
*---- Drop working storage.
      iwork = isave
 
      end
