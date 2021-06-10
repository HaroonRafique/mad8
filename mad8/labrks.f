      subroutine labrks(nord, hp, pbh, ndim)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute Poisson brackets of transfer map generator with vector Z.  *
* Input:                                                               *
*   H(*)      (poly)    Transfer map generator of order NORD.          *
* Output:                                                              *
*   PBH(NDIM,6)         Resulting Poisson brackets.                    *
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
      integer isave,itemp,j,jord,jqind,ndim,nord
      double precision hp,one,pbh
      dimension         hp(*), pbh(ndim,6)
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
 
      parameter         (one = 1.0d0)
 
*---- Make identity operation for all six variables.
      isave = iwork
      do 10 j = 1, 6
        call pa6clr(pbh(1,j), -nord)
        pbh(j,j) = 1.0
   10 continue
 
*---- Short-cut for order 3.
      if (nord .eq. 3) then
        do 20 jqind = 1, 5, 2
          call pa6dif (hp, jqind+1, 3, pbh(1,jqind))
          call pa6scl(-one, pbh(1,jqind), 2, pbh(1,jqind))
          call pa6dif (hp, jqind, 3, pbh(1,jqind+1))
   20   continue
 
*---- For NORD > 3, build Poisson brackets in reverse order.
      else if (nord .gt. 3) then
 
*---- Allocate working space.
        itemp = iwork
        iwork = itemp + itop6(nord-1)
        if (iwork .gt. nwork) then
          call mzwork(0, dq(1), dq(iwork+1), 2)
          nwork = iwork
        endif
 
*---- Loop for all six dynamic variables.
        do 90 j = 1, 6
          do 80 jord = nord, 3, -1
            call lmexpo(hp, jord, pbh(1,j), -nord, dq(itemp+1), nord-1)
            call pa6cpy(dq(itemp+1), -nord, pbh(1,j))
   80     continue
   90   continue
      endif
 
*---- Drop working storage.
      iwork = isave
 
      end
