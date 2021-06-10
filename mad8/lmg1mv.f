      subroutine lmg1mv(nord, fp, fm, gp, hp, hm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Move the exponential of a first order polynomial to the left:      *
*         exp(:F1:)exp(:F2:)exp(:F3:)exp(:F4:)exp(:G1:)                *
*       = exp(:H1:)exp(:H2:)exp(:H3:)exp(:H4:).                        *
* Source:     MARYLIE, version 5.1 (routine G1MOVE).                   *
* Author:     Liam Healy.                                              *
* Input:                                                               *
*   NORD      (integer) The order of the maps involved.                *
*   FP, FM    (map)     The map F.                                     *
*   GP        (poly)    The polynomials representing the map G.        *
* Output:                                                              *
*   HP, HM    (map)     The map H defined above.                       *
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
      integer ifj,ifk,ipb2,ipb3,ipb4,isave,itm,j,k,nord
      double precision by12,by2,by24,by6,by8,fm,fp,gneg,gp,gt,hm,hp,one
      dimension         fp(*), fm(6,6), gp(*), hp(*), hm(6,6)
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
      parameter         (by2 = 1.0d0 / 2.0d0)
      parameter         (by6 = 1.0d0 / 6.0d0)
      parameter         (by8 = 1.0d0 / 8.0d0)
      parameter         (by12 = 1.0d0 / 12.0d0)
      parameter         (by24 = 1.0d0 / 24.0d0)
 
      dimension         gneg(6), gt(6)
      logical           eflag
 
*---- If there is no G1, copy "F" map and return.
      do 10 j = 1, 6
        if (gp(j) .ne. 0.0) go to 100
   10 continue
      call pa6cpy(fp, - nord, hp)
      call m66cpy(fm, hm)
      return
 
*---- Linear map?
  100 continue
      if (nord .le. 2) then
        call pa6xfm(gp, 1, fm, gt)
        call pa6add(fp, gt, 1, hp)
        call pa6clr(hp, 2)
        call m66cpy(fm, hm)
        return
      endif
 
*---- Allocate working space.
      isave = iwork
      itm   = iwork
      ifj   = itm  + 36
      ifk   = ifj  + itop6(nord)
      ipb2  = ifk  + itop6(nord)
      ipb3  = ipb2 + itop6(nord)
      ipb4  = ipb3 + itop6(nord)
      iwork = ipb4 + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Clear result array and add G1.
      call pa6clr(hp, -nord)
      call pa6add(hp, gp, 1, hp)
 
*---- Find GNEG = - G1.
      call pa6scl(- one, gp, 1, gneg)
 
*---- Switch to suppress higher orders, if not wanted.
*     Note: NORD .LE. 2 will not come here.
      go to (200, 200, 200, 130, 120, 110), nord
 
*---- Terms arising from F6.
  110 continue
        call lmexpo(gneg, 1, fp, 6, dq(ifj+1), 6)
        call pa6add(hp, dq(ifj+1), - 6, hp)
 
*---- Terms arising from F5.
  120 continue
        call lmexpo(gneg, 1, fp, 5, dq(ifj+1), 5)
        call pa6add(hp, dq(ifj+1), - 5, hp)
 
*---- Terms arising from F4.
  130 continue
        call lmexpo(gneg, 1, fp, 4, dq(ifj+1), 4)
        call pa6add(hp, dq(ifj+1), - 4, hp)
        if (nord .ge. 6) then
          do 140 k = 1, 3
          do 140 j = k+1, 4
            call pa6brk(dq(ifj+1), j, dq(ifj+1), k, dq(ipb2+1))
            call pa6sum(by2, dq(ipb2+1), j+k-2, hp)
  140     continue
        endif
  200 continue
 
*---- Move H1 across F3.
      call pa6scl(- one, hp, 1, gneg)
      call lmexpo(gneg, 1, fp, 3, dq(ifj+1), 3)
 
*---- Factorize F(J), result is F(K).
      call pa6cpy(dq(ifj+1), - 3, dq(ifk+1))
      if (nord .ge. 4) then
        call pa6clr(dq(ifk+1), 4)
        call pa6brk(dq(ifj+1), 2, dq(ifj+1), 1, dq(ipb2+1))
        call pa6brk(dq(ifj+1), 3, dq(ifj+1), 1, dq(ipb2+1))
        call pa6brk(dq(ifj+1), 3, dq(ifj+1), 2, dq(ipb2+1))
        call pa6sum(by2, dq(ipb2+1), - 3, dq(ifk+1))
        if (nord .ge. 5) then
          call pa6clr(dq(ifk+1), 5)
          call pa6brk(dq(ifj+1), 2, dq(ipb2+1), 3, dq(ipb3+1))
          call pa6sum(- by6, dq(ipb3+1), 3, dq(ifk+1))
          call pa6brk(dq(ifj+1), 3, dq(ipb2+1), 3, dq(ipb3+1))
          call pa6sum(- by12, dq(ipb3+1), 4, dq(ifk+1))
          if (nord .ge. 6) then
            call pa6brk(dq(ifj+1), 2, dq(ipb3+1), 3, dq(ipb4+1))
            call pa6sum(by24, dq(ipb4+1), 3, dq(ifk+1))
            call pa6brk(dq(ifj+1), 3, dq(ipb3+1), 3, dq(ipb4+1))
            call pa6sum(by24, dq(ipb4+1), 4, dq(ifk+1))
            call pa6brk(dq(ifj+1), 3, dq(ipb3+1), 4, dq(ipb4+1))
            call pa6sum(by24, dq(ipb4+1), 5, dq(ifk+1))
          endif
          call pa6brk(dq(ifj+1), 1, dq(ipb2+1), 2, dq(ipb3+1))
          call pa6sum(- by6, dq(ipb3+1), 1, dq(ifk+1))
          call pa6brk(dq(ifj+1), 2, dq(ipb2+1), 2, dq(ipb3+1))
          call pa6sum(- by12, dq(ipb3+1), 2, dq(ifk+1))
          call pa6brk(dq(ifj+1), 3, dq(ipb2+1), 2, dq(ipb3+1))
          call pa6sum(by6, dq(ipb3+1), 3, dq(ifk+1))
          if (nord .ge. 6) then
            call pa6brk(dq(ifj+1), 2, dq(ipb3+1), 1, dq(ipb4+1))
            call pa6brk(dq(ifj+1), 3, dq(ipb3+1), 1, dq(ipb4+1))
            call pa6sum(- by24, dq(ipb4+1), - 2, dq(ifk+1))
            call pa6brk(dq(ifj+1), 3, dq(ipb3+1), 2, dq(ipb4+1))
            call pa6sum(by24, dq(ipb4+1), 3, dq(ifk+1))
            call pa6brk(dq(ifj+1), 2, dq(ipb3+1), 3, dq(ipb4+1))
            call pa6sum(- by8, dq(ipb4+1), 3, dq(ifk+1))
            call pa6brk(dq(ifj+1), 3, dq(ipb3+1), 3, dq(ipb4+1))
            call pa6sum(- by24, dq(ipb4+1), 4, dq(ifk+1))
          endif
          call pa6brk(dq(ifj+1), 2, dq(ipb2+1), 1, dq(ipb3+1))
          call pa6brk(dq(ifj+1), 3, dq(ipb2+1), 1, dq(ipb3+1))
          call pa6sum(by6, dq(ipb3+1), - 2, dq(ifk+1))
          if (nord .ge. 6) then
            call pa6brk(dq(ifj+1), 2, dq(ipb3+1), 1, dq(ipb4+1))
            call pa6brk(dq(ifj+1), 3, dq(ipb3+1), 1, dq(ipb4+1))
            call pa6sum(by24, dq(ipb4+1), - 2, dq(ifk+1))
            call pa6brk(dq(ifj+1), 1, dq(ipb3+1), 2, dq(ipb4+1))
            call pa6sum(- by8, dq(ipb4+1), 1, dq(ifk+1))
            call pa6brk(dq(ifj+1), 2, dq(ipb3+1), 2, dq(ipb4+1))
            call pa6sum(- by24, dq(ipb4+1), 2, dq(ifk+1))
            call pa6brk(dq(ifj+1), 3, dq(ipb3+1), 2, dq(ipb4+1))
            call pa6sum(by24, dq(ipb4+1), 3, dq(ifk+1))
          endif
        endif
      endif
 
*---- Concatenate DQ(IFK+1) with H = F4...F6.
      if (nord .ge. 5) then
        call pa6brk(dq(ifk+1), 2, hp, 2, dq(ipb2+1))
        call pa6sum(by2, dq(ipb2+1), 2, hp)
        call pa6brk(dq(ifk+1), 3, hp, 2, dq(ipb2+1))
        call pa6sum(one, dq(ipb2+1), 3, hp)
        call pa6brk(dq(ifk+1), 3, hp, 3, dq(ipb2+1))
        call pa6sum(by2, dq(ipb2+1), 4, hp)
        if (nord .ge. 6) then
          call pa6brk(dq(ifk+1), 2, dq(ipb2+1), 2, dq(ipb3+1))
          call pa6sum(by12, dq(ipb3+1), 2, hp)
          call pa6brk(dq(ifk+1), 3, dq(ipb2+1), 4, dq(ipb3+1))
          call pa6sum(- by6, dq(ipb3+1), 5, hp)
        endif
      endif
      call pa6add(hp, dq(ifk+1), - min(nord,5), hp)
 
*---- Move H1 across F2 and combine with F1.
      call pa6xfm(hp, 1, fm, gt)
      call pa6add(fp, gt, 1, hp)
 
*---- Calculate the matrix part of the factored exponential.
      call m66mak(hp, hm)
      call m66exp(hm, dq(itm+1), eflag)
      call m66mpy(dq(itm+1), fm, hm)
 
*---- Drop working storage.
      iwork = isave
 
      end
