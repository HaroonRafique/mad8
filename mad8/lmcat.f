      subroutine lmcat(nord, fp, fm, gp, gm, hp, hm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Concatenate two Lie-algebraic maps.                                *
* Source:     MARYLIE, version 5.1 (routine CONCAT).                   *
* Author:     Liam Healy.                                              *
* Input:                                                               *
*   FP, FM    (map)     First map (in beam order).                     *
*   GP, GM    (map)     Second map (in beam order).                    *
* Output:                                                              *
*   HP, HM    (map)     Concatenated map.                              *
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
      integer if1m,if1p,if3g3,ifxfm,ig3f,ig3f3,iginv,iresm,iresp,isave,
     +itemp,jord,nord
      double precision fm,four,fp,gm,gp,hm,hp,one,three,two
      dimension         fp(*), fm(6,6), gp(*), gm(6,6), hp(*), hm(6,6)
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
 
      parameter         (one    = 1.0d0)
      parameter         (two    = 2.0d0)
      parameter         (three  = 3.0d0)
      parameter         (four   = 4.0d0)
 
*---- Allocate working space.
      isave = iwork
      if1m  = iwork
      iginv = if1m
      iresm = if1m  + 36
      if1p  = iresm + 36
      ig3f  = if1p
      ifxfm = if1p  + itop6(nord)
      itemp = ifxfm + itop6(nord)
      if3g3 = itemp + itop6(nord)
      ig3f3 = if3g3 + itop6(nord)
      iresp = ig3f3 + itop6(nord)
      iwork = iresp + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Move G1 to the left across the F's.
      call lmg1mv(nord, fp, fm, gp, dq(if1p+1), dq(if1m+1))
      call pa6cpy(dq(if1p+1), 1, dq(iresp+1))
 
*---- Compute DQ(IRESM+1) = GM * F1M.
      call m66mpy(gm, dq(if1m+1), dq(iresm+1))
      call pa6clr(dq(iresp+1), 2)
 
*---- Nonlinear map?
      if (nord .ge. 3) then
 
*---- Inverse of GM.
        call m66inv(gm, dq(iginv+1))
 
*---- Compute transformed arrays and sum of FXFM and G.
        do 10 jord = 3, nord
          call pa6xfm(dq(if1p+1), jord, dq(iginv+1), dq(ifxfm+1))
          call pa6add(dq(ifxfm+1), gp, jord, dq(iresp+1))
   10   continue
        if (nord .ge. 4) then
          call lmexpo(dq(ifxfm+1), 3, gp, 3, dq(if3g3+1), nord)
          call pa6sum(one/two, dq(if3g3+1), 4, dq(iresp+1))
          if (nord .ge. 5) then
            call lmexpo(gp, 3, dq(ifxfm+1), 3, dq(ig3f3+1), nord)
            call pa6brk(gp, 3, dq(ifxfm+1), 4, dq(ig3f+1))
            call pa6sum(- one, dq(ig3f+1), 5, dq(iresp+1))
            call pa6sum(- one/three, dq(if3g3+1), 5, dq(iresp+1))
            call pa6sum(two/three, dq(ig3f3+1), 5, dq(iresp+1))
            if (nord .ge. 6) then
              call pa6brk(gp, 3, dq(ifxfm+1), 5, dq(ig3f+1))
              call pa6sum(- one, dq(ig3f+1), 6, dq(iresp+1))
              call pa6brk(gp, 3, dq(ig3f+1), 5, dq(itemp+1))
              call pa6sum(one/two, dq(itemp+1), 6, dq(iresp+1))
              call pa6brk(dq(ifxfm+1), 4, gp, 4, dq(itemp+1))
              call pa6sum(one/two, dq(itemp+1), 6, dq(iresp+1))
              call pa6add(dq(ifxfm+1), gp, 4, dq(itemp+1))
              call pa6brk(dq(itemp+1),4, dq(if3g3+1),4, dq(itemp+1))
              call pa6sum(- one/four, dq(itemp+1), 6, dq(iresp+1))
              call pa6sum(one/four, dq(if3g3+1), 6, dq(iresp+1))
              call pa6sum(- three/four, dq(ig3f3+1), 6, dq(iresp+1))
              call pa6brk(dq(ifxfm+1),3, dq(ig3f3+1),5, dq(itemp+1))
              call pa6sum(- one/four, dq(itemp+1), 6, dq(iresp+1))
            endif
          endif
        endif
      endif
 
*---- Copy result and drop working storage.
      call lmcopy(nord, dq(iresp+1), dq(iresm+1), hp, hm)
      iwork = isave
 
      end
