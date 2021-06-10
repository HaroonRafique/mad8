      subroutine labeta(nord, ap, am, bp, bm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Extract betatron portion of a Lie-algebraic map.                   *
* Source:     MARYLIE, version 3.0 (routine BETMAP).                   *
* Author:     Alex Dragt, August 1986.                                 *
* Input:                                                               *
*   NORD      (integer) Order of the map A (at most 4).                *
*   AP, AM    (map)     Map about a fixed point.                       *
* Output:                                                              *
*   BP, BM    (map)     Betatron portion of map A, i.e. the map which  *
*                       transforms A to normal form.                   *
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
      integer isave,itm,itp,nord
      double precision am,ap,bm,bp
      dimension         ap(*), am(6,6), bp(*), bm(6,6)
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
 
*---- Allocate working space.
      isave = iwork
      itm   = iwork
      itp   = itm + 36
      iwork = itp + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Extraction of betatron term B.
      call pa6clr(bp, -nord)
      call m66cpy(am, bm)
 
*---- First pass: terms linear in PT.
      if (nord .ge. 3) then
        bp(33) = ap(33)
        bp(38) = ap(38)
        bp(42) = ap(42)
        bp(45) = ap(45)
        bp(53) = ap(53)
        bp(57) = ap(57)
        bp(60) = ap(60)
        bp(67) = ap(67)
        bp(70) = ap(70)
        bp(76) = ap(76)
 
*---- Second pass: terms quadratic in PT.
        if (nord .ge. 4) then
          call lminv(nord, bp,bm, dq(itp+1),dq(itm+1))
          call lmcat(nord,dq(itp+1),dq(itm+1),ap,am,dq(itp+1),dq(itm+1))
          bp(104) = dq(itp+104)
          bp(119) = dq(itp+119)
          bp(129) = dq(itp+129)
          bp(135) = dq(itp+135)
          bp(154) = dq(itp+154)
          bp(164) = dq(itp+164)
          bp(170) = dq(itp+170)
          bp(184) = dq(itp+184)
          bp(190) = dq(itp+190)
          bp(200) = dq(itp+200)
        endif
      endif
 
*---- Drop working storage.
      iwork = isave
 
      end
