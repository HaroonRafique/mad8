      subroutine lmrefl(gp, gm, nord, hp, hm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Reflect a map, i.e. run through its elements in backward order.    *
* Source:     MARYLIE, version 3.0 (routine REV).                      *
* Author:     Alex Dragt, September 1985.                              *
* Input:                                                               *
*   GP, GM    (map)     Map to be reflected.                           *
*   NORD      (integer) Order of the map.                              *
* Output:                                                              *
*   HP, HM    (map)     The reflected map.                             *
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
      integer i,itest,j,nord
      double precision d,gm,gp,hm,hp
      dimension         gp(*), gm(6,6), hp(*), hm(6,6)
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
 
*---- Define the reversing matrix D:
      dimension         d(6)
      data d            / +1.d0, -1.d0, +1.d0, -1.d0, -1.d0, +1.d0 /
 
*---- Compute inverse map.
      call lminv(nord, gp, gm, hp, hm)
 
*---- Reverse matrix portion of map.
      do 10 i = 1, 6
      do 10 j = 1, 6
        hm(i,j) = d(i) * hm(i,j) * d(j)
   10 continue
 
*---- Reverse polynomial portion of map.
      do 20 i = 1, itop6(nord)
        itest = iq(lexp6(2)+i) + iq(lexp6(4)+i) + iq(lexp6(5)+i)
        if(mod(itest,2) .eq. 0) then
          hp(i) = - hp(i)
        endif
   20 continue
 
      end
