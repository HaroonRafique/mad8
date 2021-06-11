      subroutine lmrevf(gp, gm, nord, hp, hm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Reverse the order of factorization.                                *
* Source:     MARYLIE, version 3.0 (routine REVF).                     *
* Author:     Liam Healy, April 1985.                                  *
* Input:                                                               *
*   GP, GM    (map)     Map in standard order:                         *
*                       exp(:g2:)exp(:g3:)exp(:g4).                    *
*   NORD      (integer) Order of the map.                              *
* Output:                                                              *
*   HP, HM    (map)     Map with reverse factorization:                *
*                       exp(:h4:)exp(:h3:)exp(:h2:).                   *
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
      integer isave,itemp,jord,nord
      double precision gm,gp,hm,hp
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
*---- Allocate working space.
      isave = iwork
      itemp = iwork
      iwork = itemp + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Copy matrix.
      call m66cpy(gm, hm)
      call pa6clr(hp, -2)
 
*---- Transform higher terms.
      do 10 jord = 3, nord
        call pa6xfm(gp, jord, hm, dq(itemp+1))
        call pa6cpy(dq(itemp+1), jord, hp)
   10 continue
 
*---- Drop working storage.
      iwork = isave
 
      end
