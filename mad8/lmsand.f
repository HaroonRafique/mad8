      subroutine lmsand(nord, t1p, t1m, t2p, t2m, t3p, t3m)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Sandwich a Lie-algebraic map: T3=T1*T2*(T1 inverse)                *
* Source:     MARYLIE, version 3.0 (routine SNDWCH).                   *
* Author:     Liam Healy.                                              *
* Input:                                                               *
*   NORD      (integer) Order of the maps.                             *
*   T1P, T1M  (map)     Transforming map.                              *
*   T2P, T2M  (map)     Map to be transformed.                         *
* Output:                                                              *
*   T3P, T3M  (map)     Result of the sandwich.                        *
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
      integer isave,it4m,it4p,nord
      double precision t1m,t1p,t2m,t2p,t3m,t3p
      dimension         t1p(*), t1m(6,6), t2p(*), t2m(6,6)
      dimension         t3p(*), t3m(6,6)
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
      it4m  = iwork
      it4p  = it4m + 36
      iwork = it4p + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Perform calculation.
      call lminv(nord, t1p, t1m, dq(it4p+1), dq(it4m+1))
      call lmcat(nord, t1p, t1m, t2p, t2m, t3p, t3m)
      call lmcat(nord, t3p, t3m, dq(it4p+1), dq(it4m+1), t3p, t3m)
 
*---- Drop working storage.
      iwork = isave
 
      end
