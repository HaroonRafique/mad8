      subroutine lachrm(nord, fp, fm, a1m, a2m)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Chromatic expansion of a Lie-algebraic map, when the F3 and F4     *
*   parts contain only terms linear and quadratic in PT, respectively. *
* Source:     MARYLIE, version 3.0 (routine CHREXP).                   *
* Input:                                                               *
*   NORD      (integer) Order of the map F (at most 4).                *
*   FP, FM    (map)     The map to be expanded.                        *
* Output:                                                              *
*   A1M(6,6)  (real)    1'st derivative of transfer matrix.            *
*   A2M(6,6)  (real)    2'nd derivative of transfer matrix.            *
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
      integer isave,it1m,it1p,it2m,nord
      double precision a1m,a2m,fm,fp,half
      dimension         fp(*), fm(6,6), a1m(6,6), a2m(6,6)
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
 
      parameter         (half = 0.5d0)
 
*---- Allocate working space.
      isave = iwork
      it2m  = iwork
      it1m  = it2m + 36
      it1p  = it1m + 36
      iwork = it1p + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Calculate matrix T1M associated with PT*F2 terms in F.
      call pa6clr(dq(it1p+1), -nord)
      if (nord .ge. 3) then
        dq(it1p+7) = fp(33)
        dq(it1p+8) = fp(38)
        dq(it1p+9) = fp(42)
        dq(it1p+10) = fp(45)
        dq(it1p+13) = fp(53)
        dq(it1p+14) = fp(57)
        dq(it1p+15) = fp(60)
        dq(it1p+18) = fp(67)
        dq(it1p+19) = fp(70)
        dq(it1p+22) = fp(76)
      endif
      call m66mak(dq(it1p+1), dq(it1m+1))
 
*---- Compute A1M = T1M * FM.
      call m66mpy(dq(it1m+1), fm, a1m)
 
*---- Calculate matrix T2M associated with (PT**2)*F2 terms in F.
      call pa6clr(dq(it1p+1), -nord)
      if (nord .ge. 4) then
        dq(it1p+7) = fp(104)
        dq(it1p+8) = fp(119)
        dq(it1p+9) = fp(129)
        dq(it1p+10) = fp(135)
        dq(it1p+13) = fp(154)
        dq(it1p+14) = fp(164)
        dq(it1p+15) = fp(170)
        dq(it1p+18) = fp(184)
        dq(it1p+19) = fp(190)
        dq(it1p+22) = fp(200)
      endif
      call m66mak(dq(it1p+1), dq(it2m+1))
 
*---- Compute A2M = (T2M + T1M**2 / 2.0) * FM
      call m66mpy(dq(it1m+1), dq(it1m+1), dq(it1m+1))
      call m66scl(half, dq(it1m+1), dq(it1m+1))
      call m66add(dq(it2m+1), dq(it1m+1), dq(it1m+1))
      call m66mpy(dq(it1m+1), fm, a2m)
 
*---- Drop working storage.
      iwork = isave
 
      end
