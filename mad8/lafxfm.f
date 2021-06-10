      subroutine lafxfm(nord, gp, gm, fp, hp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Transform a function F with a map G.                               *
* Source:     MARYLIE, version 3.0 (routine FXFORM).                   *
* Input:                                                               *
*   NORD      (integer) Order of the maps involved (at most 4).        *
*   GP, GM    (map)     Transforming map.                              *
*   FP        (poly)    Polynomial to be transformed.                  *
* Output:                                                              *
*   HP        (poly)    The result of H = exp(:G:) F.                  *
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
      integer i,isave,it1p,it2p,it3p,nord
      double precision fp,gm,gp,hp
      dimension         gp(*), gm(6,6), fp(*), hp(*)
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
      it1p  = iwork
      it2p  = it1p + itop6(nord)
      it3p  = it2p + itop6(nord)
      iwork = it3p + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Clear first order part.
      call pa6clr(hp, 1)
 
*---- Copy second order part and transform by GM.
      call pa6xfm(fp, 2, gm, hp)
 
*---- Third order part.
      if (nord .ge. 3) then
 
*---- Compute [G3,F2], accumulate, and transform by GM.
        call pa6brk(gp, 3, fp, 2, dq(it1p+1))
        do 20 i = 28, 83
          dq(it1p+i) = fp(i) + dq(it1p+i)
   20   continue
        call pa6xfm(dq(it1p+1), 3, gm, hp)
 
*---- Fourth order part.
        if (nord .ge. 4) then
 
*---- Compute [G3,F3], [G3,[G3,F2]], and [G4,F2].
          call pa6brk(gp, 3, fp, 3, dq(it2p+1))
          call pa6brk(gp, 3, dq(it1p+1), 3, dq(it3p+1))
          call pa6brk(gp, 4, fp, 2, dq(it1p+1))
 
*---- Accumulate and transform by GM.
          do 30 i = 84, 209
            dq(it1p+i) =
     +      fp(i) + dq(it2p+i) + dq(it3p+i) / 2.0 + dq(it1p+i)
   30     continue
          call pa6xfm(dq(it1p+1), 4, gm, hp)
        endif
      endif
 
*---- Drop working storage.
      iwork = isave
 
      end
