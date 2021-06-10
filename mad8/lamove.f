      subroutine lamove(nord, fp, fm, orbit, hp, hm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Move through a Lie-algebraic map and return the map for the        *
*   vicinity of the orbit.                                             *
* Input:                                                               *
*   NORD      (integer) Order of the map F (at most 4).                *
*   FP, FM    (map)     Map for one element or turn.                   *
* Output:                                                              *
*   ORBIT(6)  (real)    The orbit found.                               *
*   HP, HM    (map)     Map for the vicinity of the orbit.             *
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
      integer ifj,ifk,isave,itm,nord
      double precision fm,fp,gp,gt,half,hm,hp,orbit
      dimension         fp(*), fm(6,6), orbit(*), hp(*), hm(6,6)
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      parameter         (half = 0.5d0)
      dimension         gp(6), gt(6)
      logical           eflag
 
*---- Allocate working space.
      isave = iwork
      itm   = iwork
      ifj   = itm + 36
      ifk   = ifj + 209
      iwork = ifk + 209
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Transform orbit to a G1.
      gp(1) = fp(1) + orbit(2)
      gp(2) = fp(2) - orbit(1)
      gp(3) = fp(3) + orbit(4)
      gp(4) = fp(4) - orbit(3)
      gp(5) = fp(5) + orbit(6)
      gp(6) = fp(6) - orbit(5)
 
*---- Copy original map (F3 through F(NORD)).
      call pa6clr(hp, -nord)
 
*---- Move orbit across F2 term.
      call m66inv(fm, dq(itm+1))
      call pa6xfm(gp, 1, dq(itm+1), gt)
      call pa6cpy(gt, 1, hp)
 
*---- Terms of total rank 3.
      if (nord .ge. 3) then
        call lmexpo(gt, 1, fp, 3, dq(ifj+1), 3)
        call pa6add(hp, dq(ifj+1), -3, hp)
 
*---- Terms of total rank 4.
        if (nord .ge. 4) then
          call lmexpo(gt, 1, fp, 4, dq(ifk+1), 4)
          call pa6add(hp, dq(ifk+1), -4, hp)
          call pa6brk(dq(ifj+1), 1, dq(ifj+1), 2, dq(ifk+1))
          call pa6brk(dq(ifj+1), 1, dq(ifj+1), 3, dq(ifk+1))
          call pa6brk(dq(ifj+1), 3, dq(ifj+1), 2, dq(ifk+1))
          call pa6sum(half, dq(ifk+1), -3, hp)
        endif
      endif
 
*---- Calculate the matrix part of the factored exponential.
      call m66mak(hp, hm)
      call m66exp(hm, dq(itm+1), eflag)
      call m66mpy(dq(itm+1), fm, hm)
 
*---- Convert moved G1 to orbit.
      orbit(1) = - hp(2)
      orbit(2) = + hp(1)
      orbit(3) = - hp(4)
      orbit(4) = + hp(3)
      orbit(5) = - hp(6)
      orbit(6) = + hp(5)
      call pa6clr(hp, -2)
 
*---- Drop working storage.
      iwork = isave
 
      end
