      subroutine lmmask(nord, wipe, gp, gm, hp, hm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Wipe out monomial coefficients as specified by WIPE.               *
* Source:     MARYLIE, version 3.0 (routine MASK).                     *
* Input:                                                               *
*   NORD      (integer) Order of the map.                              *
*   WIPE(*)   (logical) .TRUE. for orders to be wiped out.             *
*   GP, GM    (map)     Map to be wiped.                               *
* Output:                                                              *
*   HP, HM    (map)     Result of wiping out.                          *
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
      integer jord,nord
      double precision gm,gp,hm,hp
      logical           wipe(*)
      dimension         gp(*), gm(6,6), hp(*), hm(6,6)
 
*---- Matrix is always kept.
      call m66cpy(gm, hm)
 
*---- WIPE(JORD) refers to G(JORD).
      do 90 jord = 1, nord
        if (wipe(jord)) then
          call pa6clr(hp, jord)
        else
          call pa6cpy(gp, jord, hp)
        endif
   90 continue
 
      end
