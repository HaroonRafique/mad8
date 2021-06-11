      subroutine lmcopy(nord, rp, rm, tp, tm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Copy a Lie-algebraic map.                                          *
* Source:     MARYLIE, version 3.0 (routine MAPMAP).                   *
* Input:                                                               *
*   NORD      (integer) Order of the map.                              *
*   RP, RM    (map)     Origin map.                                    *
* Output:                                                              *
*   TP, TM    (map)     Destination map.                               *
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
      integer nord
      double precision rm,rp,tm,tp
      dimension         rp(*), rm(6,6), tp(*), tm(6,6)
 
      call m66cpy(rm, tm)
      call pa6cpy(rp, -nord, tp)
 
      end
