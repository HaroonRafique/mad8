      subroutine lminv(nord, gp, gm, hp, hm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Invert Lie-algebraic map.                                          *
* Source:     MARYLIE, version 3.0 (routine INV).                      *
* Author:     Liam Healy, April 1985.                                  *
* Input:                                                               *
*   NORD      (integer) Order of the map.                              *
*   GP, GM    (map)     Map to be inverted.                            *
* Output:                                                              *
*   HP, HM    (map)     Map resulting from inversion.                  *
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
      double precision gm,gp,hm,hp,one
      dimension         gp(*), gm(6,6), hp(*), hm(6,6)
 
      parameter         (one = 1.0d0)
 
*---- Reverse the factorization.
      call lmrevf(gp, gm, nord, hp, hm)
 
*---- Invert the matrix.
      call m66inv(hm, hm)
 
*---- Invert polynomials.
      call pa6scl(-one, hp, -nord, hp)
 
      end
