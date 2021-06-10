      subroutine laspuc(nord, fp, fm, gp, gm, tp, tm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Purify the chromatic F3 part of a static Lie-algebraic map.        *
* Source:     MARYLIE, version 3.0 (routine SCPUR3).                   *
* Input:                                                               *
*   NORD      (integer) Order of the map F (at most 4).                *
*   FP, FM    (map)     Original map to be purified (unchanged).       *
* Output:                                                              *
*   GP, GM    (map)     Purified map.                                  *
*   TP, TM    (map)     Purifying map, i. e. G = T*F*T**(-1).          *
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
      double precision fm,fp,gm,gp,tm,tp
      dimension         fp(*), fm(6,6)
      dimension         gp(*), gm(6,6), tp(*), tm(6,6)
 
      if (nord .ge. 3) then
        call laspur(nord, fp, fm, gp, gm, tp, tm, 31, 42)
      endif
 
      end
