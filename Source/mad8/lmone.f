      subroutine lmone(nord, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Set up identity map.                                              *
* Source:     MARYLIE, version 3.0 (routine IDENT).                    *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
* Output:                                                              *
*   FP, FM    (map)     Identity map.                                  *
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
      double precision fm,fp
      dimension         fp(*), fm(6,6)
 
      call m66one(fm)
      call pa6clr(fp, -nord)
 
      end
