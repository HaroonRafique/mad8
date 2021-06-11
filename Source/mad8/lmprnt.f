      subroutine lmprnt(iunit, nord, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print complete representation of a Lie-algebraic map.              *
* Input:                                                               *
*   IUNIT     (integer) Logical output unit.                           *
*   NORD      (integer) Order of the map.                              *
*   FP, FM    (map)     The map to be printed.                         *
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
      integer iunit,nord
      double precision fm,fp
      dimension         fp(*), fm(6,6)
 
*---- Linear terms contained in FM.
      write (iunit, 910)
      call m66prt(fm, iunit)
 
*---- Polynomials F1 through F(NORD).
      call pa6prt(fp, nord, iunit)
 
  910 format(' Matrix of map:')
 
      end
