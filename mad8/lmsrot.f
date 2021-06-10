      subroutine lmsrot(nord, psi, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a rotation around s-axis.                    *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   PSI       (real)    Rotation angle.                                *
* Output:                                                              *
*   FP, FM    (map)     Rotation map.                                  *
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
      double precision cospsi,fm,fp,psi,sinpsi
      dimension         fp(*), fm(6,6)
 
*---- S-Rotation is linear, thus F3 = F4 = 0.
      call lmone(nord, fp, fm)
 
*---- F2 terms (F matrix).
      cospsi = cos(psi)
      sinpsi = sin(psi)
      fm(1,1) =   cospsi
      fm(1,3) =   sinpsi
      fm(3,1) = - sinpsi
      fm(3,3) =   cospsi
      fm(2,2) =   cospsi
      fm(2,4) =   sinpsi
      fm(4,2) = - sinpsi
      fm(4,4) =   cospsi
 
      end
