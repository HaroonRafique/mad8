      subroutine lmfrg2(nord, h, e2x, e2y, sk1, h2, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Lie algebraic map for fringe field at exit.                       *
* Input:                                                               *
*                                                                      *
*   NORD      (integer) Order desired.                                 *
* Output:                                                              *
*   FP, FM    (map)     Element map.                                   *
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
      double precision e2x,e2y,fm,fp,h,h2,secx,sk1,ss,tanx,tany
      dimension         fp(*), fm(6,6)
 
*---- Linear terms.
      call lmone(nord, fp, fm)
      tanx = tan(e2x)
      tany = tan(e2y)
      secx = 1.0 / cos(e2x)
      fm(2,1) = + h * tanx
      fm(4,3) = - h * tany
 
*---- Third-order terms.
      if (nord .ge. 3) then
        ss = h * h2 * secx**3 + 2.0 * sk1 * tanx
        fp(28) = (ss + h**2 * tanx**3) / 6.0
        fp(29) = - h * tanx**2 / 2.0
        fp(39) = (h**2 * tanx * tany**2 - ss) / 2.0
        fp(40) = + h * tanx * tany
        fp(54) = + h * secx**2 / 2.0
      endif
 
*---- Fourth order not yet avaliable.
 
      end
