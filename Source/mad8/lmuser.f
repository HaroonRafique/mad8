      subroutine lmuser(nord, isp, el, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    User-defined map.                                                 *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   ISP       (integer) Subprocess code.                               *
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
      integer isp,nord
      double precision el,fm,fp
      dimension         fp(*), fm(6,6)
 
*---- Set up identity map.
      call lmone(nord, fp, fm)
 
      end
