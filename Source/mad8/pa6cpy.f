      subroutine pa6cpy(source, nord, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Copy polynomial.                                                   *
* Input:                                                               *
*   SOURCE(*)           Coefficients of source polynomial.             *
*   NORD      (integer) Order of polynomial:                           *
*       NORD > 0: monomials for order NORD only.                       *
*       NORD < 0: all monomials up to order NORD.                      *
* Output:                                                              *
*   TARGET(*)           Coefficients of target polynomial.             *
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
      integer index,nord
      double precision source,target
      dimension         target(*), source(*)
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
 
      do 10 index = ibot6(nord), itop6(nord)
        target(index) = source(index)
   10 continue
 
      end
