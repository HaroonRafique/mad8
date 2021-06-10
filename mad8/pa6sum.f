      subroutine pa6sum(scalar, source, nord, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Linear combination of two polynomials:                             *
*     Target = target + scalar * source.                               *
* Input:                                                               *
*   SCALAR              Scale factor for source.                       *
*   SOURCE(*)           Coefficients for source.                       *
*   NORD                Order of polynomials.                          *
*       NORD > 0: monomials for order NORD only.                       *
*       NORD < 0: all monomials up to order NORD.                      *
* Input/output:                                                        *
*   TARGET(*)           Coefficients for target.                       *
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
      double precision one,scalar,source,target
      dimension         target(*), source(*)
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
 
      parameter         (one = 1.0d0)
 
      if (scalar .ne. one) then
        do 10 index = ibot6(nord), itop6(nord)
          target(index) = target(index) + scalar * source(index)
   10   continue
      else
        do 20 index = ibot6(nord), itop6(nord)
          target(index) = target(index) + source(index)
   20   continue
      endif
 
      end
