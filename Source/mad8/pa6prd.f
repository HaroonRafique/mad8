      subroutine pa6prd(fact1, n1, fact2, n2, target)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Product of two homogeneous polynomials in six variables.           *
* Input:                                                               *
*   FACT1(*)            Coefficients of first factor.                  *
*   N1                  Order of first factor.                         *
*   FACT2(*)            Coefficients of second factor.                 *
*   N2                  Order of second factor.                        *
* Output:                                                              *
*   TARGET(*)           Coefficients of product.                       *
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
      integer i1,i2,lp,n1,n2
      double precision f1,f2,fact1,fact2,target
      dimension         target(*), fact1(*), fact2(*)
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
 
*---- N1 is greater or equal to N2.
      if (n1 .ge. n2) then
        do 20 i2 = ibot6(n2), itop6(n2)
          f2 = fact2(i2)
          if(f2 .ne. 0.0) then
            lp = lq(lprd6-i2) - itop6(n2-1)
            do 10 i1 = ibot6(n1), itop6(n1)
              target(iq(lp+i1)) = target(iq(lp+i1)) + fact1(i1) * f2
  10        continue
          endif
  20    continue
 
*---- N1 is less than N2.
      else
        do 70 i1 = ibot6(n1), itop6(n1)
          f1 = fact1(i1)
          if(f1 .ne. 0.0) then
          lp = lq(lprd6-i1) - itop6(n1-1)
            do 60 i2 = ibot6(n2), itop6(n2)
              target(iq(lp+i2)) = target(iq(lp+i2)) + f1 * fact2(i2)
  60        continue
          endif
  70    continue
      endif
 
      end
