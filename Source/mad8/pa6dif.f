      subroutine pa6dif(f, jvar, nord, df)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Derivative of homogeneous polynomial in six variables.             *
* Input:                                                               *
*   F(*)      (real)    Polynomial coefficients.                       *
*   JVAR      (integer) Number of variable w.r.t. to differentiate.    *
*   NORD      (integer) Order of polynomial.                           *
* Output:                                                              *
*   DF(*)     (real)    Coefficients of derivative.                    *
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
      integer index,iorder,jvar,lbasv,lexpv,nord
      double precision df,f
      dimension         f(*), df(*)
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
 
      iorder = sign(abs(nord)-1,nord)
 
*---- Calculate derivative.
      lbasv = lq(lprd6-jvar)
      lexpv = lexp6(jvar)
      do 10 index = ibot6(iorder), itop6(iorder)
        df(index) = (iq(lexpv+index) + 1) * f(iq(lbasv+index))
   10 continue
 
      end
