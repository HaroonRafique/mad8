      subroutine pa3dif(f, jvar, nord, df)
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
      integer ibot3,itop3,lexp3,lind31,lind32,lprd3
 
*---- Bookkeeping tables for polynomials of three variables.
      common /pa3lnk/   ibot3(-6:6), itop3(-6:6), lexp3(3),
     +                  lind31, lind32, lprd3
      save              /pa3lnk/
 
      iorder = sign(abs(nord)-1,nord)
 
*---- Calculate derivative.
      lbasv = lq(lprd3-jvar)
      lexpv = lexp3(jvar)
      do 10 index = ibot3(iorder), itop3(iorder)
        df(index) = (iq(lexpv+index) + 1) * f(iq(lbasv+index))
   10 continue
 
      end
