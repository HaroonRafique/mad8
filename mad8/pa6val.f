      function pa6val(poly, nord, v)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Evaluate polynomial of six variables.                             *
* Input:                                                               *
*   POLY(*)             Coefficients for polynomial.                   *
*   NORD                Order of polynomials.                          *
* Output:                                                              *
*   V(6)      (real)    Value of variables.                            *
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
      integer i,index,iord,nord
      double precision pa6val,poly,v,value,vect
      dimension         poly(*), v(6)
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
      dimension         vect(923)
 
      do 10 i = 1, 6
        vect(i) = v(i)
   10 continue
      do 30 iord = 2, nord
        do 20 index = ibot6(iord), itop6(iord)
          vect(index) = vect(iq(lind61+index)) * vect(iq(lind62+index))
   20   continue
   30 continue
 
      value = 0.0
      do 40 index = 1, itop6(iord)
        value = value + vect(index) * poly(index)
   40 continue
 
      pa6val = value
 
      end
