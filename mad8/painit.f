      subroutine painit(nord)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Prepare bookkeeping tables for polynomial and Lie algebra.         *
* Input:                                                               *
*   NORD      (integer) Maximum order for the tables to be built.      *
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
      integer index,ipaflg,itop,j,nord
 
      integer           jp(3), jq(3)
      data ipaflg       / 0 /
 
*---- Polynomials with three variables.
      if (ipaflg .eq. 0) then
        call pa3ini(nord)
 
*---- Polynomials with six variables.
        call pa6ini(nord)
 
*---- Index to rearrange a polynomial from 6 to 3 variables.
        itop = itop6(nord-1)
        call mzbook(2, larrq, larrq, 1, 'ARRQ', 0, 0, itop, 2, 0)
        call mzbook(2, larrp, larrp, 1, 'ARRP', 0, 0, itop, 2, 0)
        do 10 j = 1, itop
          jq(1) = iq(lexp6(1)+j)
          jq(2) = iq(lexp6(3)+j)
          jq(3) = iq(lexp6(5)+j)
          call paxind(jq, 3, index)
          iq(larrq+j) = index
          jp(1) = iq(lexp6(2)+j)
          jp(2) = iq(lexp6(4)+j)
          jp(3) = iq(lexp6(6)+j)
          call paxind(jp, 3, index)
          iq(larrp+j) = index
   10   continue
        ipaflg = 1
      endif
 
      end
