      subroutine pa3ini(nord)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build bookkeeping tables for polynomials in three variables.       *
* Algorithm by: Liam Healy.                                            *
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
      integer ibot3,itop3,lexp3,lind31,lind32,lprd3
 
*---- Bookkeeping tables for polynomials of three variables.
      common /pa3lnk/   ibot3(-6:6), itop3(-6:6), lexp3(3),
     +                  lind31, lind32, lprd3
      save              /pa3lnk/
      integer i,i1,i2,imax,imin,index,iord,itop,jcarry,jord,jvar,l,last,
     +lastnz,lbase,mvar,nd,nord,nterms
 
      parameter         (mvar = 3)
      integer           jexp(mvar)
 
*---- Initialize link area.
      call mzlink(0, '/PA3LNK/', lexp3(1), lprd3, lexp3(1))
 
*---- Calculate IBOT3(JORD) and ITOP3(JORD):
*     NTERMS is number of terms for order JORD.
*     For negative orders the limits mean all terms up to |order|.
      nterms = 1
      ibot3(0) = 1
      itop3(0) = 0
      do 10 jord = 1, nord
        nterms       = (nterms * (jord + mvar - 1)) / jord
        ibot3(jord)  = itop3(jord-1) + 1
        itop3(jord)  = itop3(jord-1) + nterms
        ibot3(-jord) = 1
        itop3(-jord) = itop3(jord)
   10 continue
 
*---- Set up table of indices.
      itop = itop3(nord)
      do 20 jvar = 1, mvar
        jexp(jvar) = 0
        call mzbook(2, l, lexp3(jvar), 1, 'EXP3', 0, 0, itop, 2, 0)
   20 continue
      do 90 index = 1, itop
        jcarry = jexp(mvar)
        jexp(mvar) = 0
        lastnz = 0
        do 60 jvar = 1, mvar - 1
          if (jexp(jvar) .gt. 0) lastnz = jvar
   60   continue
        if (lastnz .gt. 0) jexp(lastnz) = jexp(lastnz) - 1
        jexp(lastnz+1) = jexp(lastnz+1) + 1 + jcarry
        do 80 jvar = 1, mvar
          iq(lexp3(jvar)+index) = jexp(jvar)
   80   continue
   90 continue
 
*---- Set up table of products for total order up to NORD.
      itop = itop3(nord/2)
      call mzbook(2, lprd3, lprd3, 1, 'PRD3', itop, itop, 0, 2, 0)
      do 140 iord = 1, mvar
        do 130 i2 = ibot3(iord), itop3(iord)
          imin = ibot3(iord) - 1
          imax = itop3(nord-iord)
          nd = imax - imin
          call mzbook(2, lbase, lprd3, -i2, 'PR31', 0, 0, nd, 2, 0)
          do 120 i1 = imin + 1, imax
            do 110 jvar = 1, mvar
              jexp(jvar) = iq(lexp3(jvar)+i1) + iq(lexp3(jvar)+i2)
  110       continue
            call paxind(jexp, mvar, index)
            iq(lbase+i1-imin) = index
  120     continue
  130   continue
  140 continue
 
*---- Indices for monomial calculation:
      do 150 jvar = 1, mvar
        jexp(jvar) = jvar
  150 continue
      i2 = mvar
      last = mvar
      itop = itop3(nord-1)
      call mzbook(2, l, lind31, 1, 'ID31', 0, 0, itop, 2, 0)
      call mzbook(2, l, lind32, 1, 'ID32', 0, 0, itop, 2, 0)
      do 190 jord = 2, nord - 1
        do 180 jvar = 1, mvar
          i1 = jexp(jvar)
          jexp(jvar) = last + 1
          do 170 i = i1, i2
            last = last + 1
            iq(lind31+last) = jvar
            iq(lind32+last) = i
  170     continue
  180   continue
        i2 = last
  190 continue
 
      end
