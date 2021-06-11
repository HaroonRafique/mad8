      subroutine pa6ini(nord)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build bookkeeping tables for polynomials in six variables.         *
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
      integer i,i1,i2,imax,imin,index,iord,itop,jcarry,jord,jvar,l,last,
     +lastnz,lbase,mvar,nd,nord,nterms
 
      parameter         (mvar = 6)
      integer           jexp(6)
 
*---- Initialize link area.
      call mzlink(0, '/PA6LNK/', lexp6(1), lprd6, lexp6(1))
 
*---- Calculate IBOT6(JORD) and ITOP6(JORD):
*     NTERMS is number of terms for order JORD.
*     For negative orders the limits mean all terms up to |order|.
      nterms = 1
      ibot6(0) = 1
      itop6(0) = 0
      do 10 jord = 1, 6
        nterms       = (nterms * (jord + mvar - 1)) / jord
        ibot6(jord)  = itop6(jord-1) + 1
        itop6(jord)  = itop6(jord-1) + nterms
        ibot6(-jord) = 1
        itop6(-jord) = itop6(jord)
   10 continue
 
*---- Set up table of indices.
      itop = itop6(nord)
      do 20 jvar = 1, mvar
        jexp(jvar) = 0
        call mzbook(2, l, lexp6(jvar), 1, 'EXP6', 0, 0, itop, 2, 0)
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
          iq(lexp6(jvar)+index) = jexp(jvar)
   80   continue
   90 continue
 
*---- Set up table of products for total order up to 6.
      itop = itop6(nord/2)
      call mzbook(2, lprd6, lprd6, 1, 'PRD6', itop, itop, 0, 2, 0)
      do 140 iord = 1, 3
        do 130 i2 = ibot6(iord), itop6(iord)
          imin = ibot6(iord) - 1
          imax = itop6(6-iord)
          nd = imax - imin
          call mzbook(2, lbase, lprd6, -i2, 'PR61', 0, 0, nd, 2, 0)
          do 120 i1 = imin + 1, imax
            do 110 jvar = 1, mvar
              jexp(jvar) = iq(lexp6(jvar)+i1) + iq(lexp6(jvar)+i2)
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
      itop = itop6(nord-1)
      call mzbook(2, l, lind61, 1, 'ID61', 0, 0, itop, 2, 0)
      call mzbook(2, l, lind62, 1, 'ID62', 0, 0, itop, 2, 0)
      do 190 jord = 2, nord - 1
        do 180 jvar = 1, mvar
          i1 = jexp(jvar)
          jexp(jvar) = last + 1
          do 170 i = i1, i2
            last = last + 1
            iq(lind61+last) = jvar
            iq(lind62+last) = i
  170     continue
  180   continue
        i2 = last
  190 continue
 
      end
