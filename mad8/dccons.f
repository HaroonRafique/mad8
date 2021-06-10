      subroutine dccons(ilink, idata, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode constraint data type.                                       *
* Input:                                                               *
*   LDCBNK   /DCLINK/   Data bank pointer.                             *
*   ILINK    (integer)  Bias for pointer to sub-bank.                  *
*   IDATA    (integer)  Bias for data block.                           *
* Output:                                                              *
*   EFLAG    (logical)  Error flag.                                    *
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
      integer ibias,idata,iexpr,ilink,index,ioper,iput,itype,j,nd
      double precision rval
      logical           eflag
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
*---- Lift constraint bank, if not done already.
      eflag = .false.
      ldcatt = lq(ldcbnk-ilink)
      if (ldcatt .eq. 0) then
        nd = 2 * mcsiz + 2
        call mzbook(2, ldcatt, ldcbnk, -ilink, 'CONS', 2, 2, nd, 7, 0)
        iq(ldcatt+1) = 16 * 1 + 2
        ibias = 2
        do 10 j = 1, 2
          iq(ldcatt+ibias+mcf1) = 16 * 1 + 2
          iq(ldcatt+ibias+mcf2) = 16 * mwnam + mreal
          ibias = ibias + mcsiz
   10   continue
      endif
      itype = iq(ldcatt+2)
 
*---- "=", "<", or ">" valid.
      ioper = index('=><', token(jtok-1))
 
*---- "=", Equality constraint.
  100 continue
        if (ioper .eq. 1) then
          if (itype .ne. 0) then
            call rdfail('DCCONS', 1, 'Inconsistent constraint.')
            itype = 0
          else
            itype = 4
            iput = 1
          endif
 
*---- ">", Minimum constraint.
        else if (ioper .eq. 2) then
          if (itype .eq. 1  .or.  itype .ge. 3) then
            call rdfail('DCCONS', 1, 'Inconsistent constraint.')
            itype = 0
          else
            itype = itype + 1
            iput = 1
          endif
 
*---- "<", Maximum constraint.
        else if (ioper .eq. 3) then
          if (itype .ge. 2) then
            call rdfail('DCCONS', 1, 'Inconsistent constraint.')
            itype = 0
          else
            itype = itype + 2
            iput = 2
          endif
 
*---- Invalid operator.
        else
          call rdfail('DCCONS', 1,
     +    'Constraint relational "=", "<", or ">" expected.')
          itype = 0
        endif
 
*---- Decode constraint value.
        call exread(2, rval, iexpr)
        if (iexpr .eq. 0) then
          itype = 0
        else
          ibias = 2 + (iput - 1) * mcsiz
          call exmake(ldcatt, iput, ibias + mcval, rval, iexpr)
          iq(ldcatt+ibias+mctyp) = 10 * mtflt + iexpr
        endif
        ioper = index('=><', token(jtok))
      if (ioper .gt. 0) then
        jtok = jtok + 1
        go to 100
      endif
 
*---- Store constraint type, or drop constraint in case of error.
  200 continue
      if (itype .ne. 0) then
        iq(ldcatt+2) = itype
        iq(ldcbnk+idata+mctyp) = 10*mtcon + itype
      else
        call aadrop(ldcatt)
        iq(ldcbnk+idata+mctyp) = 10*mtcon
      endif
 
      end
