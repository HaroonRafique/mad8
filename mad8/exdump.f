      subroutine exdump(lexp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Dump expression bank.                                              *
* Output:                                                              *
*   LEXP(1)   (pointer)  Pointer to expression bank.                   *
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
      integer ibias,idata,idir,iopr,jbias,jop
      double precision rval
      integer           lexp(*)
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
      integer mvattr,mvbank,mvbias,mvf1,mvf2,mvind1,mvind2,mvind3,
     +mvseen,mvsiz
 
*---- Bias for variable reference group.
      parameter         (mvf1   = 1,
     +                   mvbank = 2,
     +                   mvattr = mvbank + mwnam,
     +                   mvf2   = mvattr + mwnam,
     +                   mvseen = mvf2 + 1,
     +                   mvind1 = mvseen + 1,
     +                   mvind2 = mvind1 + 1,
     +                   mvind3 = mvind2 + 1,
     +                   mvbias = mvind3 + 1,
     +                   mvsiz = mvbias)
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
      integer ifun,ipre,narg,nfun
 
*---- Function definitions for expressions.
      parameter         (nfun = 26)
      common /funnam/   funnam(nfun)
      common /fundat/   ipre(-8:nfun), ifun(nfun), narg(nfun)
      save              /funnam/, /fundat/
      character*(mcnam) funnam
      integer lcali,lcatt,lccls,lccmd,lccom,lcdef,lcelm,lcexp,lcfld,
     +lckey,lcseq,lcspl,lcsrc,lcvar,ldbnk,ldkey,lref1,lref2,lsali,lscom,
     +lsdir,lsfld,lsflg,lsnum,lsspl,lbeam,lconsm,ldummy
 
*---- Global reference links.
      common /refer/    lref1,
     +                  lcali, lcatt, lccls, lccmd, lccom, lcdef, lcelm,
     +                  lcexp, lcfld, lckey, lcseq, lcspl, lcsrc, lcvar,
     +                  lbeam, lconsm, ldbnk(4), ldkey(4), ldummy(10),
     +                  lsali, lscom, lsdir, lsfld, lsflg, lsnum, lsspl,
     +                  lref2
      save              /refer/
      integer liftseq, currseq
      common /seqinfi/ liftseq, currseq
      character * (mcnam) sequnam, seqnames
      common /seqinfc/ sequnam, seqnames(mttact)
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      character*(mcnam) axbank, axattr
 
      write (iqlog, 910)
      ibias = 0
      lexexp = lexp(1)
 
*---- Loop over load and arithmetic operations.
      do 90 jop = 1, iq(lexexp-2)
        lexvar = lq(lexexp-jop)
        iopr = iq(lexexp+ibias+mxop)
        if (iopr .eq. -8) then
          call uhtoc(q(lexvar+1), mcwrd, axbank, mcnam)
          write (iqlog, 920) axbank
        else if (iopr .le. -5) then
          jbias = iq(lexexp+ibias+mxval)
          if (iopr .eq. -7) then
            write (iqlog, 930) 'GETTABD', jbias
          else if (iopr .eq. -6) then
            write (iqlog, 930) 'GETTABS', jbias
          else if (iopr .eq. -5) then
            write (iqlog, 930) 'GETTABI', jbias
          endif
        else if (iopr .eq. -4) then
          call uhtoc(q(lexvar+mvbank), mcwrd, axbank, mcnam)
          write (iqlog, 940) axbank, iq(lexvar+mvind1)
        else if (iopr .eq. -3) then
          call uhtoc(q(lexvar+mvbank), mcwrd, axbank, mcnam)
          call uhtoc(q(lexvar+mvattr), mcwrd, axattr, mcnam)
          write (iqlog, 950) axbank, axattr,
     +      iq(lexvar+mvind1), iq(lexvar+mvind2), iq(lexvar+mvind3)
        else if (iopr .eq. -2) then
          call uhtoc(q(lexvar+mvbank), mcwrd, axbank, mcnam)
          write (iqlog, 960) axbank, iq(lexvar+mvind1)
        else if (iopr .eq. -1) then
          call ucopy(q(lexexp+ibias+mxval), rval, mwflt)
          write (iqlog, 970) rval
        else
          write (iqlog, 980) funnam(iopr)
        endif
        ibias = ibias + mxsiz
   90 continue
 
*---- Store operation.
      if (ibias .lt. iq(lexexp-1)) then
        lexbnk = lq(lexexp+1)
        idata  = iq(lexexp+ibias+mxval)
        idir   = iq(lexbnk+mbnam)
        if (idir .eq. 0) then
          axbank = ' '
        else
          call diname(ldbnk, idir, axbank)
        endif
        write (iqlog, 990) axbank, idata
      endif
 
  910 format(' EXDUMP.')
  920 format(t11,'GETDESC',t21,a)
  930 format(t11,a,t41,'[',i5,']')
  940 format(t11,'GETSEQ',t21,a,t41,'[',i5,']')
  950 format(t11,'GETATTR',t21,a,t41,'[',a,'(',i5,',',i5,',',i5,')]')
  960 format(t11,'GETPAR',t21,a,t41,'[',i5,']')
  970 format(t11,'GETCON',t21,1pe16.8)
  980 format(t11,a)
  990 format(t11,'PUT',t21,a,t41,'[',i5,']'/' ')
 
      end
