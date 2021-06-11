      subroutine lnxput(icode, idir, iseq, nseq)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Store a position to the sequence banks during line expansion.      *
* Input:                                                               *
*   ICODE     (integer) Position code: 1=element, 2=entry, 3=exit.     *
*   IDIR      (integer) Directory index for item in this position.     *
* Input/output:                                                        *
*   ISEQ      (integer) Current position in expansion.                 *
*   NSEQ      (integer) length of expansion banks allocated so far.    *
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
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
      integer icode,idir,iseq,jdir,msi,nseq
 
      parameter         (msi = 500)
 
*---- Make sure there is enough space in the sequence bank.
      iseq = iseq + 1
      if (iseq .gt. nseq) then
        call mzpush(0, lsdir, 0, msi, 'I')
        call mzpush(0, lsflg, 0, msi, 'I')
        nseq = nseq + msi
      endif
 
*---- Increment occurrence counts.
      if (icode .ne. 3) then
        iq(ldbnk(4)+idir) = iq(ldbnk(4)+idir) + 1
        lref2 = lq(ldbnk(3)-idir)
 10     if (lref2 .ne. 0) then
          jdir = iq(lref2+mbnam)
          if (jdir .ne. idir) iq(ldbnk(4)+jdir) = iq(ldbnk(4)+jdir) + 1
          if (iq(lref2+mbpr) .ne. mplin) then
            lref2 = lq(lref2-iq(lref2+mbat)-mbecls)
            go to 10
          endif
        endif
      endif
 
      iq(lsdir+iseq) = idir
      call sbyt(icode, iq(lsflg+iseq), 1, mcode)
      call sbyt(iq(ldbnk(4)+idir), iq(lsflg+iseq), mocc1, mocc2)
 
      end
