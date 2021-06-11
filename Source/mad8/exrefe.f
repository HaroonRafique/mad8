      subroutine exrefe(ieval, bnknam, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate parameter operation for fetching value.                   *
* Input:                                                               *
*   IEVAL    (integer)  Evaluation flag:                               *
*                       1: Constant, 2: Normal, 3: Deferred.           *
*   BNKNAM   (char)     Data bank name.                                *
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
      integer ieval,index1,index2,index3,iocode,ipar,leng
      character*(mcnam) bnknam
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
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
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
      integer ixopr,ixsub1,ixsub2,ixsub3,maxexp,nxopr
      double precision rxval
 
*---- Expression description.
      parameter         (maxexp = 100)
      common /exprsa/   nxopr, ixopr(maxexp),
     +                  ixsub1(maxexp), ixsub2(maxexp), ixsub3(maxexp)
      common /exprsc/   axbank(maxexp), axattr(maxexp)
      common /exprsr/   rxval(maxexp)
      save              /exprsa/, /exprsc/, /exprsr/
      character*(mcnam) axbank, axattr
      integer isopr,isval,level,maxstk
      double precision rsval
 
*---- Stack for expression decoding and evaluation.
      parameter         (maxstk = 100)
      common /exstki/   level, isopr(maxstk), isval(maxstk)
      common /exstkr/   rsval(maxstk)
      save              /exstki/, /exstkr/
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
 
      character*(mcnam) atrnam
 
*---- Initialize.
      eflag = .false.
      index1 = 1
      index2 = 1
      index3 = 1
 
*---- Parameter or constant.
      if (token(jtok) .ne. '[') then
        call difind(ldbnk, bnknam, ipar, lexbnk)
 
*---- Constant value: Generate constant load.
        if (lexbnk .ne. 0  .and.  iq(lexbnk+mbpr) .eq. mppar  .and.
     +      iq(lexbnk+mbsp) .eq. 1) then
          nxopr = nxopr + 1
          ixopr(nxopr) = - 1
          call ucopy(q(lexbnk+mbat+mcval), rxval(nxopr), mwflt)
          isval(level) = 0
          rsval(level) = rxval(nxopr)
          go to 9999
        endif
 
*---- Generate parameter load.
        atrnam = ' '
        iocode = - 2
 
*---- Bank attribute: finish decoding and generate attribute load.
      else
        jtok = jtok + 1
        call rdword(atrnam, leng)
        if (leng .eq. 0) then
          call rdfail('EXREFE', 1, 'Attribute name expected.')
          eflag = .true.
          go to 9999
        endif
 
*---- Possible subscript list.
        call dcindx(index1, index2, index3, eflag)
        if (eflag) go to 9999
 
*---- Closing bracket.
        if (token(jtok) .ne. ']') then
          call rdfail('EXREFE', 1, 'Closing bracket "]" missing.')
          eflag = .true.
          go to 9999
        endif
        jtok = jtok + 1
 
*---- Generate bank attribute load.
        iocode = - 3
      endif
 
*---- Error, if variable occurs in constant expression.
      if (ieval .eq. 1) then
        call rdfail('EXREFE', 1, 'Variable in constant expression.')
        eflag = .true.
        go to 9999
      endif
      nxopr = nxopr + 1
      ixopr(nxopr) = iocode
      ixsub1(nxopr) = index1
      ixsub2(nxopr) = index2
      ixsub3(nxopr) = index3
      rxval(nxopr) = 0.
      axbank(nxopr) = bnknam
      axattr(nxopr) = atrnam
      isval(level) = 1
      rsval(level) = 0.
 
 9999 end
