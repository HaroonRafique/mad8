      subroutine exeval(lexp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Evaluate one expression, propagate modification flags.             *
* Input:                                                               *
*   LEXP(1)   (pointer) Pointer to expression bank.                    *
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
      integer ibias,idata,iopr,jbit,jop
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
      integer isopr,isval,level,maxstk
      double precision rsval
 
*---- Stack for expression decoding and evaluation.
      parameter         (maxstk = 100)
      common /exstki/   level, isopr(maxstk), isval(maxstk)
      common /exstkr/   rsval(maxstk)
      save              /exstki/, /exstkr/
      integer ifun,ipre,narg,nfun
 
*---- Function definitions for expressions.
      parameter         (nfun = 26)
      common /funnam/   funnam(nfun)
      common /fundat/   ipre(-8:nfun), ifun(nfun), narg(nfun)
      save              /funnam/, /fundat/
      character*(mcnam) funnam
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
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
 
      character*(mcnam) bnknam
      logical           change
 
      lexexp = lexp(1)
      if (lexexp .eq. 0) then
        call aafail('EXEVAL', 1,
     +  'Internal MAD error: Missing expression.')
        go to 9999
      endif
 
*---- Operations.
      change = .false.
      level = 0
      ibias = 0
      do 90 jop = 1, iq(lexexp-2)
        iopr = iq(lexexp+ibias+mxop)
        lexvar = lq(lexexp-jop)
        lexbnk = lq(lexvar-1)
 
*---- Load constant.
        if (iopr .eq. -1) then
          level = level + 1
          call ucopy(q(lexexp+ibias+mxval), rsval(level), mwflt)
 
*---- Load parameter, attribute or position from sequence.
        else if (iopr .le. -2) then
          if (iopr .eq. -4) then
            idata = iq(lexvar+mvbias)
          else
            idata = mbat + mcsiz * (iq(lexvar+mvbias) - 1) + mcval
          endif
          level = level + 1
          call ucopy(q(lexbnk+idata), rsval(level), mwflt)
          if (jbit(iq(lexbnk),mxmod) .ne. 0) change = .true.
 
*---- Operations.
        else
          call exoper(iopr)
        endif
        ibias = ibias + mxsiz
   90 continue
 
*---- Store result.
      lexbnk = lq(lexexp+1)
      idata = iq(lexexp+ibias+mxval)
      call ucopy(rsval(level), q(lexbnk+idata), mwflt)
      if (change) then
        call sbit1(iq(lexbnk), mxmod)
        if (debug) then
          call diname(ldbnk, iq(lexbnk+mbnam), bnknam)
          msg(1) = 'Marking bank as modified: ' // bnknam
          call aainfo('EXEVAL', 1, msg)
        endif
      endif
      level = level - 1
 
*---- Dump expression and result.
      if (iexpfl .eq. 1  .or.  iexpfl .eq. 3) then
        call exdump(lexexp)
        write (msg, '(''Result = '',1P,E16.8)') rsval(1)
        call aainfo('EXEVEAL', 1, msg)
      endif
      if (iexpfl .eq. 2  .or.  iexpfl .eq. 3) then
        call dzshow('expression', 0, lexexp, 'V', 0, 0, 0, 0)
      endif
 
*---- Check for empty stack.
      if (level .ne. 0) then
        call aafail('EXEVAL', 1,
     +  'Internal MAD error: Invalid expression bank.')
        call exdump(lexexp)
      endif
 
 9999 end
