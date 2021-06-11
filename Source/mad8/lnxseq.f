      subroutine lnxseq(lbank, iseq, nseq)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Expand a beam line SEQUENCE.                                       *
* Input:                                                               *
*   LBANK(1)  (pointer) Pointer to SEQUENCE.                           *
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
      integer idata,idir,idrf,idrift,iexpr,ilast,ilen1,ilen2,ileng,iln,
     +ilst,ipos,ipr,iref,iseq,isp,jdir,ndrift,nkat,nseq,numlab
      double precision drflen,el,eps,half,pos1,pos2
      integer           lbank(1)
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
      integer meangb,meangg,meangr,mechg,mee1b,mee1g,mee2b,mee2g,meflde,
     +mefrqc,megapb,megapg,meh1b,meh1g,meh2b,meh2g,mehrmc,meintb,meintg,
     +mek1b,mek1g,mek1q,mek2b,mek2s,mek3b,mek3o,mekick,meklm,meksg,
     +mekss,melagc,melen,mesigx,mesigy,metltb,metlte,metltg,metltm,
     +metlto,metltq,metlts,metyp,mevltc,mexcol,mexma,meycol,meyma
      integer meintbx,meintgx,meapro,mek0lm,met0m,mek1lm,met1m,
     +mek2lm,met2m,mek3lm,met3m,meaprm,meapss,melosc,meaprc,mee0l,
     +medel,mephil,mefrql,melosl,mevoll,melagl,meaprl
 
*---- Bias for element attribute values.
*     These statements MUST be consistent with the command dictionary.
*     Routines using this group must also include BANKHEAD and CMDGROUP.
*     Common to all elements: TYPE and L attributes.
      parameter    (metyp  = mbat   + mcval, melen  = metyp  + mcsiz)
*     Common to RBEND and SBEND.
      parameter    (meangb = melen  + mcsiz, mek1b  = meangb + mcsiz,
     +              mee1b  = mek1b  + mcsiz, mee2b  = mee1b  + mcsiz,
     +              metltb = mee2b  + mcsiz, mek2b  = metltb + mcsiz,
     +              meh1b  = mek2b  + mcsiz, meh2b  = meh1b  + mcsiz,
     +              megapb = meh2b  + mcsiz, meintb = megapb + mcsiz)
      parameter (meintbx = meintb + mcsiz, mek3b  = meintbx + mcsiz)
*     QUADRUPO.
      parameter    (mek1q  = melen  + mcsiz, metltq = mek1q  + mcsiz)
      integer meaprq
      parameter    (meaprq = metltq + mcsiz)
*     SEXTUPOL.
      parameter    (mek2s  = melen  + mcsiz, metlts = mek2s  + mcsiz)
      integer meaprs
      parameter    (meaprs = metlts + mcsiz)
*     OCTUPOLE.
      parameter    (mek3o  = melen  + mcsiz, metlto = mek3o  + mcsiz)
      parameter    (meapro = metlto + mcsiz)
*     MULTIPOL.
      parameter    (mek0lm = melen  + mcsiz, met0m  = mek0lm + mcsiz,
     +              mek1lm = met0m  + mcsiz, met1m  = mek1lm + mcsiz,
     +              mek2lm = met1m  + mcsiz, met2m  = mek2lm + mcsiz,
     +              mek3lm = met2m  + mcsiz, met3m  = mek3lm + mcsiz,
     +              meaprm = melen  + 21*mcsiz)
*     MULTIPOL.
      parameter    (meklm  = melen  + mcsiz, metltm = meklm  + mcsiz)
*     SOLENOID.
      parameter    (mekss  = melen  + mcsiz, meapss = mekss  + mcsiz)
*     RFCAVITY.
      parameter    (mevltc = melen  + mcsiz, melagc = mevltc + mcsiz,
     +              mefrqc = melagc + mcsiz, mehrmc = mefrqc + mcsiz)
      parameter    (melosc = mehrmc + 5*mcsiz,
     +              meaprc = melosc + 3*mcsiz)
*     ELSEPARA.
      parameter    (meflde = melen  + mcsiz, metlte = meflde + mcsiz)
*     Common to SROT and YROT.
      parameter    (meangr = melen  + mcsiz)
*     Common to KICK, HKICK, and VKICK.
      parameter    (mekick = melen  + mcsiz)
*     Common to ECOLLIMA and RCOLLIMA.
      parameter    (mexcol = melen  + mcsiz, meycol = mexcol + mcsiz)
*     BEAMBEAM.
      parameter    (mesigx = melen  + mcsiz, mesigy = mesigx + mcsiz,
     +              mexma  = mesigy + mcsiz, meyma  = mexma  + mcsiz,
     +              mechg  = meyma  + mcsiz)
*     GBEND.
      parameter    (meangg = melen  + mcsiz, mek1g  = meangg + mcsiz,
     +              mee1g  = mek1g  + mcsiz, mee2g  = mee1g  + mcsiz,
     +              metltg = mee2g  + mcsiz, meksg  = metltg + mcsiz,
     +              meh1g  = meksg  + mcsiz, meh2g  = meh1g  + mcsiz,
     +              megapg = meh2g  + mcsiz, meintg = megapg + mcsiz)
*     lcavity.
      parameter    (mee0l  = melen  + mcsiz, medel  = mee0l  + mcsiz,
     +              mephil = medel  + mcsiz, mefrql = mephil + mcsiz,
     +              melosl = mefrql + mcsiz, mevoll = melosl + mcsiz,
     +              melagl = mevoll + mcsiz, meaprl = melagl + mcsiz)
      parameter (meintgx = meintg + mcsiz)
      integer mlact,mlf1,mlf2,mlfm,mlfree,mlhd,mlnxt,mlprv,mlref,mlrep,
     +mlsiz,mltyp
 
*---- Bias for beam line list information.
      parameter         (mlfm = mbat + 1, mlhd = mbat + 2,
     +                   mlf1 = mbat + 3, mlf2 = mbat + 4,
     +                   mlfree = mbat + 4)
 
*---- Bias for beam line list cells.
      parameter         (mltyp = 1, mlprv = 2, mlnxt = 3, mlrep = 4,
     +                   mlref = 5, mlact = 6, mlsiz = 6)
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer llnact,llnbnk,llncal,llneat,llnedr,llnefl,llnesq,llnhed,
     +llnrls,llnrsq,llnsup,llntmp,llnxls,llnxsq
 
*---- Link area for beam line handler.
      common /lnlink/   llnbnk, llnrls, llnrsq, llnsup,
     +                  llnact, llncal, llnhed, llnxls, llnxsq,
     +                  llnesq, llnedr, llneat, llntmp(4), llnefl
      save              /lnlink/
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      parameter         (half = 0.5d0, eps = 0.6d-6)
      character*(mcnam) label, refnam, seqnam, refdic(3)
      character*(mcnam) elm1, elm2
      double precision l_fact, an
      data numlab       / 0 /
      data refdic       / 'ENTRY', 'EXIT', 'CENTRE' /
 
*---- Decode reference position.
      refnam = 'CENTRE'
      call utgnam(lbank, 1, 1, refnam)
      call utlook(refnam, refdic, 3, iref)
      if (iref .eq. 0) then
        call utleng(refnam, ileng)
        msg(1) = 'Unknown reference keyword "' // refnam(1:ileng)
     +  // '" --- "CENTRE" assumed.'
        call aawarn('LNXSEQ', 1, msg)
        iref = 3
      endif
 
*---- Position for begin of sequence.
      ilst = iq(lbank(1)+mbnam)
      call diname(ldbnk, ilst, seqnam)
      call lnxput(2, ilst, iseq, nseq)
 
*---- Find DRIFT keyword.
      call difind(ldkey, 'DRIFT', idrf, lckey)
      call kwget(lckey, iln, ipr, isp, nkat)
 
*---- Expand contents of sequence.
      ndrift = 0
      llnedr = lq(lbank(1)-1)
      llneat = lbank(1)
      ilast = iq(llnedr+1)
      idata = mbat + mcsiz + 2
      idir  = iq(llnedr+ilast)
      lcelm = lq(ldbnk(3)-idir)
      call diname(ldbnk, idir, elm1)
      pos1 = 0.0
 
*---- Get position difference.
      do 90 ipos = 2, ilast
        level = 0
        nxopr = 0
        call exload(seqnam, ' ', llneat, ipos, idata)
        pos2 = rsval(1)
        if (ipos .gt. 2) then
          call exload(seqnam, ' ', llneat, ipos - 1, idata - mwflt)
          call exbin(2)
        endif
 
*---- Cancel length of previous element.
        if (iref .ne. 2  .and.  iq(lcelm+mbsp) .ne. 8) then
          l_fact = 1.d0
          if (iq(lcelm+mbsp) .eq. 2) then
*--- HG001026: arc length to rectangular bend
            call ucopy(q(lcelm+meangb), an, mwflt)
            if (an .ne. 0.d0 .and. rbarc)
     +      l_fact = 0.5d0 * an / sin(0.5d0 * an)
          endif
          call exload(elm1, 'L', lcelm, 2, melen)
          if (iref .eq. 3) then
            call exfact(0.5d0 * l_fact)
          else
            call exfact(l_fact)
          endif
          call exbin(2)
        endif
 
*---- Cancel length of current element.
        idir = iq(llnedr+ipos)
        lcelm = lq(ldbnk(3)-idir)
        call diname(ldbnk, idir, elm2)
        if (iref .ne. 1  .and.  iq(lcelm+mbsp) .ne. 8) then
          l_fact = 1.d0
          if (iq(lcelm+mbsp) .eq. 2) then
*--- HG001026: arc length to rectangular bend
            call ucopy(q(lcelm+meangb), an, mwflt)
            if (an .ne. 0.d0 .and. rbarc)
     +      l_fact = 0.5d0 * an / sin(0.5d0 * an)
          endif
          call exload(elm2, 'L', lcelm, 2, melen)
          if (iref .eq. 3) then
            call exfact(0.5d0 * l_fact)
          else
            call exfact(l_fact)
          endif
          call exbin(2)
        endif
 
*---- Error, if drift length is negative.
        drflen = rsval(1)
        iexpr  = isval(1) + 1
        if (iexpr .eq. 1) then
          if (drflen .lt. -eps) then
            call utleng(elm1, ilen1)
            call utleng(elm2, ilen2)
            write (msg, 910) drflen, elm1(1:ilen1), pos1,
     +                         elm2(1:ilen2), pos2
  910       format('Negative drift length = ',f12.6,' detected.'/
     +             'preceding element: ',a,' at position ',f18.6/
     +             'following element: ',a,' at position ',f18.6)
            call aafail('LNXSEQ', 3, msg)
          endif
        endif
 
*---- Build drift space and store length.
        if (.not. error) then
          if (abs(drflen) .gt. eps  .or.  iexpr .gt. 1) then
 
*---- For constant length, try finding the same constant length.
            if (iexpr .eq. 1) then
              lccls = lq(lckey-1)
              do 70 idrift = 1, ndrift
                if (lq(lccls-2) .eq. 0) then
                  call ucopy(q(lccls+melen), el, mwflt)
                  if (100.0 * abs(el - drflen) .lt. eps) go to 80
                endif
                lccls = lq(lccls)
   70         continue
            endif
 
*---- Not found; define a new anonymous drift.
            ndrift = ndrift + 1
            call aabook(lccls, 'DRIF', ipr, isp, lckey, 1)
            call exmake(lccls, 2, mbat + mcsiz + mcval, drflen, iexpr)
            iq(lccls+mbat+mcsiz+mctyp) = 10 * mtflt + iexpr
            write (label, '(''['',I6.6,'']        '')') numlab
            numlab = numlab + 1
            call didefi(ldbnk, label, lccls)
 
*---- DEFINE dump option.
            if (ideffl .eq. 1  .or.  ideffl .eq. 3) then
              call aadump(lccls)
            endif
            if (ideffl .eq. 2  .or.  ideffl .eq. 3) then
              call dzshow('element', 0, lccls, 'V', 0, 0, 0, 0)
            endif
 
*---- Append drift space.
   80       continue
            jdir = iq(lccls+mbnam)
            call lnxput(1, jdir, iseq, nseq)
          endif
 
*---- Append current element.
          call lnxput(1, idir, iseq, nseq)
        endif
        elm1 = elm2
        pos1 = pos2
        idata = idata + mwflt
   90 continue
 
*---- Position for end of sequence.
      if (.not. error) call lnxput(3, ilst, iseq, nseq)
 
      end
