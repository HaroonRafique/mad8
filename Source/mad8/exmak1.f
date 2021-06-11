      subroutine exmak1(ltab, lbnk, ilink)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate bank for table column expression.                         *
*   Constant or deferred expression is not permitted.                  *
* Input:                                                               *
*   LTAB(1)  (pointer)  Pointer to table being listed.                 *
*   LBNK(1)  (pointer)  Supporting bank for expression bank.           *
*   ILINK    (integer)  Supporting link in bank LBNK (like MZBOOK).    *
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
      integer ibias,icat,idir,ikat,ileng,ilink,iln,ind1,ind2,ind3,ipr,
     +isp,jbias,jform,jk,jleng,jx,nd,nkat,nl
      integer           ltab(1), lbnk(1)
 
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
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
*---- Build expression bank. No space reserved for store operation.
      lexbnk = lbnk(1)
      nl = nxopr
      nd = nxopr * mxsiz
      call mzbook(2, lexexp, lexbnk, ilink, 'EXPR', nl, nl, nd, 7, 0)
 
*---- Fill in operations.
      ibias = 0
      do 90 jx = 1, nxopr
 
*---- Bank format and operation code.
        iq(lexexp+ibias+mxf1) = 16 * 1 + 2
        iq(lexexp+ibias+mxop) = ixopr(jx)
        iq(lexexp+ibias+mxf2) = 16 * mwflt + 2
 
*---- Load constant: Store constant value.
        if (ixopr(jx) .eq. -1) then
          iq(lexexp+ibias+mxf2) = 16 * mwflt + mreal
          call ucopy(rxval(jx), q(lexexp+ibias+mxval), mwflt)
 
*---- Load variable.
        else if (ixopr(jx) .le. -2) then
 
*---- Look up variable in table columns.
          if (ixopr(jx) .eq. -2) then
            call tbcol(ltab, axbank(jx), jform, jbias)
            if (jform .ge. 2  .and.  jform .le. 4) then
              ixopr(jx) = - (jform + 3)
 
*---- Look up variable in table descriptors.
            else
              call tbqdsc(ltab, axbank(jx), jform)
              if (jform .ge. 2  .and.  jform .le. 4) then
                ixopr(jx) = -8
              endif
            endif
            iq(lexexp+ibias+mxop) = ixopr(jx)
          endif
 
*---- Load variable from table descriptor.
          if (ixopr(jx) .eq. -8) then
            call mzbook(2,lexvar,lexexp,-jx,'GTBD',0,0,mwnam,5,0)
            call uctoh(axbank(jx), iq(lexvar+1), mcwrd, mcnam)
 
*---- Load variable from table column.
          else if (ixopr(jx) .le. -5) then
            iq(lexexp+ibias+mxval) = jbias
 
*---- Book variable reference bank and fill in.
          else
            call mzbook(2,lexvar,lexexp,-jx,'VREF',1,0,mvsiz,7,0)
            iq(lexvar+mvf1) = 16 * 2 * mwnam + 5
            call uctoh(axbank(jx), iq(lexvar+mvbank), mcwrd, mcnam)
            call uctoh(axattr(jx), iq(lexvar+mvattr), mcwrd, mcnam)
            iq(lexvar+mvf2) = 16 * 6 + 2
            iq(lexvar+mvind1) = ixsub1(jx)
            iq(lexvar+mvind2) = ixsub2(jx)
            iq(lexvar+mvind3) = ixsub3(jx)
 
*---- Find bank referred to.
            call utleng(axbank(jx), ileng)
            call utleng(axattr(jx), jleng)
            call difind(ldbnk, axbank(jx)(1:ileng), idir, lexbnk)
            lq(lexvar-1) = lexbnk
            if (lexbnk .eq. 0) then
              write (msg, 910) axbank(jx)(1:ileng)
              call aafail('EXMAK1', 1, msg)
 
*---- Parameter: Check.
            else
              if (ixopr(jx) .eq. -2) then
                if (iq(lexbnk+mbpr) .ne. mppar) then
                  write (msg, 920) axbank(jx)(1:ileng)
                  call aafail('EXMAK1', 1, msg)
                else
                  iq(lexvar+mvbias) = 1
                  iq(lexvar+mvseen) = 1
                endif
 
*---- Bank attribute: Fill in bias.
              else
                call kwget(lq(lexbnk+1), iln, ipr, isp, nkat)
                call utleng(axattr(jx), ileng)
                call utlook(axattr(jx)(1:ileng), katnam, nkat, ikat)
                ind1 = ixsub1(jx)
                ind2 = ixsub2(jx)
                ind3 = ixsub3(jx)
                if (ikat .eq. 0) then
                  write (msg, 930) axbank(jx)(1:ileng),
     +              axattr(jx)(1:jleng)
                  call aafail('EXMAK1', 1, msg)
                else if (iatype(ikat) .ne. mtflt) then
                  write (msg, 940) axbank(jx)(1:ileng),
     +              axattr(jx)(1:jleng)
                  call aafail('EXMAK1', 1, msg)
                else if (ind1 .gt. iadim1(ikat)  .or.
     +                   ind2 .gt. iadim2(ikat)  .or.
     +                   ind3 .gt. iadim3(ikat)) then
                  write (msg, 950) axbank(jx)(1:ileng),
     +              axattr(jx)(1:jleng), ind1, ind2, ind3
                  call aafail('EXMAK1', 1, msg)
                else
                  icat = 0
                  do 10 jk = 1, ikat
                    icat = icat + iadim1(jk) * iadim2(jk) * iadim3(jk)
   10             continue
                  iq(lexvar+mvbias) = icat + (ind1 - iadim1(ikat)) +
     +              (ind2 - iadim2(ikat)) * iadim1(ikat) +
     +              (ind3 - iadim3(ikat)) * iadim1(ikat) * iadim2(ikat)
                  iq(lexvar+mvseen) = 2
                endif
              endif
            endif
          endif
        endif
        ibias = ibias + mxsiz
   90 continue
 
*---- Dump expression.
      if (iexpfl .eq. 1  .or.  iexpfl .eq. 3) then
        call exdump(lexexp)
      endif
      if (iexpfl .eq. 2  .or.  iexpfl .eq. 3) then
        call dzshow('expression', 0, lexexp, 'V', 0, 0, 0, 0)
      endif
 
  910 format('Expression refers to name "',a,'" --- unable to find.')
  920 format('Expression refers to name "',a,'" --- not a parameter.')
  930 format('Expression refers to attribute "',a,'[',a,
     +       '] --- unable to find.')
  940 format('Expression refers to attribute "',a,'[',a,
     +       '] --- not of real type.')
  950 format('Expression refers to aAttribute "',a,'[',a,'(',i5,',',i5,
     +       ',',i5,')] --- index out of range.')
 
      end
