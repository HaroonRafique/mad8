      subroutine lnmain(label, if1, if2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode beam line definitions.                                      *
* Input:                                                               *
*   LABEL    (char)     Label for new definition (required).           *
*   IF1      (integer)  First character of formals list (optional).    *
*   IF2      (integer)  Last character of formals list.                *
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
      integer if1,if2,iln,ipr,isp,nkat
      character*(mcnam) label
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
 
      ipr = iq(lckey+mbpr)
      isp = iq(lckey+mbsp)
 
*==== Line and sequence definition commands.
      if (isp .le. 10) then
 
*---- ISP = 1, Beam line definition.
        if (isp .eq. 1) then
          call lnbeam(label, if1, if2)
 
*---- ISP = 2, Sequence definition.
        else if (isp .eq. 2) then
          call lnseq(label)
 
*---- ISP = 3, Replacement list definition.
        else if (isp .eq. 3) then
          call lnlist(label)
 
*---- Other.
        else
          call userdf(ipr, isp)
        endif
 
*---- LINE dump option.
        if (.not. error) then
          if (ilinfl .eq. 1  .or.  ilinfl .eq. 3) then
           call lndump(lccmd, label)
          endif
          if (ilinfl .eq. 2  .or.  ilinfl .eq. 3) then
            call dzshow('line', 0, lccmd, 'V', 0, 0, 0, 0)
          endif
        endif
 
*==== Sequence editor commands; skip in scanning mode
      else if (.not. scan) then
 
*---- Copy keyword to local storage.
        call kwget(lckey, iln, ipr, isp, nkat)
 
*---- Build new command bank (no defaults).
        call aabook(lccmd, label, ipr, isp, lckey, 1)
 
*---- Decode attributes.
        if (isp .eq. 12) nkat = - nkat
        call aaattr(lq(lckey-2), lccmd, nkat, error)
        if (error) go to 9999
 
*---- Fill in variable references.
        call exfill
        if (.not. error) then
 
*---- Order and evaluate expressions.
          call exordr
          if (.not. error) then
            call exupdt
          endif
        endif
 
*---- ISP = 11, SEQEDIT.
        if (isp .eq. 11) then
          if (imodul .ne. 0) then
            msg(1) = 'Cannot initiate sequence editor while previous '
     +               // 'process is not complete,'
            msg(2) = 'Enter proper ENDxxxx command first.'
            call aafail('LNMAIN', 2, msg)
          else
            call lnebgn
            if (.not. error) imodul = ipr
          endif
 
*---- Check valid use of sequence editor subcommand.
        else if (imodul .ne. ipr) then
          msg(1) =
     +    'Cannot run editor subcommand outside sequence editor,'
          msg(2) = 'SEQEDIT command required first.'
          call aafail('LNEDIT', 2, msg)
 
*---- ISP = 12, INSTALL.
        else if (isp .eq. 12) then
          call lneins
 
*---- ISP = 13, MOVE.
        else if (isp .eq. 13) then
          call lnemov
 
*---- ISP = 14, REMOVE.
        else if (isp .eq. 14) then
          call lnerem
 
*---- ISP = 15, CYCLE.
        else if (isp .eq. 15) then
          call lnecyc
 
*---- ISP = 16, REFLECT.
        else if (isp .eq. 16) then
          call lneref
 
*---- ISP = 17, ENDEDIT.
        else if (isp .eq. 17) then
 
*---- Drop selection bank.
          llnefl = lq(lq(llnesq-1)-1)
          if (llnefl .ne. 0) call mzdrop(0, llnefl, ' ')
          llnesq = 0
          imodul = 0
 
*---- LINE dump option.
          if (.not. error) then
            if (ilinfl .eq. 1  .or.  ilinfl .eq. 3) then
             call lndump(lccmd, label)
            endif
            if (ilinfl .eq. 2  .or.  ilinfl .eq. 3) then
              call dzshow('seqedit', 0, lccmd, 'V', 0, 0, 0, 0)
            endif
          endif
 
*---- ISP = 18, REPLACE.
        else if (isp .eq. 18) then
          call lnerep
 
*---- Other.
        else
          call userdf(ipr, isp)
        endif
      endif
 
 9999 end
