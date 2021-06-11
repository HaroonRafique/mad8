      subroutine aasubr(label, key)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode (and execute) subroutine command.                           *
*   Uses the pointers to current keyword and current command.          *
* Input:                                                               *
*   LABEL    (char)     Name of subroutine bank.                       *
*   KEY      (char)     Name of keyword bank.                          *
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
      integer idir,ileng,iln,ipr,isp,nkat
      character*(mcnam) label, key
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
 
      character*(mcnam) subnam
      logical           eflag
 
*---- Command called by label only. Decode attributes.
      if (lccmd .ne. 0) then
        call aaattr(lq(lckey-2), lccmd, nkat, error)
        if (.not. error) then
          isp = iq(lccmd+mbsp)
 
*---- DO, SUBROUTINE or IF.
          if (isp .eq. 1 .or. isp .eq. 5 .or. isp .eq. 8) then
            call aaruns
 
*---- CALLSUBROUTINE.
          else if (isp .eq. 7) then
            call utgnam(lccmd, 1, 1, subnam)
            call utleng(subnam, ileng)
            call difind(ldbnk, subnam(1:ileng), idir, lccmd)
            msg(1) = '"' // subnam(1:ileng)
     +      // '" is not known as a subroutine.'
            if (lccmd .eq. 0) then
              call rdfail('AASUBR', 1, msg)
            else if (iq(lccmd+mbpr) .ne. mpsub  .or.
     +               iq(lccmd+mbsp) .ne. 5) then
              call rdfail('AASUBR', 1, msg)
            else
              call aaruns
            endif
 
*---- Other subroutine commands not allowed here.
          else
            call rdfail('AASUBR', 1, 'Subroutine command out of order.')
          endif
        endif
 
*---- Lift command bank and decode attributes.
      else
        call kwget(lckey, iln, ipr, isp, nkat)
        call aabook(lccmd, label, ipr, isp, lckey, 1)
!        lq(lckey-2) = lq(lckey-2)
        call aaattr(lq(lckey-2), lccmd, nkat, eflag)
 
*---- If error detected, drop command bank, else link it to directory.
        if (eflag) then
          call aadrop(lccmd)
        else
          call didefi(ldbnk, label, lccmd)
 
*---- ISP = 1 or 8: Build procedure bank for DO or IF and execute it.
          if (isp .eq. 1 .or. isp .eq. 8) then
            iq(lccmd+mbat+mcval) = iq(lccmd+mbat+mcval+mcsiz)
            call aaproc
 
*---- Execute DO or IF only when no LABEL is present.
            if (label .eq. ' ') then
              call aaruns
            endif
 
*---- ISP = 5: Build procedure bank for SUBROUTINE.
          else if (isp .eq. 5) then
            iq(lccmd+mbat+mcval) = 1
            call aaproc
 
*---- ISP = 7: Execute CALLSUBROUTINE.
          else if (isp .eq. 7) then
            call utgnam(lccmd, 1, 1, subnam)
            call utleng(subnam, ileng)
            call difind(ldbnk, subnam(1:ileng), idir, lccmd)
            msg(1) = '"' // subnam(1:ileng)
     +      // '" is not known as a subroutine.'
            if (lccmd .eq. 0) then
              call rdfail('AASUBR', 1, msg)
            else if (iq(lccmd+mbpr) .ne. mpsub  .or.
     +               iq(lccmd+mbsp) .ne. 5) then
              call rdfail('AASUBR', 1, msg)
            else
              call aaruns
            endif
          endif
        endif
      endif
 
      end
