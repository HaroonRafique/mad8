      subroutine aaproc
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build procedure for DO or SUBROUTINE command.                      *
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer laabnk,laacur,laadef,laakey,laanew,laaold,laaprc,laasrc,
     +laastk,laatar
 
*---- Local links for control module.
      common /aalink/   laabnk, laadef, laasrc, laatar, laakey, laacur,
     +                  laaprc, laastk, laanew, laaold
      save              /aalink/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer icmd,if1,if2,ileng,ipr,ipush,isp,mpi,ncmd
 
      character*(mcnam) label, key, subnam
      parameter         (mpi = 20)
 
*---- Book procedure bank, reserve one pointer for stack pointer.
      call mzbook(2, laaprc, lccmd, -1, 'PROC', mpi + 1, 0, 2, 2, 0)
      ncmd = 1
 
*---- Command loop.
  100 continue
      error = .false.
 
*---- Read label and keyword.
*     If there is no keyword, LCCMD is set to non-zero.
      call aaread(label, key, if1, if2)
 
*---- Get process code, make sure the command is labelled.
      if (.not. error) then
        ipr = iq(lckey+mbpr)
        isp = iq(lckey+mbsp)
        if (label .eq. ' ') then
          iq(lroot+msrseq) = iq(lroot+msrseq) + 1
          write (label, '(A8,''*'',I6.6,''*'')') key, iq(lroot+msrseq)
        endif
 
*==== Definitions should not occur.
        if (ipr .lt. mpsub) then
          call rdwarn('AAPROC', 1, 'Definition should not occur'
     +    // ' within a subroutine --- it will be executed now.')
 
*==== Subroutine commands.
        else if (ipr .eq. mpsub) then
 
*---- ENDDO, ENDSUBROUTINE or ENDIF.
          if (isp .eq. 2 .or. isp .eq. 6 .or. isp .eq. 9) then
            go to 200
 
*---- CALLSUBROUTINE.
          else if (isp .eq. 7) then
            call aacmnd(label, key)
            call utgnam(lccmd, 1, 1, subnam)
            call difind(ldbnk, subnam, icmd, lccmd)
            call utleng(subnam, ileng)
            msg(1) = '"' // subnam(1:ileng)
     +      // '" is not known as a subroutine.'
            if (lccmd .eq. 0) then
              call rdfail('AAPROC', 1, msg)
            else if (iq(lccmd+mbpr) .ne. mpsub  .or.
     +               iq(lccmd+mbsp) .ne. 5) then
              call rdfail('AAPROC', 1, msg)
            endif
 
*---- STORE and ENDSTORE are invalid.
          else if (isp .eq. 3  .or.  isp .eq. 4) then
            call rdfail('AAPROC', 1, 'STORE or ENDSTORE must not'
     +      // ' occur within DO or SUBROUTINE range.')
 
*---- Nested definition?
          else if (lccmd .eq. 0) then
            call rdfail('AAPROC', 1,
     +      'Nested definition of DO or SUBROUTINE.')
          endif
 
*==== Executable commands.
        else
          call aacmnd(label, key)
        endif
      endif
 
*---- Test for error.
      if (error) then
        if (inter  .and.  iqread .eq. iqttin) then
          msg(1) = '*** Please retype command ***'
          msg(2) = '*** To quit subroutine, type ENDSUB ***'
          call aainfo('AAPROC', 2, msg)
        else if (.not. scan) then
          call aainfo('AAPROC', 1, 'Entering scanning mode.')
          scan = .true.
        endif
 
*---- Link to procedure.
      else
        ncmd = ncmd + 1
        if (ncmd .gt. iq(laaprc-3)) then
          call mzpush(0, laaprc, mpi, 0, 'I')
        endif
        lq(laaprc-ncmd) = lccmd
      endif
      go to 100
 
*---- Drop unused space and restore command pointer.
  200 continue
      ipush = ncmd - iq(laaprc-3)
      call mzpush(0, laaprc, ipush, 0, 'I')
      lccmd = lq(laaprc+1)
 
 9999 end
