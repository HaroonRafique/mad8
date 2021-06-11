      subroutine cowkik
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Write most recent corrector settings to a TFS file.                *
* PUTKICK command:                                                     *
*   FILENAME  (string)  TFS file name to be written.                   *
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
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      integer lcobuf,lcocor,lcoelm,lcomon,lcotab
 
*---- Links for closed orbit correction module.
      common /colink/   lcotab, lcobuf, lcocor, lcomon, lcoelm
      save              /colink/
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
      integer idir,iflag,ileng,iocc,ipos,iunit,jbit,jbyt,jpl,jrow,ncol,
     +nrow
      double precision zero
 
      character*(mcfil) filnam
      character*(mcnam) cornam, kiknam, tabnam, cnam(3)
      character*(mcnam) tmpnam
      character*(mcnam) plane, pnam(3)
      integer           icfrm(3)
      logical direct
 
      data pnam         / 'X', 'Y', 'BOTH' /
      data tabnam       / '*SETTINGS TABLE*' /
 
*---- Check main beam line.
      call lnchck('PUTKICK', error)
      if (error) go to 9999
 
*---- Retrieve file name.
      filnam = 'setting'
      call utgstr(lccmd, 1, 1, filnam)
 
*---- Retrieve desired plane.
      plane = 'BOTH'
      call utgnam(lccmd, 2, 2, plane)
      call utleng(plane, ileng)
      call utlook(plane(1:ileng), pnam, 3, jpl)
      if (jpl .eq. 0) then
        call aawarn('COWKIK', 1, 'Unknown plane code "' //
     +    plane(1:ileng) // '", "BOTH" assumed.')
        jpl = 3
      endif
 
*---- Retrieve "direct name" flag
      direct = .false.
      call utglog(lccmd, 3, 3, direct)
 
*---- Check presence of corrector and monitor table.
      if (lq(lcseq-mscom) .eq. 0) then
        call aawarn('COWKIK', 1, 'Kicker settings not yet available.')
 
*---- Build internal table.
      else
        nrow  = 0
        lccom = lq(lcseq-mscor)
   10   if (lccom .ne. 0) then
          nrow  = nrow + 1
          lccom = lq(lccom-1)
          go to 10
        endif
 
        cnam(1) = 'STR_NAME'
        icfrm(1) = 5
        icfrm(2) = 3
        if (double) icfrm(2) = mreal
        if (jpl .eq. 3) then
          cnam(2) = 'DK_N_H'
          cnam(3) = 'DK_N_V'
          ncol = 3
          icfrm(3) = icfrm(2)
        else
          cnam(2) = 'DK_N'
          ncol = 2
        endif
        call tbcrea(tabnam, 1, nrow, ncol, cnam, icfrm, 1, lcotab)
        if (jpl .eq. 1) then
          call tbpdsc(lcotab, 'TYPE', 5, 0, zero, 'H_SETTING')
        else if (jpl .eq. 2) then
          call tbpdsc(lcotab, 'TYPE', 5, 0, zero, 'V_SETTING')
        else
          call tbpdsc(lcotab, 'TYPE', 5, 0, zero, 'SETTING')
        endif
 
*---- Loop on all correctors.
        lccom = lq(lcseq-mscor)
        do 90 jrow = 1, nrow
          call ucopy(q(lccom+1), xcm, iq(lccom-1))
          if (jpl .eq. 2  .or.  jbit(iq(lccom),3) .eq. 0) xcm = 0.0
          if (jpl .eq. 1  .or.  jbit(iq(lccom),4) .eq. 0) ycm = 0.0
 
*---- If non-zero corrector strength, build buffer.
          if (xcm .ne. 0.0  .or.  ycm .ne. 0.0) then
            ipos  = iq(lccom-5)
            idir  = iq(lq(lcseq-msdir)+ipos)
            iflag = iq(lq(lcseq-msflg)+ipos)
            iocc  = jbyt(iflag,mocc1,mocc2)
            call diname(ldbnk, idir, cornam)
            lcelm = lq(ldbnk(3)-idir)
            lcexp = lq(lcelm-3)
            if (lcexp .eq. 0) then
              call utocnm(cornam, iocc, kiknam)
            else if (iq(lcexp-2) .ne. 1  .or.
     +        iq(lcexp+mxop) .ne. -2  .or.  lq(lcexp-1) .eq. 0) then
              call utocnm(cornam, iocc, kiknam)
            else
              call uhtoc(q(lq(lcexp-1)+mvbank), mcwrd, kiknam, mcnam)
            endif
 
*---- Store to table.
            call tbset(lcotab, jrow, 3, lcobuf)
            if (direct) then
              tmpnam = cornam
            else
              tmpnam = kiknam
            endif
            call uctoh(tmpnam, iq(lcobuf+1), mcwrd, mcnam)
            if (jpl .eq. 1) then
              if (double) then
                call ucopy(xcm, q(lcobuf+mwnam+1), mwflt)
              else
                q(lcobuf+mwnam+1) = xcm
              endif
            else if (jpl .eq. 2) then
              if (double) then
                call ucopy(ycm, q(lcobuf+mwnam+1), mwflt)
              else
                q(lcobuf+mwnam+1) = ycm
              endif
            else
              if (double) then
                call ucopy(xcm, q(lcobuf+mwnam+1), 2*mwflt)
              else
                q(lcobuf+mwnam+1) = xcm
                q(lcobuf+mwnam+2) = ycm
              endif
            endif
          endif
          lccom = lq(lccom-1)
   90   continue
        call tbclos(lcotab)
 
*---- Write table in TFS format.
        call flopen(filnam, 'SWFD', 0, 0, iunit, error)
        if (.not. error) then
          call tbwtfs(tabnam, iunit)
          call flname(iunit, filnam)
          call flclos(iunit, error)
          if (.not. error) then
            msg(1) = 'Kicker settings written on file: ' // filnam
            call aainfo('COWKIK', 1, msg)
          endif
        endif
 
*---- Drop table.
        call tbopen(tabnam, 0, lcotab)
        call tbdrop(lcotab)
      endif
 
 9999 end
