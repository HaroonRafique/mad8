      subroutine corkik
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read most recent corrector settings from a TFS file.               *
* GETKICK command:                                                     *
*   FILENAME  (string)  TFS file name to be read.                      *
*   PLANE     (name)    Plane(s) to be read: 'X', 'Y', 'BOTH'.         *
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
      integer mtbact,mtbbky,mtbbuf,mtbcfm,mtbcnm,mtbcol,mtbcps,mtbdsc,
     +mtbf1,mtbf2,mtbfst,mtblst,mtbmod,mtbnam,mtbrow,mtbseg,mtbsiz,
     +mtbsky,mtbwid
 
*---- Parameters for table manager bank structure.
      parameter         (mtbact = 1, mtbbuf = 2, mtbmod = 1)
      parameter         (mtbf1  = 1,
     +                   mtbseg = 2, mtbrow = 3, mtbcol = 4, mtbwid = 5,
     +                   mtbf2  = 6,
     +                   mtbnam = 7, mtbsiz = mtbnam + mwnam - 1)
      parameter         (mtbsky = 2, mtbbky = 3, mtbcnm = 4,
     +                   mtbcfm = 5, mtbcps = 6, mtbdsc = 7,
     +                   mtbfst = 8, mtblst = 9)
      integer ibias1,ibias2,ibias3,idir,iflag,iform1,iform2,iform3,
     +ileng,iocc,ipos,iunit,jbit,jbyt,jrow,nrow
      double precision xkick,ykick
 
      character*(mcfil) filnam
      character*(mcnam) cornam, kiknam, plane, rednam, tabnam
      character*(mcnam) tmpnam
      logical           adding, direct
 
      data tabnam       / '*SETTINGS TABLE*' /
 
*---- Check main beam line.
      call lnchck('GETKICK', error)
      if (error) go to 800
 
*---- Retrieve file name.
      filnam = 'setting'
      call utgstr(lccmd, 1, 1, filnam)
 
*---- Retrieve plane name.
      plane = 'BOTH'
      call utgnam(lccmd, 2, 2, plane)
 
*---- Retrieve increment flag.
      adding = .false.
      call utglog(lccmd, 3, 3, adding)
 
*---- Retrieve "direct name" flag
      direct = .false.
      call utglog(lccmd, 4, 4, direct)
 
*---- Set up corrector and monitor table.
      call cotble(error)
      if (error) go to 800
 
*---- Read table in TFS format.
      call flopen(filnam, 'SRFD', 0, 0, iunit, error)
      if (error) go to 800
      call tbrtfs(tabnam, iunit)
      call flname(iunit, filnam)
      call flclos(iunit, error)
      if (error) go to 800
 
*---- Check table format.
      call tbopen(tabnam, 1, lcotab)
      call tbcol(lcotab, 'STR_NAME', iform1, ibias1)
      if (iform1 .ne. 5) then
        write (msg, 910) 'STR_NAME', 'string'
        call aafail('CORKIK', 1, msg)
      endif
      if (plane .eq. 'X') then
        call tbcol(lcotab, 'DK_N', iform2, ibias2)
        if (iform2 .lt. 3  .or.  iform2 .gt. 4) then
          write (msg, 910) 'DK_N', 'real'
          call aafail('CORKIK', 1, msg)
        endif
        iform3 = 0
      else if (plane .eq. 'Y') then
        iform2 = 0
        call tbcol(lcotab, 'DK_N', iform3, ibias3)
        if (iform3 .lt. 3  .or.  iform3 .gt. 4) then
          write (msg, 910) 'DK_N', 'real'
          call aafail('CORKIK', 1, msg)
        endif
      else
        call tbcol(lcotab, 'DK_N_H', iform2, ibias2)
        if (iform2 .lt. 3  .or.  iform2 .gt. 4) then
          write (msg, 910) 'DK_N_H', 'real'
          call aafail('CORKIK', 1, msg)
        endif
        call tbcol(lcotab, 'DK_N_V', iform3, ibias3)
        if (iform3 .lt. 3  .or.  iform3 .gt. 4) then
          write (msg, 910) 'DK_N_V', 'real'
          call aafail('CORKIK', 1, msg)
        endif
      endif
      if (error) go to 800
 
*---- Set all correctors to zero increment.
      if (.not. adding) then
        lccom = lq(lcseq-mscor)
   10   if (lccom .ne. 0) then
          if (plane .eq. 'X') then
            call sbit0(iq(lccom), 3)
            call uzero(q(lccom+1), 1, mwflt)
          else if (plane .eq. 'Y') then
            call sbit0(iq(lccom), 4)
            call uzero(q(lccom+mwflt+1), 1, mwflt)
          else
            call sbyt(0, iq(lccom), 3, 2)
            call uzero(q(lccom+1), 1, 2*mwflt)
          endif
          lccom = lq(lccom-1)
          go to 10
        endif
      endif
 
*---- Drop precomputed maps.
      newmap = .true.
 
*---- Load corrector settings into data structure.
      nrow = iq(lcotab+mtbrow)
      do 90 jrow = 1, nrow
        call tbset(lcotab, jrow, 1, lcobuf)
        call uhtoc(q(lcobuf+ibias1+1), mcwrd, rednam, mcnam)
 
*---- Search corrector table for this name.
        lccom = lq(lcseq-mscor)
   20   if (lccom .ne. 0) then
 
*---- Fetch corrector name.
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
     +      iq(lcexp+mxop) .ne. -2  .or.  lq(lcexp-1) .eq. 0) then
            call utocnm(cornam, iocc, kiknam)
          else
            call uhtoc(q(lq(lcexp-1)+mvbank), mcwrd, kiknam, mcnam)
          endif
 
*---- If same name, store settings.
          if (direct)  then
            tmpnam = cornam
          else
            tmpnam = kiknam
          endif
          if (tmpnam .eq. rednam) then
            call ucopy(q(lccom+1), xcm, 2*mwflt)
            if (jbit(iq(lccom),1) .ne. 0) then
              if (iform2 .eq. 3) then
                xkick = q(lcobuf+ibias2+1)
              else if (iform2 .eq. 4) then
                call ucopy(q(lcobuf+ibias2+1), xkick, mwflt)
              endif
              xcm = xcm + xkick
            endif
            if (jbit(iq(lccom),2) .ne. 0) then
              if (iform3 .eq. 3) then
                ykick = q(lcobuf+ibias3+1)
              else if (iform3 .eq. 4) then
                call ucopy(q(lcobuf+ibias3+1), ykick, mwflt)
              endif
              ycm = ycm + ykick
            endif
 
*---- Activate corrector, set used, and store data.
            call sbyt(iq(lccom), iq(lccom), 5, 2)
            call sbyt(iq(lccom), iq(lccom), 3, 2)
            call ucopy(xcm, q(lccom+1), 2*mwflt)
            go to 90
          endif
 
*---- Skip to next corrector.
          lccom = lq(lccom-1)
          go to 20
        endif
 
*---- Failing to find name in table.
        call utleng(rednam, ileng)
        msg(1) = 'Corrector "' // rednam(1:ileng)
     +  // '" not found in beam line.'
        call aawarn('CORKIK', 1, msg)
   90 continue
 
*---- Drop table.
      msg(1) = 'Kicker settings read on file: ' // filnam
      call aainfo('CORKIK', 1, msg)
  800 continue
      if (lcotab .ne. 0) call tbdrop(lcotab)
 
  910 format('Cannot find column "',a,'" with ',a,' format.')
 
      end
