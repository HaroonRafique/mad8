      subroutine svslin
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build beam line from range selected in USE command.                *
* Input:                                                               *
*   LBANK(1)  (pointer) Pointer to data bank.                          *
*   ILINK     (integer) Attribute number.                              *
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
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
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
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer idir,idrft,iline,ipos,ipos1,ipos2,jbit,jdir,l,maxnum,
     +mxuse,ndrft,nline
      double precision rval,tol
 
      parameter         (maxnum = 250, mxuse = 10)
      parameter         (tol = 1.0d-8)
 
      character*(mcnam) elmnam
      character*(mcfil) filnam
      logical           seen
 
*---- Check main beam line.
      call lnchck('SAVELINE', error)
      if (error) go to 9999
 
*---- Retrieve beam line descripbbtion.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Name of line (default is main line name).
      call utgnam(lccmd, 1, 1, linnam)
 
*---- File name.
      filnam = 'line'
      call utgstr(lccmd, 2, 2, filnam)
      call flopen(filnam, 'SWFD', 0, 0, isave, error)
      if (.not. error) then
 
*---- Reserve working storage for drift lengths.
        nwork = 100
        call mzwork(0, dq(1), dq(nwork+1), -1)
 
*---- Phase 1: Save beam line by pieces.
        write (isave, '(''! Snapshot date: '',A,'' Time: '',A/1X)')
     +    cdate, ctime
        write (isave, '(''! Beam lines from SAVELINE command:''/1X)')
        nline = 0
        ndrft = 0
 
        do 50 ipos1 = irg1, irg2, maxnum
          nline = nline + 1
          write (elmnam, '(''L'',I6.6)') nline
          call svbegn
          call svname(elmnam)
          call svlitt(': LINE = (')
          ipos2 = min(ipos1+maxnum-1,irg2)
          seen = .false.
          do 40 ipos = ipos1, ipos2
            idir = iq(lsdir+ipos)
            lcelm = lq(ldbnk(3)-idir)
            if (iq(lcelm+mbpr) .eq. mpelm) then
              call diname(ldbnk, iq(lcelm+mbnam), elmnam)
 
*---- Not a generated drift space.
              if (elmnam(1:1).ne.'[' .or. elmnam(8:8).ne.']') then
                if (seen) call svlitt(', ')
                seen = .true.
                call svname(elmnam)
*---- Mark all superclasses of this element for output.
   10           if (lcelm .ne. 0) then
                  call sbit1(iq(lcelm), mxuse)
                  lcelm = lq(lcelm)
                  go to 10
                endif
 
*---- A generated drift space.
*     Set up table of different lengths.
              else
                call ucopy(q(lcelm+melen), rval, mwflt)
                if (abs(rval) .gt. tol) then
                  do 20 idrft = 1, ndrft
                    if (abs(dq(idrft) - rval) .lt. tol) go to 30
   20             continue
 
*---- No equal drift found.
                  ndrft = ndrft + 1
                  idrft = ndrft
                  if (ndrft .gt. nwork) then
                    nwork = nwork + 100
                    call mzwork(0, dq(1), dq(nwork+1), -1)
                  endif
                  dq(ndrft) = rval
 
*---- Output the drift number.
   30             continue
                  write (elmnam, '(''D'',I6.6)') idrft
                  if (seen) call svlitt(', ')
                  seen = .true.
                  call svname(elmnam)
                endif
              endif
            endif
   40     continue
          call svlitt(')')
          call svdump
          write (isave, '(1X)')
   50   continue
 
*---- Phase 2: Combine sublines.
        call svbegn
        call svname(linnam)
        call svlitt(': LINE = (')
        do 60 iline = 1, nline
          write (elmnam, '(''L'',I6.6)') iline
          call svname(elmnam)
          if (iline .lt. nline) call svlitt(',')
   60   continue
        call svlitt(')')
        call svdump
 
*---- Phase 3: Save all referred element definitions,
*     excluding generated drift spaces.
        write (isave, '(1X/''! Element definitions:''/1X)')
        do 70 idir = iq(ldbnk(3)+3) + 1, iq(ldbnk(3)+1)
          lcelm = lq(ldbnk(3)-idir)
          if (lcelm .ne. 0  .and.
     +        iq(lcelm+mbpr) .eq. mpelm  .and.
     +        iq(lcelm+mbsp) .ne. 1  .or.
     +        jbit(iq(lcelm),mxuse) .ne. 0) then
            jdir = iq(lcelm+mbnam)
            iq(lcelm+mbnam) = idir
            call svsnap(lcelm)
            iq(lcelm+mbnam) = jdir
            call sbit0(iq(lcelm),mxuse)
          endif
   70   continue
 
*---- Phase 4: Write all generated drift spaces.
        write (isave, '(1X/''! Generated drifts:''/1X)')
        do 80 idrft = 1, ndrft
          write (isave, '(''D'',I6.6,'': DRIFT, L = '',F12.6)')
     +      idrft, dq(idrft)
   80   continue
 
*---- Close output file.
        write (isave, '(1X/''RETURN'')')
        call flclos(isave, error)
        if (.not. error) then
          call flname(isave, filnam)
          call utleng(linnam, l)
          call aainfo('SVSLIN', 1, 'Beam line "' // linnam(1:l)
     +      // '" and definitions written on file: ' // filnam)
        endif
 
*---- Release working storage.
        iwork = 0
        nwork = 0
        call mzwork(0, dq(1), dq(1), -1)
      endif
 
 9999 end
