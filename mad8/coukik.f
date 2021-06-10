      subroutine coukik
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Activate/deactivate selected correctors.                           *
* USEKICK command:                                                     *
*   STATUS    (logical) Flag to activate or deactivate.                *
*   MODE      (name)    May be ALL, USED, UNUSED, or blank.            *
*   RANGE     (range)   Range to limit correctors to be changed.       *
*   CLASS     (name)    Class of elements to be affected in RANGE.     *
*   PATTERN   (string)  Regular expression to limit choice.            *
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer imode,istat,jbit,jpl,l,mclass,mmode,mpatt,mrange,mstat
 
      parameter         (mstat  = 1, mmode  = 2)
      parameter         (mrange = 3, mclass = 4, mpatt  = 5)
 
      external          cofkik
      character*(mcnam) mode, dict(4), class
      character*(mcstr) patt
      character*1       plane(2)
      integer           ncount(2)
      logical           done, status
 
      data dict         / 'ALL', 'USED', 'UNUSED', ' ' /
      data plane        / 'X', 'Y' /
 
*---- Check main beam line.
      call lnchck('COUKIK', error)
      if (error) go to 9999
 
*---- Get data for range.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Set up corrector and monitor table.
      call cotble(error)
      if (error) go to 9999
 
*---- Fetch option flags.
      status = .false.
      mode = ' '
      call utglog(lccmd, mstat, mstat, status)
      call utgnam(lccmd, mmode, mmode, mode)
 
*---- Decode MODE attribute.
      call utlook(mode, dict, 4, imode)
      if (imode .eq. 0) then
        call utleng(mode, l)
        call aawarn('COUKIK', 1,
     +    'Unknown value "MODE = ' // mode(1:l) // '" ignored.')
      endif
 
*---- Encode flags to arguments for COFKIK.
      istat = 0
      if (status) istat = 1
 
*---- Clear counts of correctors affected.
      ncount(1) = 0
      ncount(2) = 0
 
*---- "MODE=ALL" affects all correctors for this plane.
      if (imode .eq. 1) then
        do 40 jpl = 1, 2
          lccom  = lq(lcseq-mscor)
   30     if (lccom .ne. 0) then
            if (jbit(iq(lccom),jpl) .ne. 0) then
              call sbit(istat, iq(lccom), jpl+4)
              ncount(jpl) = ncount(jpl) + 1
            endif
            lccom = lq(lccom-1)
            go to 30
          endif
   40   continue
 
*---- "MODE=USED" option affects all correctors used for this plane.
      else if (imode .eq. 2) then
        do 60 jpl = 1, 2
          lccom = lq(lcseq-mscor)
   50     if (lccom .ne. 0) then
            if (jbit(iq(lccom),jpl)   .ne. 0  .and.
     +          jbit(iq(lccom),jpl+2) .ne. 0) then
              call sbit(istat, iq(lccom), jpl+4)
              ncount(jpl) = ncount(jpl) + 1
            endif
            lccom = lq(lccom-1)
            go to 50
          endif
   60   continue
 
*---- "MODE=UNUSED" affects all correctors not used for this plane.
      else if (imode .eq. 3) then
        do 80 jpl = 1, 2
          lccom = lq(lcseq-mscor)
   70     if (lccom .ne. 0) then
            if (jbit(iq(lccom), jpl)   .ne. 0  .and.
     +          jbit(iq(lccom), jpl+2) .eq. 0) then
              call sbit(istat, iq(lccom), jpl+4)
              ncount(jpl) = ncount(jpl) + 1
            endif
            lccom = lq(lccom-1)
            go to 70
          endif
   80   continue
 
*---- Use RANGE, CLASS and PATTERN.
      else
        class = ' '
        patt  = ' '
        call utgnam(lccmd, mclass, mclass, class)
        call utgstr(lccmd, mpatt,  mpatt,  patt)
        lcatt = lq(lccmd-mrange)
        call ensrng(lcatt, class, patt, cofkik, istat, ncount, done)
      endif
 
*---- Write message on number of correctors affected.
      do 120 jpl = 1, 2
        if (status) then
          write (msg(jpl), 910) ncount(jpl), plane(jpl)
        else
          write (msg(jpl), 920) ncount(jpl), plane(jpl)
        endif
  120 continue
      call aainfo('COUKIK', 2, msg)
 
  910 format(i8,' corrector(s) activated for plane ',a1,'.')
  920 format(i8,' corrector(s) deactivated for plane ',a1,'.')
 
 9999 end
