      subroutine aasmod(idir, lold, lnew)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Perform actions required for a modification of the main beam line. *
*   Reject redefinition of a class; drop data which are outdated, etc. *
* Input:                                                               *
*   IDIR     (integer)  Directory index of old element.                *
*   LOLD(1)  (pointer)  Pointer to old bank.                           *
*   LNEW(1)  (pointer)  Pointer to new bank.                           *
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
      integer idir,ileng,ipos,isp,jsp,lines
      integer           lold(*), lnew(*)
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
 
      character*(mcnam) label
      logical           found, drpali, drpfld, drpspl
 
*---- Get label of element.
      laaold = lold(1)
      laanew = lnew(1)
      call diname(ldbnk, idir, label)
      call utleng(label, ileng)
      lines = 1
      msg(1) = 'Replacing name "' // label(1:ileng) // '",'
 
*==== Replacement element --> element.
      if (iq(laaold+mbpr) .eq. mpelm  .and.
     +         iq(laanew+mbpr) .eq. mpelm) then
 
*---- Find occurrences of exact name in main beam line.
*     If element type changes, drop error definitions on this element.
        found = .false.
        drpali = .false.
        drpfld = .false.
        drpspl = .false.
        isp = iq(laanew+mbsp)
        jsp = iq(laaold+mbsp)
        if (lcseq .ne. 0) then
          lsdir = lq(lcseq-msdir)
          lsali = lq(lcseq-msali)
          lsspl = lq(lcseq-msspl)
          lsfld = lq(lcseq-msfld)
          do 30 ipos = 1, iq(lsdir-1)
            if (iq(lsdir+ipos) .eq. idir) then
              found = .true.
              if (isp .ne. jsp) then
                if (lsali .ne. 0) then
                  lcali = lq(lsali-ipos)
                  if (lcali .ne. 0) then
                    call mzdrop(0, lcali, '.')
                    drpali = .true.
                  endif
                endif
                if (lsfld .ne. 0) then
                  lcfld = lq(lsfld-ipos)
                  if (lcfld .ne. 0) then
                    call mzdrop(0, lcfld, '.')
                    drpfld = .true.
                  endif
                endif
                if (lsspl .ne. 0) then
                  lcspl = lq(lsspl-ipos)
                  if (lcspl .ne. 0) then
                    call mzdrop(0, lcspl, '.')
                    drpspl = .true.
                  endif
                endif
              endif
            endif
   30     continue
        endif
 
*---- If replacement involves monitor or corrector, drop MICADO tables.
        if (found) then
          call lnmark('AASMOD')
          lines = lines + 1
          msg(lines) = 'All occurrences replaced in main beam line,'
          if (isp .ne. jsp  .and.  lq(lcseq-mscom) .ne. 0) then
            if (isp .ge. 14  .and.  isp .le. 18   .or.
     +          jsp .ge. 14  .and.  jsp .le. 18) then
              lines = lines + 1
              msg(lines) =
     +        'Replacement involves a corrector or monitor'
     +        // ' (Closed orbit correction lost),'
              call mzdrop(0, lq(lcseq-mscom), 'L')
            endif
          endif
        endif
        if (drpali) then
          lines = lines + 1
          msg(lines) = 'Misalignment errors dropped,'
        endif
        if (drpfld) then
          lines = lines + 1
          msg(lines) = 'Field errors dropped,'
        endif
        if (drpspl) then
          lines = lines + 1
          msg(lines) = 'SPLIT information dropped,'
        endif
 
*==== Replacement involving beam line.
      else
 
*---- Look for occurence of item in sequence.
        found = .false.
        if (lcseq .ne. 0) then
          lsdir = lq(lcseq-msdir)
          do 10 ipos = iq(lcseq+msr1), iq(lcseq+msr2)
            if (iq(lsdir+ipos) .eq. idir) found = .true.
   10     continue
        endif
 
*---- If found, then drop sequence.
        if (found) then
          lines = lines + 1
          msg(lines) =
     +    'Name occurs in main beam line; expansion deleted,'
          call mzdrop(0, lcseq, '.')
          lcseq = 0
        endif
      endif
 
*---- Finish warning message.
      lines = lines + 1
      write (msg(lines), 910) iq(laaold+mbln)
  910 format('Previous definition occurred in line ',i6,'.')
      call aawarn('AASMOD', lines, msg)
 
      end
