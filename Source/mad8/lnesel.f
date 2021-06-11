      subroutine lnesel
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   SELECT command, set dump flags for sequence editor.                *
* Attributes, must be given in this order in the dictionary:           *
*   FLAG      (name)    Must be "SEQEDIT" when we come here.           *
*   RANGE     (range)   Range to limit selections.                     *
*   CLASS     (name)    Class of elements to be affected in RANGE.     *
*   PATTERN   (string)  Regular expression to limit choice.            *
*   FULL      (logical) If true, all dump flags are set.               *
*   CLEAR     (logical) If true, all dump flags are cleared first.     *
* (*) Dimensions may be changed in the command dictionary.             *
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
      integer llnact,llnbnk,llncal,llneat,llnedr,llnefl,llnesq,llnhed,
     +llnrls,llnrsq,llnsup,llntmp,llnxls,llnxsq
 
*---- Link area for beam line handler.
      common /lnlink/   llnbnk, llnrls, llnrsq, llnsup,
     +                  llnact, llncal, llnhed, llnxls, llnxsq,
     +                  llnesq, llnedr, llneat, llntmp(4), llnefl
      save              /lnlink/
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
      integer idir,ipos,ipos1,ipos2,j,l,mclass,mclear,mfull,mpatt,
     +mrange,ncount,nd
 
      integer           itype(6)
      logical           flag(2), found
      character*(mcnam) class, label
      character*(mcstr) patt
 
*---- Note that attribute "FLAG" has already been used.
      parameter         (mrange = 2, mclass = 3, mpatt  = 4)
      parameter         (mfull  = 5, mclear = 6)
 
*---- Get bank pointers.
      llneat = llnesq
      llnedr = lq(llneat-1)
      llnefl = lq(llnedr-1)
      if (llnefl .eq. 0) then
        nd = iq(llnedr-1)
        call mzbook(2, llnefl, llnedr, -1, 'SFLG', 0, 0, nd, 2, 0)
      endif
 
*---- Fetch logical flags.
      call utgtyp(lccmd, itype)
      flag(1) = .false.
      flag(2) = .false.
      call utglog(lccmd, mfull, mclear, flag)
      ipos1 = 2
      ipos2 = iq(llnedr+1) - 1
 
*---- FULL option sets select flags for full map.
      if (flag(1)) then
        do 10 ipos = ipos1, ipos2
          iq(llnefl+ipos) = 1
   10   continue
 
*---- CLEAR option: clears select flags for full map.
      else if (flag(2)) then
        call mzdrop(0, llnefl, ' ')
        do 20 j = mrange, mpatt
          if (itype(j) .ne. 0) then
            call aawarn('LNESEL', 1,
     +        'No selection can be made with "CLEAR" option.')
            return
          endif
   20   continue
 
*---- Now set select flags according to RANGE, CLASS and PATTERN.
      else
        class = ' '
        patt  = ' '
        call utgnam(lccmd, mclass, mclass, class)
        call utgstr(lccmd, mpatt,  mpatt,  patt)
        lcatt = lq(lccmd-mrange)
 
*---- Determine range to be considered.
        if (lcatt .ne. 0) then
          call lnepos(lcatt, llnedr, 0, ipos1, error)
          call lnepos(lcatt, llnedr, 3, ipos2, error)
          if (.not. error  .and.  ipos1 .gt. ipos2) then
            call aafail('UTGRNG', 1,
     +                  'Begin and end of range inverted.')
            error = .true.
          endif
        endif
        if (error) go to 9999
 
*---- Find class name.
        if (class .eq. ' ') then
          lccls = 0
        else
          call difind(ldbnk, class, idir, lccls)
          if (lccls .eq. 0) then
            call utleng(class, l)
            msg(1) = 'Unknown class name "' // class(1:l) // '".'
            call aafail('ENSRNG', 1, msg)
            error = .true.
          endif
        endif
 
*---- Build pattern.
        if (patt .eq. ' '  .or.  patt .eq. '.*') then
          lref1 = 0
        else
          call utpatt(patt, lref1)
        endif
 
*---- Loop for positions.
        if (error) go to 9999
        ncount = 0
        do 90 ipos = ipos1, ipos2
          idir = iq(llnedr+ipos)
          lcelm = lq(ldbnk(3)-idir)
 
*---- If CLASS != ' ', test for class membership.
          if (lccls .ne. 0) then
            found = .false.
            lref2 = lcelm
   30       if (lref2 .ne. 0  .and. .not. found) then
              if (lref2 .eq. lccls) then
                found = .true.
              endif
              if (iq(lref2+mbpr) .ne. mplin) then
                lref2 = lq(lref2-iq(lref2+mbat)-mbecls)
                go to 30
              endif
            endif
 
*---- Default for CLASS = ' ' is all classes.
          else
            found = .true.
          endif
 
*---- Test for pattern match (skip, if not found as a class member).
          if (found  .and.  lref1 .ne. 0) then
            call diname(ldbnk, idir, label)
            call utmtpt(lref1, label, found)
          endif
 
*---- If tests succeeded, perform action.
          if (found) then
            iq(llnefl+ipos) = 1
            ncount = ncount + 1
          endif
   90   continue
 
*---- Drop pattern.
        if (lref1 .ne. 0) call mzdrop(0, lref1, 'L')
 
*---- Test for empty range.
        if (.not. error  .and.  ncount .eq. 0) then
          call aawarn('ENSRNG', 1,
     +    'No elements found in range which fulfill the conditions.')
        else
          write(msg, 910) ncount
  910     format('You have selected ',i6,' elements in this range.')
          call aainfo('ENSRNG', 1, msg)
        endif
      endif
 
 9999 end
