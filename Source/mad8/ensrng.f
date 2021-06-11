      subroutine ensrng(lrng, class, patt, action, idum1, idum2, done)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Apply subroutine ACTION to all places in a range.                  *
* Input:                                                               *
*   LCSEQ     /REFER/   Current beam line sequence.                    *
*   LRNG(1)   (pointer) Range reference bank.                          *
*   CLASS     (name)    Class name to limit selection.                 *
*   PATTERN   (string)  Pattern string to limit selection.             *
*                       (all three conditions "anded" together).       *
*   ACTION    (subr)    Subroutine to be executed in each position.    *
*   IDUM1(*)  (integer) Additional argument for ACTION.                *
*   IDUM2(*)  (integer) Additional argument for ACTION.                *
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
      integer idir,ipos,ipos1,ipos2,l
      integer           lrng(*), idum1(*), idum2(*)
      character*(mcnam) class, label
      character*(mcstr) patt
      logical           done
      external          action
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
 
      logical           eflag, found
 
*---- Determine range to be considered.
      if (lrng(1) .eq. 0) then
        ipos1 = iq(lcseq+msr1)
        ipos2 = iq(lcseq+msr2)
      else
        call utgrng(lrng, lcseq, ipos1, ipos2, error)
      endif
 
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
      done = .false.
      lsdir = lq(lcseq-msdir)
      do 90 ipos = ipos1, ipos2
        idir  = iq(lsdir+ipos)
        lcelm = lq(ldbnk(3)-idir)
 
*---- If CLASS != ' ', test for class membership.
        if (lccls .ne. 0) then
          found = .false.
          lref2 = lcelm
   20     if (lref2 .ne. 0  .and. .not. found) then
            if (lref2 .eq. lccls) then
              found = .true.
            endif
            if (iq(lref2+mbpr) .ne. mplin) then
              lref2 = lq(lref2-iq(lref2+mbat)-mbecls)
              go to 20
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
          eflag = .false.
          call action(ipos, idum1, idum2, eflag)
          error = error .or. eflag
          done = .true.
        endif
   90 continue
 
*---- Drop pattern.
      if (lref1 .ne. 0) call mzdrop(0, lref1, 'L')
 
*---- Test for empty range.
      if (.not. (error .or. done)) then
        call aawarn('ENSRNG', 1,
     +  'No elements found in range which fulfill the conditions.')
      endif
 
 9999 end
