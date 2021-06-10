      subroutine ensplt
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Assign split information to a set of beam elements.                *
* SPLIT command. Attributes:                                           *
*   NAME      (name)    Name to be given to intermediate point.        *
*   FRACTION  (real)    Fraction of element where split is desired.    *
*   RANGE     (range)   Range to limit selections.                     *
*   CLASS     (name)    Class of elements to be affected in RANGE.     *
*   PATTERN   (string)  Regular expression to limit choice.            *
*   FULL      (logical) If true, all elements are split at FRACTION.   *
*   CLEAR     (logical) If true, all splits are cleared first.         *
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer icount,ienum,iflag,iocc,ipos,ipos1,ipos2,mclass,mclear,
     +mfrac,mfull,mname,mpatt,mrange,nl
      double precision fract
 
      external          enspca
      character*(mcnam) elmnam, class
      character*(mcstr) patt
      logical           done, flag(2)
 
      parameter         (mname  = 1, mfrac  = 2)
      parameter         (mrange = 3, mclass = 4, mpatt  = 5)
      parameter         (mfull  = 6, mclear = 7)
 
*---- Check if a map module exists.
      call lnchck('SPLIT', error)
      if (.not. error) then
 
*---- Check if fraction is in range.
        call utgflt(lccmd, mfrac, mfrac, fract)
        if (fract .lt. 0.0  .or.  fract .gt. 1.0) then
          msg(1) = 'SPLIT fraction should lie in the range (0..1)'
     +    // ' SPLIT ignored.'
          call aawarn('ENSPLT', 1, msg)
 
*---- Get logical flags.
        else
          flag(1) = .false.
          flag(2) = .false.
          call utglog(lccmd, mfull, mclear, flag)
 
*---- CLEAR option clears split flags.
          lsdir = lq(lcseq-msdir)
          lsspl = lq(lcseq-msspl)
          if (flag(2)  .and.  lsspl .ne. 0) then
            call mzdrop(0, lsspl, 'V')
            call aainfo('ENSPLT', 1, 'SPLIT positions cleared.')
            lsspl = 0
 
*---- Lift split bank, if not already done.
          else
            if (lsspl .eq. 0) then
              nl = iq(lsdir-1)
              call mzbook(2, lsspl, lcseq, -msspl, 'SPLT', nl, nl, 0,
     +                    2, 0)
            endif
 
*---- FULL option splits all elements; this requires no further setting.
            icount = 0
            if (flag(1)) then
              ipos1 = iq(lcseq+msr1)
              ipos2 = iq(lcseq+msr2)
              do 10 ipos = ipos1, ipos2
                call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
                call enspca(ipos, icount, 0, error)
   10         continue
 
*---- Now set select flags according to RANGE, CLASS and PATTERN.
            else
              class = ' '
              patt  = ' '
              call utgnam(lccmd, mclass, mclass, class)
              call utgstr(lccmd, mpatt,  mpatt,  patt)
              lcatt = lq(lccmd-mrange)
              call ensrng(lcatt, class, patt, enspca, icount, 0, done)
            endif
 
*---- Tell user how many split positions have been added.
            if (icount .ne. 0) then
              write (msg, 920) icount
  920         format('Split positions added to ',i6,' elements.')
              call aainfo('ENSPLT', 1, msg)
            endif
          endif
        endif
      endif
 
      end
