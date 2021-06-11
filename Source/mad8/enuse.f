      subroutine enuse
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Execute USE command, i.e. expand main beam line.                   *
* Creates a beam line sequence bank and two of its dependents:         *
*   link 1:   Directory indices for line members.                      *
*   link 2:   Position types and flags.                                *
* More dependent banks may be created later.                           *
* Attributes, must be given in this order in the dictionary:           *
*   PERIOD    (line)    Beam line to be expanded.                      *
*   RANGE     (range)   Range of beam line to be used.                 *
*   SYMM      (logical) Symmetry flag.                                 *
*   SUPER     (integer) Number of superperiods.                        *
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
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer idata,idir,itype,irg1,irg2,i,ns,nprev,nlift,ikey
 
      character*(mcnam) linnam
      character*(mcrng) rngnam
 
*---- Set up the main beam line.
      ns = 0
      nprev = currseq
      nlift = 0
*     LNREFE lifts the first two dependent banks.
      if (iq(lccmd+mbat+mctyp) .ne. 10 * mtlin) then
        itype = iq(lccmd+mbat+mctyp)
        idir = iq(lccmd+mbat+mcval)
        if (idir .eq. 0  .or.  itype/10 .ne. mtlin) then
          call rdfail('ENUSE', 1, 'Invalid line reference.')
          return
        endif
        call diname(ldbnk, idir, sequnam)
        do i = 1, liftseq
          if (seqnames(i) .eq. sequnam)  ns = i
        enddo
        if (ns .eq. 0)  then
*--- not pre-existing
          if (liftseq .eq. iq(lq(lroot-mcseq)-3))  then
            call mzpush(0, lq(lroot-mcseq), liftseq, 0, 'I')
          endif
          liftseq = liftseq + 1
          currseq = liftseq
          nlift = 1
        else
          call get_active(sequnam, 'ENUSE')
          if (lcseq .ne. 0)  call lndrop(lcseq)
          currseq = ns
        endif
        call lnrefe(lccmd, 1, lcseq, lq(lroot-mcseq), -currseq)
        if (error) then
          if (nlift .ne. 0)  liftseq = liftseq - 1
          currseq = nprev
          go to 9999
        endif
        if (ns .eq. 0)  then
          msg(1) = 'New beam line expanded: ' // sequnam
        else
          msg(1) = 'Beam line re-expanded: ' // sequnam
        endif
        seqnames(currseq) = sequnam
        call enget
        call aainfo('ENUSE', 1, msg)
 
*---- Unlink any BETA0 bank which may exist.
        call difind(ldkey, 'BETA0', ikey, lckey)
        ltwbet = lq(lckey-1)
   10   if (ltwbet .ne. 0) then
          iq(ltwbet-5) = -99999
          ltwbet = lq(ltwbet)
          go to 10
        endif
 
*---- No main beam line set.
      else if (lcseq .eq. 0) then
        call aawarn('ENUSE', 1, 'No beam line specified.')
        return
 
*---- Old beam line kept.
      else
        call uhtoc(q(lcseq+msbn), mcwrd, linnam, mcnam)
        msg(1) = 'Old beam line kept: ' // linnam
        call aainfo('ENUSE', 1, msg)
      endif
  100 continue
*---- Find desired range.
      idata = mbat + mcsiz
      if (iq(lccmd+idata+mctyp) .eq. 10 * mtrng + 1) then
        iq(lcseq+msr1) = 1
        iq(lcseq+msr2) = iq(lq(lcseq-msdir)-1)
        lcatt = lq(lccmd-2)
        call utgrng(lcatt, lcseq, irg1, irg2, error)
        if (error) go to 9999
 
*---- Store range name and limits.
        call enrang(lcatt, rngnam)
        call uctoh(rngnam, iq(lcseq+msrn), mcwrd, 40)
        iq(lcseq+msr1) = irg1
        iq(lcseq+msr2) = irg2
*---- Precomputed maps become invalid.
        call lnmark('ENUSE')
      endif
 
*---- Symmetry flag.
      idata = idata + mcsiz
      if (iq(lccmd+idata+mctyp) .eq. 10 * mtlog + 1) then
        iq(lcseq+msym) = iq(lccmd+idata+mcval)
      endif
 
*---- Number of superperiods.
      idata = idata + mcsiz
      if (iq(lccmd+idata+mctyp) .eq. 10 * mtint + 1) then
        iq(lcseq+msup) = iq(lccmd+idata+mcval)
      endif
 
 9999 end
