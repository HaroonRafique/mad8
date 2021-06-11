      subroutine endump
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   SELECT command, set dump flags in working beam line.               *
* Attributes, must be given in this order in the dictionary:           *
*   FLAG      (name)    Dump flag to be set or reset.                  *
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer idict,iflag,ipos,ipos1,ipos2,j,leng,mclass,mclear,mflag,
     +mfull,mpatt,mrange,ndict
 
      parameter         (ndict = 9)
      parameter         (mflag  = 1)
      parameter         (mrange = 2, mclass = 3, mpatt  = 4)
      parameter         (mfull  = 5, mclear = 6)
 
      logical           done, flag(2)
      character*(mcnam) dict(ndict), flgnam, class
      character*(mcstr) patt
      integer           kflag(ndict), itype(6)
      external          enflag
 
      data dict         / 'FIRST',  'LIE',    'REFER',  'SECOND',
     +                    'TWISS',  'OPTICS', 'TRACK',  'ERROR',
     +                    'SEQEDIT' /
      data kflag        /  mfrst,    mlump,    mrefe,    mscnd,
     +                     mprnt,    moptc,    mtrck,    mserr,
     +                     0 /
 
*---- Fetch flag name.
      call utgtyp(lccmd, itype)
      flgnam = 'OPTICS'
      call utgnam(lccmd, mflag, mflag, flgnam)
      call utleng(flgnam, leng)
      call utlook(flgnam(1:leng), dict, ndict, idict)
      if (idict .eq. 0) then
        msg(1) = 'Unknown selection flag "' // flgnam // '" ignored.'
        call aawarn('ENDUMP', 1, msg)
 
*---- Flag is "SEQEDIT"; reroute call to sequence editor
*     Check valid use of sequence editor subcommand.
      else if (idict .eq. ndict) then
        if (imodul .ne. mplin) then
          msg(1) =
     +    'Cannot run editor subcommand outside sequence editor,'
          msg(2) = 'SEQEDIT command required first.'
          call aafail('LNESEL', 2, msg)
        else
          call lnesel
        endif
 
*---- Check main beam line.
      else
        call lnchck('SELECT', error)
        if (.not. error) then
          lsflg = lq(lcseq-msflg)
          iflag = kflag(idict)
 
*---- Fetch logical flags.
          flag(1) = .false.
          flag(2) = .false.
          call utglog(lccmd, mfull, mclear, flag)
 
*---- FULL option sets select flags for full map.
*     In this case no further processing is needed.
          ipos1 = iq(lcseq+msr1)
          ipos2 = iq(lcseq+msr2)
          if (flag(1)) then
            do 20 ipos = ipos1, ipos2
              call sbit1(iq(lsflg+ipos), iflag)
   20       continue
 
*---- CLEAR option: clears select flags for full map.
          else if (flag(2)) then
            do 30 ipos = ipos1, ipos2
              call sbit0(iq(lsflg+ipos), iflag)
   30       continue
            do 40 j = mrange, mpatt
              if (itype(j) .ne. 0) then
                call aawarn('ENDUMP', 1,
     +            'No selection can be made with "CLEAR" option.')
                return
              endif
   40       continue
 
*---- Now set select flags according to RANGE, CLASS and PATTERN.
          else
            class = ' '
            patt  = ' '
            call utgnam(lccmd, mclass, mclass, class)
            call utgstr(lccmd, mpatt,  mpatt,  patt)
            lcatt = lq(lccmd-mrange)
            call ensrng(lcatt, class, patt, enflag, iflag, 0, done)
          endif
        endif
      endif
 
      end
