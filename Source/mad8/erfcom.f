      subroutine erfcom
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Assign misalignment errors to a set of beam elements or lines.     *
* EFCOMP command. Attributes:                                          *
*   ORDER     (integer) Order of base component.                       *
*   RADIUS    (real)    Radius for relative errors.                    *
*   DBLN      (defer)   Normal dipole error DKLN(0).                   *
*   DKLN(*)   (defer)   Normal multipole error.                        *
*   DBLS      (defer)   Skewed dipole errors DKLS(0).                  *
*   DKLS(*)   (defer)   Skewed multipole errors.                       *
*   DBLNR     (defer)   Relative normal dipole error DKLNR(0).         *
*   DKLNR(*)  (defer)   Relative normal multipole error.               *
*   DBLSR     (defer)   Relative skewed dipole error DKLSR(0).         *
*   DKLSR(*)  (defer)   Relative skewed multipole errors.              *
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
 
*---- Option for additive error components.
      common /erdata/   adderr
      logical           adderr
      save              /erdata/
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
 
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer iln,inorma,inormr,iord,iorder,ipos,ipr,iskewa,iskewr,isp,
     +jbit,knorma,knormr,kskewa,kskewr,lines,mnorma,mord,nkat,nl
 
      integer maxmul
 
*---- Maximum order of multipoles.
      parameter         (maxmul = 20)
      parameter         (mord   = 1, mnorma = 3)
      integer           nelm(2)
 
*---- Check if a map module exists.
      call lnchck('EFIELD', error)
      if (error) go to 9999
 
*---- Get attribute dimensions from keyword.
      call kwget(lckey, iln, ipr, isp, nkat)
 
*---- Lift error banks, if not already done.
      lsdir = lq(lcseq-msdir)
      lsflg = lq(lcseq-msflg)
      lsfld = lq(lcseq-msfld)
      if (lsfld .eq. 0) then
        nl = iq(lsdir-1)
        call mzbook(2, lsfld, lcseq, -msfld, 'EFLD', nl, nl, 0, 2, 0)
      endif
 
*---- Check for conflicts between absolute and relative errors.
      knorma = mnorma
      kskewa = knorma + iadim1(4) + 1
      knormr = kskewa + iadim1(6) + 1
      kskewr = knormr + iadim1(8) + 1
      inorma = mbat + (knorma - 1) * mcsiz
      iskewa = mbat + (kskewa - 1) * mcsiz
      inormr = mbat + (knormr - 1) * mcsiz
      iskewr = mbat + (kskewr - 1) * mcsiz
      do 10 iord = 0, maxmul
        if (iord .le. iadim1(4)  .and.  iord .le. iadim1(8)) then
          if (mod(iq(lccmd+inorma+mctyp),10) .ne. 0  .and.
     +        mod(iq(lccmd+inormr+mctyp),10) .ne. 0) then
            write (msg, 910) iord
            call aawarn('ERFCOM', 2, msg)
          endif
        endif
        if (iord .le. iadim1(6)  .and.  iord .le. iadim1(10)) then
          if (mod(iq(lccmd+iskewa+mctyp),10) .ne. 0  .and.
     +        mod(iq(lccmd+iskewr+mctyp),10) .ne. 0) then
            write (msg, 910) iord
            call aawarn('ERFCOM', 2, msg)
          endif
        endif
        inorma = inorma + mcsiz
        iskewa = iskewa + mcsiz
        inormr = inormr + mcsiz
        iskewr = iskewr + mcsiz
   10 continue
 
*---- Check base order.
      iorder = 0
      call utgint(lccmd, mord, mord, iorder)
      if (iorder .lt. 0  .or.  iorder .gt. maxmul) then
        write (msg, 920) iorder
        call aafail('ERFCOM', 2, msg)
        return
      endif
 
*---- Retrieve beam line description.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Call ERFCCA for setting errors.
      nelm(1) = 0
      nelm(2) = 0
      do 90 ipos = irg1, irg2
        if (jbit(iq(lsflg+ipos), mserr) .ne. 0) then
          call erfcca(ipos, nelm, 0, error)
        endif
   90 continue
 
*---- Message about errors created.
      if (nelm(1) + nelm(2) .eq. 0) then
        call aawarn('ERFCOM', 1,
     +  'No field errors assigned (no or empty range seen).')
      else
        lines = 0
        if (nelm(1) .ne. 0) then
          lines = lines + 1
          write (msg(lines), 930) 'assigned to', nelm(1)
        endif
        if (nelm(2) .ne. 0) then
          lines = lines + 1
          if (adderr) then
            write (msg(lines), 930) 'added to on', nelm(2)
          else
            write (msg(lines), 930) 'replaced on', nelm(2)
          endif
        endif
        call aainfo('ERFCOM', lines, msg)
        newmap = .true.
      endif
 
  910 format('Both absolute and relative field error of order ',
     +       i2,' seen,'/
     +       'absolute errors will be used in case of conflict.')
  920 format('Order of basic component = ',i5,' is out of order.')
  930 format('Field errors ',a,' ',i7,' element(s).')
 
 9999 end
