      subroutine eralca(ipos, ncount, idum, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Assign alignment errors to current element.                        *
* Input:                                                               *
*   IPOS      (integer) Current position number.                       *
*   NCOUNT(2) (integer) Counter for misalignments assigned.            *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
*----------------------------------------------------------------------*
* Modified: 7-JAN-1999, T. Raubenheimer (SLAC)                         *
*   Added LCAV (ISP #27) to physical elements that can be misaligned   *
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
      integer i,idata,idum,iflag,ileng,iocc,ipos,ipr,isp,itype,jbyt,
     +jpos,maxval,nd,nv
      double precision err,errors
      logical           eflag
      integer           ncount(2)
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
 
*---- Option for additive error components.
      common /erdata/   adderr
      logical           adderr
      save              /erdata/
 
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      parameter         (maxval = 10)
 
      character*(mcnam) elmnam
      dimension         errors(maxval)
 
*---- Test for valid pointer.
      eflag = .false.
      lcelm = lq(ldbnk(3)-iq(lsdir+ipos))
      if (lcelm .eq. 0) go to 90
      ipr = iq(lcelm+mbpr)
      iflag = jbyt(iq(lsflg+ipos), 1, mcode)
 
*---- Test for beginning of line.
      if (ipr .eq. mplin  .and.  iflag .eq. 2) go to 10
 
*---- Test for element.
      if (ipr .eq. mpelm) then
        isp = iq(lcelm+mbsp)
        go to (90, 10, 10, 90, 10, 10, 10, 10, 10, 10,
     +         10, 90, 90, 10, 10, 10, 20, 20, 20, 10,
     +         10, 90, 10, 90, 90, 10, 10, 90, 90, 90,
     +         90, 90, 90, 90, 90, 90, 90, 90, 90, 90), isp
      endif
      go to 90
 
*---- All magnet types, cavity, collimator, lump, line, sequence.
   10 continue
        nv = 6
      go to 50
 
*---- Monitor.
   20 continue
        nv = maxval
 
*---- Test for redefinition of errors.
   50 continue
      call diname(ldbnk, iq(lsdir+ipos), elmnam)
      iocc = jbyt(iq(lsflg+ipos), mocc1, mocc2)
      lcali = lq(lsali-ipos)
      call uzero(errors, 1, mwflt*maxval)
      if (lcali .eq. 0) then
       ncount(1) = ncount(1) + 1
      else
        if (adderr) call ucopy(q(lcali+1), errors, iq(lcali-1))
        ncount(2) = ncount(2) + 1
        call mzdrop(0, lcali, '.')
      endif
 
*---- Allocate space for errors.
      nd = nv * mwflt
      call mzbook(2, lcali, lsali, -ipos, 'EALI', 0, 0, nd, mreal, 0)
 
*---- Generate error values.
      idata = mbat
      do 80 i = 1, maxval
        itype = mod(iq(lccmd+idata+mctyp),10)
        if (itype .eq. 3) then
          lcexp = lq(lccmd-i)
          call exeval(lcexp)
        endif
        if (i .le. nv) then
          call ucopy(q(lccmd+idata+mcval), err, mwflt)
          errors(i) = errors(i) + err
        else if (itype .ne. 0) then
          call utleng(elmnam, ileng)
          write (msg, 910) elmnam(1:ileng), iocc
  910     format('Monitor errors ignored: ',a,'[',i8,'].')
          call aawarn('ERALCA', 1, msg)
        endif
        idata = idata + mcsiz
   80 continue
      call ucopy(errors, q(lcali+1), nd)
 
*---- Copy errors to end of line.
      if (iflag .eq. 2) then
        do 85 jpos = ipos, iq(lcseq+msr2)
          if (iq(lsdir+jpos) .eq. iq(lsdir+ipos)  .and.
     +        jbyt(iq(lsflg+ipos), 1, mcode) .eq. 3) then
            if (lq(lsali-jpos) .ne. 0) then
              call mzdrop(0, lq(lsali-jpos), ' ')
            endif
            call mzcopy(2, lcali, 2, lsali, -jpos, 'S')
          endif
  85    continue
      endif
 
*---- Anything else cannot be misaligned.
   90 continue
 
      end
