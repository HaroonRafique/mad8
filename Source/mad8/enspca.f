      subroutine enspca(ipos, icount, idum2, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Assign split information to current element.                       *
* Input:                                                               *
*   IPOS      (integer) Current position number.                       *
*   ICOUNT    (integer) Counter for split banks generated.             *
*   IDUM2     (integer) Unused.                                        *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
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
      integer ibias,icount,idata,idum2,ipos,ipr,isp,l,nd
      double precision fract,ftest
      logical           eflag
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
 
      character*(mcnam) posnam
 
*---- Test for valid beam element.
      eflag = .false.
      if (lcelm .eq. 0) go to 90
      ipr = iq(lcelm+mbpr)
      isp = iq(lcelm+mbsp)
      if (ipr .ne. mpelm  .or.  isp .le. 0  .or.  isp .gt. 30) go to 90
 
*---- Can element be split?
      go to (10, 10, 10, 90, 10, 10, 10, 90, 10, 10,
     +       10, 90, 90, 10, 10, 10, 10, 10, 10, 10,
     +       10, 90, 90, 10, 90, 10, 10, 90, 90, 90,
     +       90, 90, 90, 90, 90, 90, 90, 90, 90, 90), isp
 
*---- Lift split bank and put it in proper order.
   10 continue
        idata = mbat + mcsiz
        call ucopy(q(lccmd+idata+mcval), fract, mwflt)
        lsspl = lq(lcseq-msspl)
        ibias = - ipos
        lcspl = lq(lsspl-ipos)
   20   if (lcspl .ne. 0) then
          call ucopy(q(lcspl+mwnam+3), ftest, mwflt)
          if (ftest .lt. fract) then
            lsspl = lcspl
            ibias = 0
            lcspl = lq(lcspl)
            go to 20
          endif
        endif
        nd = mwnam + mwflt + 2
        call mzbook(2, l, lsspl, ibias, 'SPLT', 0, 0, nd, 0, 0)
 
*---- Fill in split information.
        iq(l+1) = mwnam * 16 + 5
        posnam = ' '
        call utgnam(lccmd, 1, 1, posnam)
        call uctoh(posnam, iq(l+2), mcwrd, mcnam)
        iq(l+mwnam+2) = mwflt * 16 + 3
        call ucopy(fract, q(l+mwnam+3), mwflt)
 
*---- Count split banks generated.
        icount = icount + 1
   90 continue
 
      end