      subroutine lnrefe(lbank, ibias, lseq, lsup, isup)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Expand a beam line reference and return beam sequence bank.        *
*   Input is a normal data reference.                                  *
* Input:                                                               *
*   LBANK(1)  (pointer) Pointer to data bank containg reference.       *
*   IBIAS     (integer) Number of data group.                          *
*   LSUP(1)   (pointer) Supporting link (like for MZBOOK).             *
*   ISUP      (integer) Bias (like for MZBOOK).                        *
* Input/output:                                                        *
*   LSEQ(1)   (pointer) Generated sequence bank.                       *
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
      integer ibias,idata,idir,ienum,ileng,ipos,isup,itype,jbyt,nd
      integer           lbank(*), lseq(*), lsup(*)
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
 
      character*(mcnam) elmnam
      character*(mcrng) rngnam
 
*---- Book beam line sequence bank.
      llnbnk = lbank(1)
      llnsup = lsup(1)
      call mzbook(2, llnrsq, llnsup, isup, 'SEQ ', msref, mslnk,
     +            mss, 7, 0)
 
*---- Type of beam line reference.
      idata = mbat + mcsiz * (ibias - 1)
      itype = iq(llnbnk+idata+mctyp)
      idir = iq(llnbnk+idata+mcval)
      if (idir .eq. 0  .or.  itype/10 .ne. mtlin) then
        call rdfail('LNREFE', 1, 'Invalid line reference.')
      else
        call diname(ldbnk, idir, elmnam)
        call utleng(elmnam, ileng)
        llnrls = lq(ldbnk(3)-idir)
        msg(1) = 'Attempt to expand "' // elmnam(1:ileng)
     +  // '" --- not a beam line.'
        if (llnrls .eq. 0) then
          call rdfail('LNREFE', 1, msg)
        else if (iq(llnrls+mbpr) .ne. mplin  .or.
     +           iq(llnrls+mbsp) .eq. 3) then
          call rdfail('LNREFE', 1, msg)
        else if (itype .eq. 10*mtlin+1) then
          call lnxpnd(idir, 0, llnrsq)
        else if (itype .eq. 10*mtlin+2) then
          call lnxpnd(idir, lq(llnbnk-ibias), llnrsq)
        else if (itype .eq. 10*mtlin+3) then
          call lnxpnd(idir, 0, llnrsq)
        else
          call rdfail('LNREFE', 1, 'Invalid line reference.')
        endif
      endif
 
*---- In case of error, drop beam line sequence bank.
      if (error) then
        call lndrop(llnrsq)
 
*---- Default is full range, SYMM = .FALSE., SUPER = 1.
      else
        nd = iq(lq(llnrsq-msdir)-1)
        iq(llnrsq+msf1) = 16 * 4 + 2
        iq(llnrsq+msr1) = 1
        iq(llnrsq+msr2) = nd
        iq(llnrsq+msym) = 0
        iq(llnrsq+msup) = 1
        iq(llnrsq+msf2) = 16 * (mss - msbn) + 5
        rngnam = '#S/#E'
        call uctoh(elmnam, iq(llnrsq+msbn), mcwrd, mcnam)
        call uctoh(rngnam, iq(llnrsq+msrn), mcwrd, 40)
 
*---- Build element number bank.
        call mzbook(2, lsnum, llnrsq, -msnum, 'ENUM', 0, 0, nd, 2, 0)
        ienum = 0
        lsflg = lq(llnrsq-msflg)
        do 10 ipos = 1, nd
          if (jbyt(iq(lsflg+ipos),1,mcode) .eq. 1) ienum = ienum + 1
          iq(lsnum+ipos) = ienum
   10   continue
      endif
      lseq(1) = llnrsq
 
      end
