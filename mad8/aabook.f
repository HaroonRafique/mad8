      subroutine aabook(lbank, label, ipr, isp, lkey, ilink)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Book new command or parameter bank and link it to keyword bank.    *
*   Requires that the keyword common blocks are loaded first.          *
* Input:                                                               *
*   LABEL    (char)     Name for new bank.                             *
*   IPR      (integer)  Process code.                                  *
*   ISP      (integer)  Subprocess code.                               *
*   LKEY(1)  (pointer)  Pointer to current keyword.                    *
*   ILINK    (integer)  Link bias in keyword bank.                     *
* Output:                                                              *
*   LBANK(1) (pointer)  Pointer to new command bank.                   *
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
      integer idata,ikat,ilink,ipr,isp,j,ncat,nd,nkat,nr,ns
      character*(*)     label
      integer           lbank(*), lkey(*)
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
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
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
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*4       name
 
*---- Count command attributes.
      nkat = iq(lkey(1)+mbat)
      ncat = 0
      do 10 j = 1, nkat
        ncat = ncat + iadim1(j) * iadim2(j) * iadim3(j)
   10 continue
 
*---- Set up data and link counters.
      nd = mbat + ncat * mcsiz
      ns = ncat
      nr = ncat
      if (ipr .eq. mpelm) then
        ns = ncat + mbelie
        nr = ncat + mbecls
      endif
 
*---- Lift bank, link it to keyword, and mark it as modified.
      name = label
      call mzbook(2, lbank, lkey, -ilink, name, nr, ns, nd, 0, 0)
      call aamark('AABOOK', lbank)
 
*---- Store bank description.
      iq(lbank(1)+mbfrm) = 16 * 5 + 2
      iq(lbank(1)+mbnam) = 0
      iq(lbank(1)+mbln) = lintok
      iq(lbank(1)+mbpr) = ipr
      iq(lbank(1)+mbsp) = isp
      iq(lbank(1)+mbat) = ncat
 
*---- Format control words and data types.
      idata = mbat
      do 90 ikat = 1, nkat
        nd = iadim1(ikat) * iadim2(ikat) * iadim3(ikat)
        do 80 j = 1, nd
          iq(lbank(1)+idata+mcf1) = 16 * 1 + 2
          iq(lbank(1)+idata+mctyp) = 10 * iatype(ikat)
          if (iatype(ikat) .eq. mtnam) then
            iq(lbank(1)+idata+mcf2) = 16 * mwnam + 5
            call vblank(iq(lbank(1)+idata+mcval), mwnam)
          else if (iatype(ikat) .eq. mtflt  .or.
     +             iatype(ikat) .eq. mtdef) then
            iq(lbank(1)+idata+mcf2) = 16 * mwnam + mreal
          else
            iq(lbank(1)+idata+mcf2) = 16 * mwnam + 2
          endif
          idata = idata + mcsiz
   80   continue
   90 continue
 
      end
