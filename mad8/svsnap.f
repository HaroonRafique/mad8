      subroutine svsnap(lbank)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save snapshot data for one element bank.                           *
* Input:                                                               *
*   LBANK(1)  (pointer) Bank pointer.                                  *
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
      integer icat,idata,ikat,iln,ipr,isp,itype,nkat
      double precision rval
      integer           lbank(1)
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*(mcnam) label, class
 
*---- Retrieve label and keyword definition.
      lcelm = lbank(1)
      lckey = lq(lcelm+1)
      call diname(ldbnk, iq(lcelm+mbnam), label)
      call kwget(lckey, iln, ipr, isp, nkat)
      call diname(ldkey, iq(lckey+mbnam), class)
 
*---- Class name.
      if (iq(lcelm+mbpr) .eq. mpelm) then
        if (iq(lcelm+mbsp) .eq. 1  .and.
     +      label(1:1) .eq. '['  .and.  label(8:) .eq. ']') then
          go to 9999
        endif
        lcsrc = lq(lcelm-iq(lcelm+mbat)-mbecls)
        if (lcsrc .ne. 0) then
          call diname(ldbnk, iq(lcsrc+mbnam), class)
        endif
      endif
 
*---- Save label and keyword.
      call svbegn
      if (label .ne. ' ') then
        call svname(label)
        call svlitt(': ')
      endif
      call svname(class)
 
*---- Loop over all attributes.
      idata = mbat
      icat  = 1
      do 90 ikat = 1, nkat
        itype  = iatype(ikat)
 
*---- Test for defined real attribute value.
        if (itype .eq. mtflt  .and.
     +      mod(iq(lcelm+idata+mctyp),10) .ne. 0) then
          call svlitt(', ')
          call svname(katnam(ikat))
          call svlitt('=')
          call ucopy(q(lcelm+idata+mcval), rval, mwflt)
          call svreal(rval)
        endif
        idata = idata + mcsiz
        icat  = icat  + 1
   90 continue
 
      call svdump
 
 9999 end
