      subroutine lnpmod(lmark)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Propagate modification flags for lumps.                            *
* Input:                                                               *
*   LLUMP(1) (pointer)  Pointer to lump to be marked.                  *
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
      integer icall,idir,ielem,jbit,laacur,nelem
      integer           lmark(1)
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
      integer mlact,mlf1,mlf2,mlfm,mlfree,mlhd,mlnxt,mlprv,mlref,mlrep,
     +mlsiz,mltyp
 
*---- Bias for beam line list information.
      parameter         (mlfm = mbat + 1, mlhd = mbat + 2,
     +                   mlf1 = mbat + 3, mlf2 = mbat + 4,
     +                   mlfree = mbat + 4)
 
*---- Bias for beam line list cells.
      parameter         (mltyp = 1, mlprv = 2, mlnxt = 3, mlrep = 4,
     +                   mlref = 5, mlact = 6, mlsiz = 6)
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
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
 
      character*(mcnam) bnknam
      logical           fmark
 
*---- Initialize.
      icall = 1
      fmark = .false.
      laacur = lmark(1)
      llump = 0
 
*---- Begin procedure to mark lumps.
  100 continue
        call mzbook(2, llump, llump, 1, 'LSTK', 1, 0, 2, 2, 0)
        lq(llump-1) = laacur
        iq(llump+1) = icall
 
*---- Skip, if already marked.
        if (jbit(iq(laacur),mxknw) .eq. 0) go to 110
          fmark = jbit(iq(laacur),mxmod) .ne. 0
          go to 300
 
*---- Element: test modify flag.
  110   if (iq(laacur+mbpr) .ne. mpelm) go to 150
          if (iq(laacur+mbsp) .eq. 23) go to 120
            fmark = jbit(iq(laacur),mxmod) .ne. 0
            go to 300
 
*---- LUMP: Check list for members.
  120     continue
            call sbit1(iq(laacur), mxknw)
            laacur = lq(ldbnk(3)-iq(laacur+mbat+3*mcsiz+mcval))
            icall = 2
            if (laacur .ne. 0) go to 100
 
*---- Check list for actual arguments.
  130       laacur = lq(lq(llump-1)-4)
            icall = 3
            if (laacur .ne. 0) go to 100
          go to 250
 
*---- LINE, LIST, or SEQUENCE: Check members.
  150   if (iq(laacur+mbpr) .ne. mplin) go to 300
 
*---- LINE or LIST
          if (iq(laacur+mbsp) .eq. 2) go to 200
          call sbit1(iq(laacur), mxknw)
          iq(llump+2) = iq(laacur+mlhd)
  160     if (iq(llump+2) .ge. iq(laacur-1)) go to 250
            if (iq(laacur+iq(llump+2)+mltyp) .lt. 6) go to 180
              laacur = lq(ldbnk(3)-iq(laacur+iq(llump+2)+mlref))
              icall = 4
              if (laacur .ne. 0) go to 100
  180       laacur = lq(llump-1)
            iq(llump+2) = iq(llump+2) + mlsiz
          go to 160
 
*---- SEQUENCE.
  200     call sbit1(iq(laacur), mxknw)
          llnedr = lq(laacur-1)
          nelem = iq(llnedr+1)
          ielem = 2
  210     if (ielem .gt. nelem) go to 250
            idir = iq(llnedr+ielem)
            laacur = lq(ldbnk(3)-idir)
            icall = 5
            if (laacur .ne. 0) go to 100
  220       ielem = ielem + 1
          go to 210
 
*---- Mark LUMP, LINE, LIST, or SEQUENCE as up to date.
  250     laacur = lq(llump-1)
          if (debug) then
            call diname(ldbnk, iq(laacur+mbnam), bnknam)
            msg(1) = 'Marking bank as up  to date: ' // bnknam
            call aainfo('LNPMOD', 1, msg)
          endif
 
*---- End of marking procedure.
  300   laacur = lq(llump-1)
        icall = iq(llump+1)
        call mzdrop(0, llump, '.')
        if (fmark) then
          call sbit1(iq(laacur), mxknw)
          call aamark('LNPMOD', laacur)
          if (llump .ne. 0) go to 300
        endif
      go to (9999, 130, 250, 180, 220), icall
 
 9999 end
