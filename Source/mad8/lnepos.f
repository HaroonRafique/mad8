      subroutine lnepos(lrng, lseq, iend, ipos, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find place in sequence.                                            *
* Input:                                                               *
*   LRNG(1)   (pointer) Range reference bank.                          *
*   LSEQ(1)   (pointer) Sequence directory bank.                       *
*   IEND      (integer) 0: Begin of range, 3: End of range.            *
* Output:                                                              *
*   IPOS      (integer) Bias of position.                              *
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
      integer icode,idir,iend,ileng,index,iocc,ipos,jdir
      integer           lrng(*), lseq(*)
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
 
      character*(mcnam) elmnam
 
      icode = iq(lrng(1)+iend+1)
      index = iq(lrng(1)+iend+3)
 
*---- Start of sequence.
      if (icode .eq. 1) then
        ipos = 2
 
*---- End of sequence.
      else if (icode .eq. 2) then
        ipos = iq(lsdir+1) -1
 
*---- Codes #F, #L, #iii not allowed
      else if (icode .le. 5) then
        call aawarn('LNEPOS', 1,
     +              'Codes #F, #L, or #n not defined in SEQEDIT.')
 
*---- Named position: Find occurrence of name in sequence.
      else
        idir = iq(lrng(1)+iend+2)
        iocc = 0
        do 90 ipos = 2, iq(llnedr+1) - 1
          jdir = iq(llnedr+ipos)
 
*---- Try the present object itself.
          if (jdir .eq. idir) then
            iocc = iocc + 1
            if (iocc .ge. index) go to 9999
 
*---- Try all classes which may contain the present element.
          else if (iq(lcelm+mbpr) .eq. mpelm) then
            lccls = lq(ldbnk(3)-jdir)
   20       if (lccls .ne. 0) then
              jdir = iq(lccls+mbnam)
              if (jdir .eq. idir) then
                iocc = iocc + 1
                if (iocc .ge. index) go to 9999
              endif
              lccls = lq(lccls-iq(lccls+mbat)-mbecls)
              go to 20
            endif
          endif
   90   continue
 
*---- Name not found.
        call diname(ldbnk, idir, elmnam)
        call utleng(elmnam, ileng)
        if (index .eq. 0) then
          write (msg, 920) elmnam(1:ileng)
  920     format('Element "',a,'" not found in sequence.')
        else
          write (msg, 930) elmnam(1:ileng), index
  930     format('Element "',a,'[',i6,']" not found in sequence.')
        endif
        call aafail('LNEPOS', 1, msg)
        eflag = .true.
      endif
 
 9999 end
