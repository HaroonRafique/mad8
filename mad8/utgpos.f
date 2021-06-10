      subroutine utgpos(lrng, lseq, iend, ipos, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find place in beam line sequence.                                  *
* Input:                                                               *
*   LRNG(1)   (pointer) Range reference bank.                          *
*   LSEQ(1)   (pointer) Beam line sequence bank.                       *
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
      integer icode,idir,iend,iflag,ileng,index,iocc,ipos,jbyt,jdir
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
 
      eflag = .false.
 
*---- Check for existing sequence bank.
      call lnchck('UTGPOS', error)
      if (error) go to 800
 
      lsdir = lq(lseq(1)-msdir)
      lsflg = lq(lseq(1)-msflg)
      icode = iq(lrng(1)+iend+1)
      index = iq(lrng(1)+iend+3)
 
*---- Start of main beam line.
      if (icode .eq. 1) then
        ipos = 1
        go to 9999
 
*---- End of main beam line.
      else if (icode .eq. 2) then
        ipos = iq(lsflg-1)
        go to 9999
 
*---- Start of range.
      else if (icode .eq. 3) then
        ipos = iq(lseq(1)+msr1)
        go to 9999
 
*---- End of range.
      else if (icode .eq. 4) then
        ipos = iq(lseq(1)+msr2)
        go to 9999
 
*---- Find numbered position in working range.
      else if (icode .eq. 5) then
        iocc = 0
        do 10 ipos = 1, iq(lsflg-1)
          if (jbyt(iq(lsflg+ipos),1,mcode) .eq. 1) then
            iocc = iocc + 1
            if (iocc .ge. index) go to 9999
          endif
   10   continue
 
*---- Numbered position not found.
        write (msg, 910) index
  910   format('Element number ',i8,' not found in working beam line.')
 
*---- Named position: Set code to be searched for.
*     Element             (IFLAG = 1),
*     Beam line begin     (IFLAG = 2),
*     Beam line end       (IFLAG = 3).
      else
        idir = iq(lrng(1)+iend+2)
        lcelm = lq(ldbnk(3)-idir)
        if (iq(lcelm+mbpr) .eq. mpelm) then
          iflag = 1
        else if (iend .eq. 0) then
          iflag = 2
        else
          iflag = 3
        endif
 
*---- Find occurrence of name and code in working range.
        iocc = 0
        do 90 ipos = 1, iq(lsflg-1)
          if (jbyt(iq(lsflg+ipos),1,mcode) .eq. iflag) then
            jdir = iq(lsdir+ipos)
            lcelm = lq(ldbnk(3)-jdir)
 
*---- Try the present object itself.
            if (jdir .eq. idir) then
              iocc = iocc + 1
              if (iocc .ge. index) go to 9999
 
*---- Try all classes which may contain the present element.
            else if (iq(lcelm+mbpr) .eq. mpelm) then
              lccls = lcelm
   20         if (lccls .ne. 0) then
                jdir = iq(lccls+mbnam)
                if (jdir .eq. idir) then
                  iocc = iocc + 1
                  if (iocc .ge. index) go to 9999
                endif
                lccls = lq(lccls-iq(lccls+mbat)-mbecls)
                go to 20
              endif
            endif
          endif
   90   continue
 
*---- Name not found.
        call diname(ldbnk, idir, elmnam)
        call utleng(elmnam, ileng)
        if (iflag .eq. 1) then
          if (index .eq. 0) then
            write (msg, 920) elmnam(1:ileng)
  920       format('Element "',a,'" not found in working beam line.')
          else
            write (msg, 930) elmnam(1:ileng), index
  930       format('Element "',a,'[',i6,
     +      ']" not found in working beam line.')
          endif
        else
          if (index .eq. 0) then
            write (msg, 940) elmnam(1:ileng)
  940       format('Beam line "',a,'" not found in working beam line.')
          else
            write (msg, 950) elmnam(1:ileng), index
  950       format('Beam line "',a,'[',i6,
     +      ']" not found in working beam line.')
          endif
        endif
      endif
 
*---- Error exit.
  800 continue
      call aafail('UTGPOS', 1, msg)
      eflag = .true.
 
 9999 end
