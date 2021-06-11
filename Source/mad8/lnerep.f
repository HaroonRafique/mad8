      subroutine lnerep
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Remove an element from a beam line sequence, REMOVE command.       *
* Attributes:                                                          *
*   CLASS    (name)    Name of class to be removed.                    *
*   PATTERN  (string)  Pattern for names to be removed.                *
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
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
      integer mlact,mlf1,mlf2,mlfm,mlfree,mlhd,mlnxt,mlprv,mlref,mlrep,
     +mlsiz,mltyp
 
*---- Bias for beam line list information.
      parameter         (mlfm = mbat + 1, mlhd = mbat + 2,
     +                   mlf1 = mbat + 3, mlf2 = mbat + 4,
     +                   mlfree = mbat + 4)
 
*---- Bias for beam line list cells.
      parameter         (mltyp = 1, mlprv = 2, mlnxt = 3, mlrep = 4,
     +                   mlref = 5, mlact = 6, mlsiz = 6)
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
      integer idir,ielem,ilen,jdir,jelem,kdir,mxals,nelem
 
      character*(mcnam) oldnam, newnam
      logical           found,  replst, select
 
*---- Retrieve bank pointers and element names.
      llneat = llnesq
      llnedr = lq(llneat-1)
      llnefl = lq(llnedr-1)
      idir = 0
      oldnam = ' '
      newnam = ' '
      call utgnam(lccmd, 1, 1, oldnam)
      call utgnam(lccmd, 2, 2, newnam)
      if (oldnam .eq. ' '  .or.  newnam .eq. ' ') then
        call aafail('LNEREP', 1, 'Need "ELEMENT" and "BY".')
        error = .true.
        go to 9999
      endif
 
*---- "SELECTED" --> remove all selected elements
      select = .false.
      if (oldnam .eq. 'SELECTED') then
 
*---- Should have made a selection.
        if (llnefl .eq. 0) then
          call aafail('LNEREM', 1, 'You have made no selection yet.')
          error = .true.
        else
          select = .true.
        endif
 
*---- Find old name in directory.
      else
        call utleng(oldnam, ilen)
        call difind(ldbnk, oldnam(1:ilen), idir, lcelm)
        if (idir .eq. 0  .or.  iq(lcelm+mbpr) .ne. mpelm) then
          msg(1) = 'Element "' // oldnam(1:ilen) // '" is unknown.'
          call aafail('LNEREP', 1, msg(1))
          go to 9999
        endif
      endif
 
*---- Look up new name in directory
      replst = .false.
      call utleng(newnam, ilen)
      call difind(ldbnk, newnam(1:ilen), kdir, lcelm)
*     Not found ?
      if (kdir .eq. 0) then
        msg(1) = 'Element "' // newnam(1:ilen) // '" is unknown.'
        call aafail('LNEREP', 1, msg(1))
        error = .true.
      else
*     Replacement list ?
        if (iq(lcelm+mbpr).eq.mplin .and. iq(lcelm+mbsp).eq.3) then
          call lnxres
          llncal = lcelm
          replst = .true.
*     Should be element.
        else if (iq(lcelm+mbpr) .ne. mpelm) then
          msg(1) = 'Element "' // newnam(1:ilen) // '" is ivalid.'
          call aafail('LNEREP', 1, msg(1))
          error = .true.
        endif
      endif
 
*---- Retrieve data for sequence to be edited.
      if (error) go to 9999
      llneat = llnesq
      llnedr = lq(llneat-1)
      llnefl = lq(llnedr-1)
      nelem = iq(llnedr+1)
      jelem = 0
 
*---- Loop over sequence and replace.
      do 90 ielem = 2, nelem
        if (select) then
          found = iq(llnefl+ielem) .ne. 0
          iq(llnefl+ielem) = 0
        else
          found = iq(llnedr+ielem) .eq. idir
        endif
 
*---- Element must be replaced.
*     For replacement list, advance to next item
        if (found) then
          if (replst) then
            call lnxlst(1)
            kdir = iq(llncal+mlfree+mlsiz+mlref)
            lcelm = lq(ldbnk(3)-kdir)
          endif
          idir = iq(llnedr+ielem)
          jdir = iq(lq(ldbnk(3)-idir)+mbnam)
 
*---- Element is the class object (X, AT=...)$
*     Change reference to use Y.
          if (jdir .eq. idir) then
            iq(llnedr+ielem) = kdir
 
*---- Element is alias (X1: X, AT=...):
*     Change alias X1 to point to Y and mark Y as an alias.
          else
            lq(ldbnk(3)-idir) = lcelm
            call sbit1(iq(lcelm), mxals)
          endif
          jelem = jelem + 1
        endif
   90 continue
 
*---- Mark, if modified.
      if (jelem .gt. 0) then
        call aamark('LNEREP', llneat)
        write (msg, 910) jelem
  910   format(i5,' element(s) replaced.')
        call aainfo('LNEREP', 1, msg)
      else
        call aawarn('LNEREP', 1, 'No element replaced.')
      endif
 
 9999 end
