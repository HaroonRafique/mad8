      subroutine lnerem
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
      integer ibias,idata,idir,ielem,ilen,jdata,jelem,nelem
 
      character*(mcnam) elmnam
      logical           found, select
 
*---- Retrieve bank pointers and element name
      llneat = llnesq
      llnedr = lq(llneat-1)
      llnefl = lq(llnedr-1)
      elmnam = ' '
      call utgnam(lccmd, 1, 1, elmnam)
      if (elmnam .eq. ' ') then
        call aafail('LNEREM', 1, 'Need element name or "SELECTED".')
        go to 9999
 
*---- "SELECTED" --> remove all selected elements
      else if (elmnam .eq. 'SELECTED') then
 
*---- Should have made a selection.
        if (llnefl .eq. 0) then
          call aafail('LNEREM', 1, 'You have made no selection yet.')
          error = .true.
        else
          select = .true.
        endif
 
*---- Element name given --> find it in directory.
      else
        select = .false.
        call utleng(elmnam, ilen)
        call difind(ldbnk, elmnam(1:ilen), idir, lcelm)
        if (idir .eq. 0  .or.  iq(lcelm+mbpr) .ne. mpelm) then
          msg(1) = 'Element "' // elmnam(1:ilen) // '" is unknown.'
          call aafail('LNEREM', 1, msg(1))
          error = .true.
        endif
      endif
 
*---- Retrieve data for sequence to be edited.
      if (error) go to 9999
      llneat = llnesq
      llnedr = lq(llneat-1)
      llnefl = lq(llnedr-1)
      nelem = iq(llnedr+1)
      jelem = 1
      idata = mbat + mcsiz + 2
      jdata = idata
 
*---- Loop over sequence to remove.
      do 90 ielem = 2, nelem
       if (select) then
          found = iq(llnefl+ielem) .ne. 0
        else
          found = iq(llnedr+ielem) .eq. idir
        endif
 
*---- If found, drop expression.
        if (found) then
          lcexp = lq(llneat-ielem)
          if (lcexp .ne. 0) call aadrop(lcexp)
 
*---- Otherwise move down.
        else
          jelem = jelem + 1
          if (jelem .lt. ielem) then
            call ucopy(q(llneat+idata), q(llneat+jdata), mwflt)
            iq(llnedr+jelem) = iq(llnedr+ielem)
            lcexp = lq(llneat-ielem)
            if (lcexp .ne. 0) then
              call zshunt(0, lcexp, llneat, -jelem, 0)
              ibias = mxsiz * iq(lcexp-3) + mxval
              iq(lcexp+ibias) = jdata
            endif
            if (llnefl .ne. 0) iq(llnefl+jelem) = iq(llnefl+ielem)
          endif
          jdata = jdata + mwflt
        endif
        idata = idata + mwflt
   90 continue
      iq(llnedr+1) = jelem
 
*---- Mark, if modified.
      if (jelem .lt. nelem) then
        call aamark('LNEREM', llneat)
        write (msg, 910) nelem - jelem
  910   format(i5,' element(s) removed.')
        call aainfo('LNEREM', 1, msg)
      else
        call aawarn('LNEREM', 1, 'No element removed.')
      endif
 
 9999 end
