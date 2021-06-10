      subroutine lnecyc
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Cyclic exchange of elements in sequence, CYCLE command.            *
* Attributes:                                                          *
*   START    (name)    Name of element for new start (marker).         *
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
      integer ixopr,ixsub1,ixsub2,ixsub3,maxexp,nxopr
      double precision rxval
 
*---- Expression description.
      parameter         (maxexp = 100)
      common /exprsa/   nxopr, ixopr(maxexp),
     +                  ixsub1(maxexp), ixsub2(maxexp), ixsub3(maxexp)
      common /exprsc/   axbank(maxexp), axattr(maxexp)
      common /exprsr/   rxval(maxexp)
      save              /exprsa/, /exprsc/, /exprsr/
      character*(mcnam) axbank, axattr
      integer isopr,isval,level,maxstk
      double precision rsval
 
*---- Stack for expression decoding and evaluation.
      parameter         (maxstk = 100)
      common /exstki/   level, isopr(maxstk), isval(maxstk)
      common /exstkr/   rsval(maxstk)
      save              /exstki/, /exstkr/
 
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
      integer idir,ielem,ilast,ilen,incr,jdata,jelem,kdata,kelem,kfinal,
     +ndata,nelem
      double precision zero
 
      parameter         (zero = 0.0d0)
 
      character*(mcnam) elmnam
 
*---- Retrieve data for sequence to be edited.
      llnedr = lq(llnesq-1)
      llneat = llnesq
      nelem  = iq(llnedr+1)
 
*---- Check that last element is marker.
      idir  = iq(llnedr+nelem)
      lcelm = lq(ldbnk(3)-idir)
      if (iq(lcelm+mbpr) .ne. mpelm  .or. iq(lcelm+mbsp) .ne. 25) then
        call aafail('LNECYC', 1,
     +    'Last element in sequence should be a marker.')
        go to 9999
      endif
 
*---- Retrieve START position.
      elmnam = ' '
      call utgnam(lccmd, 1, 1, elmnam)
      if (elmnam .eq. ' ') then
        call rdfail('LNECYC', 1, 'Need START name for CYCLE.')
        go to 9999
      endif
 
*---- Look up element name in directory.
      call utleng(elmnam, ilen)
      call difind(ldbnk, elmnam(1:ilen), idir, lcelm)
      if (lcelm .eq. 0) then
        call rdfail('LNECYC', 1, 'Element "' // elmnam(1:ilen) //
     +              '" is not defined."')
        go to 9999
      endif
 
*---- Search for START position in sequence.
      do 10 ielem = 2, nelem
        if (iq(llnedr+ielem) .eq. idir) go to 20
   10 continue
      call rdfail('LNECYC', 1, 'Element "' // elmnam(1:ilen) //
     +            '" not found in sequence.')
      go to 9999
 
*---- Avoid cycling to previous starting position.
   20 if (ielem .eq. 2) then
        call aawarn('LNECYC', 1,
     +    'START position is already origin of sequence.')
        go to 9999
      endif
 
*---- Start position must be a marker.
      lcelm = lq(ldbnk(3)-idir)
      if (iq(lcelm+mbpr) .ne. mpelm  .or. iq(lcelm+mbsp) .ne. 25) then
        call aafail('LNECYC', 1,
     +    'START position for CYCLE should be a marker.')
        go to 9999
      endif
 
*---- Make sure there is enough space for interchange.
      ilast = nelem + ielem - 2
      if (ilast .ge. iq(llnedr-1)) then
        incr = ilast - iq(llnedr-1) + 1
        call mzpush(0, llnedr, 0, incr, '.')
        call mzpush(0, llneat, incr, incr * mwflt, '.')
        llnesq = llneat
      endif
 
*---- Move elements preceding START to end, adding total length.
      jdata = mbat + mcsiz + 2
      ndata = mbat + mcsiz + 2 + (nelem - 2) * mwflt
      kelem = nelem
      kdata = ndata
      do 30 jelem = 2, ielem - 1
        kelem = kelem + 1
        kdata = kdata + mwflt
        iq(llnedr+kelem) = iq(llnedr+jelem)
        level = 0
        nxopr = 0
        call excopy(llneat, jelem, jdata)
        call excopy(llneat, nelem, ndata)
        call exbin(1)
        call exmake(llneat, kelem, kdata, rsval(1), isval(1) + 1)
        jdata = jdata + mwflt
   30 continue
      kfinal = kelem
 
*---- Move START element, keep its original position.
      kelem = 2
      kdata = mbat + mcsiz + 2
      iq(llnedr+kelem) = iq(llnedr+ielem)
      call ucopy(q(llneat+jdata), q(llneat+kdata), mwflt)
      lcexp = lq(llneat-2)
      if (lcexp .ne. 0) call aadrop(lcexp)
      lcexp = lq(llneat-ielem)
      call zshunt(0, lcexp, llneat, -2, 0)
 
*---- Move all elements to position, subtracting START position.
*     Skip element NELEM (end marker stays in place).
      do 40 jelem = ielem + 1, kfinal
        jdata = jdata + mwflt
        if (jelem .ne. nelem) then
          kelem = kelem + 1
          kdata = kdata + mwflt
          iq(llnedr+kelem) = iq(llnedr+jelem)
          level = 0
          nxopr = 0
          call excopy(llneat, jelem, jdata)
          call excopy(llneat, 2, mbat+mcsiz+2)
          call exbin(2)
          call exmake(llneat, kelem, kdata, rsval(1), isval(1) + 1)
        endif
   40 continue
 
*---- Set START element's position to zero.
      lcexp = lq(llneat-2)
      if (lcexp .ne. 0) call aadrop(lcexp)
      call ucopy(zero, q(llneat+mbat+mcsiz+2), mwflt)
 
*---- Mark as modified.
      call aamark('LNECYC', llneat)
      call aainfo('LNECYC', 1,
     +  'Cyclic interchange completed, end marker stays in place.')
 
 9999 end
