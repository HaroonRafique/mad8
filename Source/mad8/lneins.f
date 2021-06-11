      subroutine lneins
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Insert element(s) in a beam line sequence, INSERT command.         *
*   Last element must be marker.                                       *
* Attributes:                                                          *
*   ELEMENT  (name)    Name of element to be added.                    *
*   CLASS    (name)    Class name (like SEQUENCE definition).          *
*   AT       (real)    Position for new element.                       *
*   FROM     (name)    If given, AT is relative to this element,       *
*                      otherwise AT is absolute position.              *
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
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
      integer ibias,icat,idata,idir,ielem,iexpr,ilen,iln,ipr,isp,jdata,
     +jdir,jelem,kdata,kelem,leng,melem,ncat,nd,nelem,ni,nkat,nl
      double precision pos,posi,posj
 
      character*(mcnam) clsnam, elmnam, frmnam, label
      integer           itype(4)
      logical           copy, found, select
 
*---- Retrieve data for sequence to be edited.
      llneat = llnesq
      llnedr = lq(llneat-1)
      llnefl = lq(llnedr-1)
 
*---- Retrieve data flags.
      call utgtyp(lccmd, itype)
 
*---- Retrieve name of element to be inserted.
      elmnam = ' '
      call utgnam(lccmd, 1, 1, elmnam)
      if (elmnam .eq. ' ') then
        call rdfail('LNEINS', 1, 'Need element name.')
        error = .true.
      endif
 
*---- Retrieve class of element to be inserted.
      clsnam = elmnam
      call utgnam(lccmd, 2, 2, clsnam)
 
*---- Look up class name.
      call utleng(clsnam, leng)
      call difind(ldbnk, clsnam(1:leng), idir, lccls)
      if (lccls .eq. 0  .or.  iq(lccls+mbpr) .ne. mpelm) then
        msg(1) = 'Element "' // clsnam(1:leng) // '" is unknown.'
        call rdfail('LNEINS', 1, msg)
        go to 9999
      endif
 
*---- Retriev "FROM" name.
      select = .false.
      if (itype(4) .ne. 0) then
        frmnam = ' '
        call utgnam(lccmd, 4, 4, frmnam)
 
*---- "SELECTED" --> install, element, at=ds, from=selected
        if (frmnam .eq. 'SELECTED') then
          select = .true.
 
*---- Look up "FROM" element name in directory.
        else
          call utleng(frmnam, ilen)
          call difind(ldbnk, frmnam(1:ilen), jdir, lcelm)
          if (jdir .eq. 0  .or.  iq(lcelm+mbpr) .ne. mpelm) then
            msg(1) = 'Element "' // frmnam(1:ilen) // '" is unknown.'
            call aafail('LNEINS', 1, msg)
            error = .true.
          endif
        endif
      endif
 
*---- Set flag for allowable copy.
      if (elmnam .ne. clsnam) then
        call direfe(ldbnk, elmnam, idir)
 
*---- Already defined --> redefinition not allowed.
        if (lq(ldbnk(3)-idir) .ne. 0) then
          lcelm = lccls
          copy = .false.
*---- Not yet defined --> new element.
        else
          lckey = lq(lccls+1)
          call kwget(lckey, iln, ipr, isp, nkat)
          call aabook(lcelm, elmnam, ipr, isp, lckey, 1)
          lq(ldbnk(3)-idir) = lccls
          copy = .true.
        endif
      else
        lcelm = lccls
        copy = .false.
      endif
 
*---- Test for additional attributes.
      if (token(jtok) .eq. ',') then
 
*---- Attributes are allowed.
        if (copy) then
 
*---- Copy default attributes from class.
          ncat = iq(lccls+mbat)
          do 10 icat = 1, ncat
            call aacopy(lccls, icat, lcelm)
  10      continue
 
*---- Link to class name.
          call sbit1(iq(lccls), mxcls)
          lq(lcelm-ncat-mbecls) = lccls
 
*---- Decode additional attributes.
          lcdef = lq(lckey-2)
          call aaattr(lcdef, lcelm, nkat, error)
          if (error) call aadrop(lcelm)
 
*---- DEFINE dump option.
          lq(ldbnk(3)-idir) = lcelm
          iq(lcelm+mbnam) = idir
          if (ideffl .eq. 1  .or.  ideffl .eq. 3) then
            call aadump(lcelm)
          endif
          if (ideffl .eq. 2  .or.  ideffl .eq. 3) then
            call dzshow('element', 0, lcelm, 'V', 0, 0, 0, 0)
          endif
 
*---- Duplicate name, change of attributes is not allowed.
        else
          call utleng(elmnam, leng)
          msg(1) = 'Name "' // elmnam(1:leng) // '" is not unique.'
          call rdfail('LNEINS', 1, msg)
          error = .true.
        endif
 
*---- Mark source bank as a synonym.
      else
        call sbit1(iq(lccls), mxals)
      endif
 
*---- Pass 1: Count elements which will be inserted.
      if (error) go to 9999
      nelem = iq(llnedr+1)
      melem = 0
      do 20 ielem = 2, nelem
        if (select) then
          found = iq(llnefl+ielem) .ne. 0
        else
          found = iq(llnedr+ielem) .eq. jdir
        endif
        if (found) melem = melem + 1
   20 continue
 
*---- If there are any installations, make sure there is enough space.
      if (melem .eq. 0) then
        call aainfo('LNEINS', 1, 'No element inserted.')
        go to 9999
      endif
 
*---- Copy keyword to local storage.
      lckey = lq(llnesq+1)
      call kwget(lckey, iln, ipr, isp, nkat)
 
*---- Lift banks for modified sequence.
*     New sequence bank LLNTMP(1): positions and expressions links.
      call diname(ldbnk, iq(llnesq+mbnam), label)
      call aabook(llntmp(1), label, ipr, isp, lckey, 1)
      call ucopy(q(llnesq+mbat+1), q(llntmp(1)+mbat+1), mcsiz)
      nl = nelem + melem - 1
      nd = mwflt * (nelem + melem) + 1
      call mzpush(0, llntmp(1), nl, nd, 'I')
      ni = nelem + melem
*     New sequence bank LLNTMP(2): directory indices.
      call mzbook(2,llntmp(2),llntmp(1),-1,'SDIR',1,1,ni,2,0)
*     New sequence bank LLNTMP(5): selection flags.
      if (llnefl .ne. 0)
     +  call mzbook(2,llntmp(3),llntmp(2),-1,'SFLG',0,0,ni,2,0)
      iq(llnedr+1) = nelem + melem
 
*---- Book banks for insertions.
      nl = melem
      nd = mwflt * melem
*     Temporary bank LLNTMP(1); inserted positions and expression links.
      call mzbook(2,llntmp(4),llntmp(4),1,'SEQU',nl,nl,nd,2,0)
 
*---- Pass 2: Fill banks for insertions.
      idata = mbat + mcsiz + 2
      jelem = 1
      jdata = 1
      do 30 ielem = 2, nelem
        if (select) then
          found = iq(llnefl+ielem) .ne. 0
        else
          found = iq(llnedr+ielem) .eq. jdir
        endif
 
*---- Determine position for insertion.
        if (found) then
          level = 0
          nxopr = 0
          if (itype(4) .ne. 0) then
            call excopy(llneat, ielem, idata)
            call excopy(lccmd, 3, mbat + 2*mcsiz + mcval)
            call exbin(1)
          else
            call excopy(lccmd, 3, mbat + 2*mcsiz + mcval)
          endif
          pos   = rsval(1)
          iexpr = isval(1) + 1
          call exmake(llntmp(4), jelem, jdata, pos, iexpr)
 
*---- Go to next position.
          jelem = jelem + 1
          jdata = jdata + mwflt
        endif
 
*---- Go to next original position.
        idata = idata + mwflt
   30 continue
 
*---- Pass 3: Merge the install banks with the original banks.
*     Original position is I.
      ielem = 2
      idata = mbat + mcsiz + 2
      call ucopy(q(llneat+idata), posi, mwflt)
*     Inserted position is J.
      jelem = 1
      jdata = 1
      call ucopy(q(llntmp(4)+jdata), posj, mwflt)
*     Merged position is K.
      kelem = 2
      kdata = mbat + mcsiz + 2
 
*---- As long as there are positions in either structure do...
   50 if (ielem .le. nelem  .or.  jelem .le. melem) then
 
*---- Original position comes first.
        if (jelem .gt. melem  .or.
     +      ielem .le. nelem .and. posi .le. posj) then
*     Position.
          call ucopy(posi, q(llntmp(1)+kdata), mwflt)
          lcexp = lq(llneat-ielem)
*     Directory index.
          iq(llntmp(2)+kelem) = iq(llnedr+ielem)
*     Selection flag.
          if (llnefl .ne. 0) iq(llntmp(3)+kelem) = iq(llnefl+ielem)
*     Go to next original position.
          ielem = ielem + 1
          idata = idata + mwflt
          call ucopy(q(llneat+idata), posi, mwflt)
 
*---- Inserted position comes first.
        else
*     Position.
          call ucopy(posj, q(llntmp(1)+kdata), mwflt)
          lcexp = lq(llntmp(4)-jelem)
*     Directory index.
          iq(llntmp(2)+kelem) = idir
*     Inserted position cannot be selected at this time.
          if (llnefl .ne. 0) iq(llntmp(3)+kelem) = 0
*     Go to next inserted position.
          jelem = jelem + 1
          jdata = jdata + mwflt
          call ucopy(q(llntmp(4)+jdata), posj, mwflt)
        endif
 
*---- Move expression, if any.
        if (lcexp .ne. 0) then
          call zshunt(0, lcexp, llntmp(1), -kelem, 0)
          ibias = mxsiz * iq(lcexp-3) + mxval
          iq(lcexp+ibias) = kdata
        endif
 
*---- Go to next merged position.
        kelem = kelem + 1
        kdata = kdata + mwflt
        go to 50
      endif
 
*---- Mark as modified.
      call aamark('LNEINS', llneat)
      write (msg, 910) melem
  910 format(i5,' element(s) inserted.')
      call aainfo('LNEINS', 1, msg)
 
*---- Remove old sequence and insert banks.
*     Keep directory index.
      idir = iq(llnesq+mbnam)
*     Drop old sequence.
      call aadrop(llnesq)
*     Drop insertions bank.
      call mzdrop(0, llntmp(4), ' ')
*     Store sequence size.
      iq(llntmp(2)+1) = nelem + melem
*     Link to directory.
      iq(llntmp(1)+mbnam) = idir
      lq(ldbnk(3)-idir) = llntmp(1)
*     Current SEQEDIT sequence.
      llnesq = llntmp(1)
 
 9999 end
