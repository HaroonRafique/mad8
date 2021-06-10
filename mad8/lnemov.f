      subroutine lnemov
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Move element in a beam line sequence, MOVE command.                *
* Attributes:                                                          *
*   ELEMENT  (name)    Name of element to be moved.
*
*   BY       (real)    If given, BY is displacement relative to the    *
*                      original position, otherwise use TO/FROM.       *
*   TO       (real)    Position for new element.                       *
*   FROM     (name)    If given, TO is relative to this element,       *
*                      otherwise TO is absolute position.              *
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
      integer idata,idir,ielem,iexpr,ilen,jbias,jdata,jdir,jelem,kdata,
     +kdir,kelem,melem,nelem
      double precision oldpos,pos,refrr
 
      character*(mcnam) elmnam, frmnam
      logical           found, select
      integer           itype(4)
 
*---- Retrieve data flags and bank pointers.
      call utgtyp(lccmd, itype)
      llneat = llnesq
      llnedr = lq(llneat-1)
      llnefl = lq(llnedr-1)
 
*---- Retrieve element name
      elmnam = ' '
      call utgnam(lccmd, 1, 1, elmnam)
      select = .false.
      if (elmnam .eq. ' ') then
        call aafail('LNEMOV', 1, 'Need element name or "SELECTED".')
        go to 9999
 
*---- "SELECTED" --> remove all selected elements
      else if (elmnam .eq. 'SELECTED') then
 
*---- Should have made a selection.
        if (llnefl .eq. 0) then
          call aafail('LNEMOV', 1, 'You have made no selection yet.')
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
          call aafail('LNEMOV', 1, msg(1))
          error = .true.
        endif
      endif
 
*---- Check other attributes.
      if (select  .or.  itype(2) .ne. 0) then
        if (itype(3) .ne. 0  .or.  itype(4) .ne. 0) then
          call aafail('LNEMOV', 1, 'Inconsistent attributes given.')
          error = .true.
        endif
      endif
 
*---- Look up "FROM" element name in directory.
      call utgnam(lccmd, 4, 4, frmnam)
      if (itype(4) .ne. 0) then
        call utleng(frmnam, ilen)
        call difind(ldbnk, frmnam(1:ilen), kdir, lcelm)
        if (kdir .eq. 0  .or.  iq(lcelm+mbpr) .ne. mpelm) then
          msg(1) = 'Element "' // frmnam(1:ilen) // '" is unknown.'
          call aafail('LNEMOV', 1, msg)
          error = .true.
        endif
      endif
 
*---- Retrieve data for sequence to be edited.
      if (error) go to 9999
      nelem = iq(llnedr+1)
      melem = 0
      idata = mbat + mcsiz + 2
 
*---- Loop over sequence and move element(s).
      do 90 ielem = 2, nelem
        if (select) then
          found = iq(llnefl+ielem) .ne. 0
        else
          found = iq(llnedr+ielem) .eq. idir
        endif
 
*---- If found, move the element.
        if (found) then
          level = 0
          nxopr = 0
          if (itype(2) .ne. 0) then
            idata = mbat + mcsiz + (ielem - 2) * mwflt + 2
            call excopy(llneat, ielem, idata)
            call excopy(lccmd, 2, mbat + mcsiz + mcval)
            call exbin(1)
 
*---- "FROM" attribute.
          else
 
*---- Look up "FROM" element name in sequence.
            if (itype(4) .ne. 0) then
              do 10 kelem = 2, nelem
                if (iq(llnedr+kelem) .eq. kdir) go to 20
   10         continue
              msg(1) = 'Element "' // frmnam(1:ilen) //
     +                 '" not in sequence.'
              call aafail('LNEMOV', 1, msg)
              go to 9999
   20         continue
 
*---- Define origin of displacement.
              kdata = mbat + mcsiz + (kelem - 2) * mwflt + 2
              call excopy(llneat, kelem, kdata)
              call excopy(lccmd, 3, mbat + 2*mcsiz + mcval)
              call exbin(1)
            else
              call excopy(lccmd, 3, mbat + 2*mcsiz + mcval)
            endif
          endif
          pos   = rsval(1)
          iexpr = isval(1) + 1
 
*---- Save old position in temporary.
          jdir = iq(llnedr+ielem)
          idata = mbat + mcsiz + (ielem - 2) * mwflt + 2
          call ucopy(q(llneat+idata), oldpos, mwflt)
 
*---- Remove old position expression, if any.
          lcexp = lq(llneat-ielem)
          if (lcexp .ne. 0) call aadrop(lcexp)
 
*---- New position is lower than old one.
          jelem = ielem
          jdata = idata
          if (pos .lt. oldpos) then
 
*---- Move positions up to make room in the proper place.
   50       if (jelem .le. 2) go to 80
              call ucopy(q(llneat+jdata-mwflt), refrr, mwflt)
              if (refrr .lt. pos) go to 80
              call ucopy(refrr, iq(llneat+jdata), mwflt)
              iq(llnedr+jelem) = iq(llnedr+jelem-1)
              lcexp = lq(llneat-jelem+1)
              if (lcexp .ne. 0) then
                call zshunt(0, lcexp, llneat, -jelem, 0)
                jbias = mxsiz * iq(lcexp-3) + mxval
                iq(lcexp+jbias) = jdata
              endif
              jelem = jelem - 1
              jdata = jdata - mwflt
            go to 50
 
*---- New position is higher than old one.
          else
 
*---- Move positions down to make room in the proper place
   60       if (jelem .ge. nelem) go to 80
              call ucopy(q(llneat+jdata+mwflt), refrr, mwflt)
              if (refrr .gt. pos) go to 80
              call ucopy(refrr, iq(llneat+jdata), mwflt)
              iq(llnedr+jelem) = iq(llnedr+jelem+1)
              lcexp = lq(llneat-jelem-1)
              if (lcexp .ne. 0) then
                call zshunt(0, lcexp, llneat, -jelem, 0)
                jbias = mxsiz * iq(lcexp-3) + mxval
                iq(lcexp+jbias) = jdata
              endif
              jelem = jelem + 1
              jdata = jdata + mwflt
            go to 60
          endif
 
*---- Store new reference to the slot created.
   80     iq(llnedr+jelem) = jdir
          call exmake(llneat, jelem, jdata, pos, iexpr)
          melem = melem + 1
        endif
   90 continue
 
*---- Mark, if modified.
      if (melem .gt. 0) then
        call aamark('LNEMOV', llneat)
        write (msg, 910) melem
  910   format(i5,' element(s) moved.')
        call aainfo('LNEMOV', 1, msg)
      else
        call aawarn('LNEMOV', 1, 'No element moved.')
      endif
 
 9999 end
