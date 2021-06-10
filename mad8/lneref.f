      subroutine lneref
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Invert order of elements in sequence, REFLECT command.             *
*   Last element must be marker.                                       *
* No attributes.                                                       *
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
      integer idata,idir,ielem,iexpr,irefer,ix,jdata,jelem,ndata,nelem,
     +nx1,nx2
      double precision rival
 
      character*(mcnam) refdic(3), refnam
 
      data refdic       / 'ENTRY', 'CENTRE', 'EXIT' /
 
*---- Retrieve data for sequence to be edited.
      llnedr = lq(llnesq-1)
      llneat = llnesq
      nelem  = iq(llnedr+1)
 
*---- Check that last element is marker.
      idir  = iq(llnedr+nelem)
      lcelm = lq(ldbnk(3)-idir)
      if (iq(lcelm+mbpr) .ne. mpelm  .or. iq(lcelm+mbsp) .ne. 25) then
        call aafail('LNEREF', 1,
     +    'Last element in sequence should be a marker.')
 
*---- Reflect by interchanging from both ends.
      else
        idata = mbat + mcsiz + 2
        ielem = 2
        ndata = mbat + mcsiz + 2 + (nelem - 2) * mwflt
        jdata = ndata - mwflt
        jelem = nelem - 1
 
*---- Test for termination.
  100   if (ielem .lt. jelem) then
 
*---- Exchange elemens at positions I and J.
          idir = iq(llnedr+ielem)
          iq(llnedr+ielem) = iq(llnedr+jelem)
          iq(llnedr+jelem) = idir
 
*---- Build expressions for new positions (total length - position).
          level = 0
          nxopr = 0
          call excopy(llneat, nelem, ndata)
          call excopy(llneat, ielem, idata)
          call exbin(2)
          nx1 = nxopr
          call excopy(llneat, nelem, ndata)
          call excopy(llneat, jelem, jdata)
          call exbin(2)
          nx2 = nxopr
 
*---- Store new positions.
          nxopr = nx1
          call exmake(llneat, jelem, jdata, rsval(1), isval(1)+1)
          nxopr = nx2 - nx1
          do 10 ix = 1, nxopr
            ixopr(ix)  = ixopr(ix+nx1)
            ixsub1(ix) = ixsub1(ix+nx1)
            ixsub2(ix) = ixsub2(ix+nx1)
            ixsub3(ix) = ixsub3(ix+nx1)
            axbank(ix) = axbank(ix+nx1)
            axattr(ix) = axattr(ix+nx1)
            rxval(ix)  = rxval(ix+nx1)
   10     continue
          call exmake(llneat, ielem, idata, rsval(2), isval(2)+1)
 
*---- Next pair.
          ielem = ielem + 1
          idata = idata + mwflt
          jelem = jelem - 1
          jdata = jdata - mwflt
          go to 100
        endif
 
*---- Handle middle element, if any.
        if (ielem .eq. jelem) then
          level = 0
          nxopr = 0
          call excopy(llneat, nelem, ndata)
          call excopy(llneat, ielem, idata)
          call exbin(2)
          rival = rsval(1)
          iexpr = isval(1)
          call exmake(llneat, ielem, idata, rival, iexpr)
        endif
 
*---- Change reference according to REFER.
        call utgnam(llneat, 1, 1, refnam)
        call utlook(refnam, refdic, 3, irefer)
        if (irefer .ne. 0) call utpnam(llneat, 1, 1, refdic(4-irefer))
 
*---- Mark as modified.
        call aamark('LNEREF', llneat)
        call aainfo('LNEREF', 1,
     +    'Sequence has been reflected, end marker stays in place.')
      endif
 
 9999 end
