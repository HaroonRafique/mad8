      subroutine dclist(lline, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode a beam line list.                                           *
*   The pointer to the current formals list must be set before calling *
*   this routine (possibly zero).                                      *
* Input:                                                               *
*   LLINE(1) (pointer)  Beam line list bank.                           *
*   IDATA    (integer)  Bias for data block.                           *
* Output:                                                              *
*   EFLAG    (logical)  Error flag.                                    *
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
      integer icall,icell,ichar,icode,idirep,iform,ifree,ihead,iref,
     +irep,jform,leng,ml,nform
      integer           lline(*)
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
      integer mlact,mlf1,mlf2,mlfm,mlfree,mlhd,mlnxt,mlprv,mlref,mlrep,
     +mlsiz,mltyp
 
*---- Bias for beam line list information.
      parameter         (mlfm = mbat + 1, mlhd = mbat + 2,
     +                   mlf1 = mbat + 3, mlf2 = mbat + 4,
     +                   mlfree = mbat + 4)
 
*---- Bias for beam line list cells.
      parameter         (mltyp = 1, mlprv = 2, mlnxt = 3, mlrep = 4,
     +                   mlref = 5, mlact = 6, mlsiz = 6)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
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
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*(mcnam) elmnam, frmnam
      logical           sflag
      parameter         (ml = 100)
 
*---- Opening parenthesis.
      ldclin = lline(1)
      ldcfrm = lq(ldclin-1)
      eflag = .false.
      if (token(jtok) .ne. '(') then
        msg(1) = 'A beam line member should be an element,'
        msg(2) = 'a beam line name or a list in parentheses.'
        call rdfail('DCLIST', 2, msg)
        eflag = .true.
        go to 9999
      endif
 
*---- Reserve space for header information.
      ifree = iq(ldclin+mlhd)
 
*---- Prepare dummy lists for formals (look like sublists).
      if (lq(ldclin-1) .ne. 0) then
        nform = iq(lq(ldclin-1)-1)
        do 50 jform = 1, nform, mwnam
          if (ifree + 2*mlsiz .gt. iq(ldclin-1)) then
            call mzpush(0, ldclin, 0, ml*mlsiz, 'I')
          endif
          icell = ifree + mlsiz
          iq(ldclin+ifree+mltyp) = 2
          iq(ldclin+ifree+mlnxt) = icell
          iq(ldclin+ifree+mlprv) = icell
          iq(ldclin+icell+mlnxt) = ifree
          iq(ldclin+icell+mlprv) = ifree
          ifree = ifree + 2*mlsiz
   50   continue
        iq(ldclin+mlf1) = iq(ldclin+mlhd)
        iq(ldclin+mlf2) = ifree - 1
        iq(ldclin+mlhd) = ifree
      endif
 
*---- Initialize.
      icell = 0
      icall = 1
      icode = 1
 
*---- Procedure "DECODE LIST".
  100 continue
 
*---- Make header cell for current list.
        if (ifree + mlsiz .gt. iq(ldclin-1)) then
          call mzpush(0, ldclin, 0, ml*mlsiz, 'I')
        endif
        iq(ldclin+ifree+mltyp) = icode
        iq(ldclin+ifree+mlnxt) = ifree
        iq(ldclin+ifree+mlprv) = ifree
        ihead = ifree
        ifree = ifree + mlsiz
 
*---- Stack information to resume an outer list, if any.
        iq(ldclin+ihead+mlref) = icell
        iq(ldclin+ihead+mlact) = icall
 
*---- Set current cell.
        icell = ihead
 
*---- Append member cell.
  110   continue
        jtok = jtok + 1
        if (ifree + mlsiz .gt. iq(ldclin-1)) then
          call mzpush(0, ldclin, 0, ml*mlsiz, 'I')
        endif
        ihead = iq(ldclin+icell+mlnxt)
        iq(ldclin+ifree+mlnxt) = ihead
        iq(ldclin+ihead+mlprv) = ifree
        iq(ldclin+icell+mlnxt) = ifree
        iq(ldclin+ifree+mlprv) = icell
        icell = ifree
        ifree = ifree + mlsiz
 
*---- Reflection?
        if (token(jtok) .eq. '-') then
          jtok = jtok + 1
          idirep = -1
        else
          idirep = 1
        endif
 
*---- Repetition?
        if (ichtyp(ichar(token(jtok))) .le. 9) then
          call rdint(irep, sflag)
          if (sflag) go to 210
          call rdtest('*', sflag)
          if (sflag) go to 210
          jtok = jtok + 1
          idirep = idirep * irep
        endif
        iq(ldclin+icell+mlrep) = idirep
 
*---- Sublist?
        if (token(jtok) .ne. '(') go to 150
          icall = 2
          icode = 2
          go to 100
  120   continue
        iq(ldclin+icell+mlref) = ihead
        iq(ldclin+icell+mltyp) = 4
        go to 200
 
*---- Decode identifier.
  150   continue
        call rdword(elmnam, leng)
        if (leng .eq. 0) then
          msg(1) = 'A beam line member should be an element,'
          msg(2) = 'a beam line name or a list in parentheses.'
          call rdfail('DCLIST', 2, msg)
          go to 210
        endif
 
*---- Formal argument?
        if (ldcfrm .ne. 0) then
          nform = iq(ldcfrm-1)
          iform = iq(ldclin+mlf1)
          do 160 jform = 1, nform, mwnam
            call uhtoc(q(ldcfrm+jform), mcwrd, frmnam, mcnam)
            if (frmnam .eq. elmnam) then
              iq(ldclin+icell+mltyp) = 4
              iq(ldclin+icell+mlref) = iform
              go to 200
            endif
            iform = iform + 2*mlsiz
  160     continue
        endif
 
*---- Ordinary name.
        call direfe(ldbnk, elmnam, iref)
        iq(ldclin+icell+mltyp) = 6
        iq(ldclin+icell+mlref) = iref
 
*---- Actual argument list?
        if (token(jtok) .ne. '(') go to 190
          icall = 3
          icode = 1
          go to 100
  170     continue
          iq(ldclin+icell+mltyp) = 7
          iq(ldclin+icell+mlact) = ihead
  190   continue
 
*---- Comma or right parenthesis?
  200   continue
        if (token(jtok) .eq. ',') go to 110
        if (token(jtok) .eq. ')') go to 250
        call rdtest(',)', sflag)
 
*---- Error recovery.
  210   continue
        call rdfind('(),;')
        eflag = eflag .or. sflag
 
*---- Another member?
  250 continue
      if (token(jtok) .eq. ',') go to 110
 
*---- End of list?
      if (token(jtok) .eq. ')') then
        jtok = jtok + 1
 
*---- Move to header of current list.
        ihead = iq(ldclin+icell+mlnxt)
 
*---- Unstack information for outer list.
        icell = iq(ldclin+ihead+mlref)
        icall = iq(ldclin+ihead+mlact)
        iq(ldclin+ihead+mlref) = 0
        iq(ldclin+ihead+mlact) = 0
        go to (300, 120, 170), icall
      endif
 
*---- End of beam line list. Release unused space and link area.
  300 continue
      call mzpush(0, ldclin, 0, ifree - iq(ldclin-1), 'I')
      lline(1) = ldclin
 
 9999 end
