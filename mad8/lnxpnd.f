      subroutine lnxpnd(ibeam, lact, lseq)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Expand a beam line and build a beam sequence bank.                 *
*   Input is a beam line list and an actual argument list.             *
*   Directory index bank and flag bank is added to sequence.           *
* Input:                                                               *
*   IBEAM     (integer) Directory index for beam line to be expanded.  *
*   LACT(1)   (pointer) Bank containing actual arguments.              *
*   LSEQ(1)   (pointer) Beam line sequence bank.                       *
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
      integer i,iact,ibeam,ibnk,icel,idir,idirep,ihed,ileng,ilst,irep,
     +iseq,itype,msi,nerror,nrefl,nseq
      integer           lact(*), lseq(*)
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
 
      character*(mcnam) label
      parameter         (msi = 500)
 
*---- Set up necessary links.
      llnhed = lq(ldbnk(3)-ibeam)
      llnxsq = lseq(1)
      llnact = lact(1)
      nrefl  = 0
      nerror = 0
 
*---- LIST is invalid for expansion.
      if (iq(llnhed+mbsp) .eq. 3) then
        call utleng(label, ileng)
        msg(1) = 'LIST "' // label(1:ileng) // '" cannot be expanded.'
        call aafail('LNXPND', 1, msg)
        nerror = 1
 
*---- Reset "LIST" commands to their first members.
      else
        call lnxres
 
*---- Lift banks for beam sequence.
        call mzbook(2, lsdir, llnxsq, -msdir, 'SDIR', 0, 0, msi, 2, 0)
        call mzbook(2, lsflg, llnxsq, -msflg, 'SFLG', 0, 0, msi, 2, 0)
        nseq = msi
        iseq = 0
 
*---- Clear occurrence counts.
        call utclrc
 
*==== Expand a LINE.
        if (iq(llnhed+mbsp) .eq. 1) then
 
*---- Call procedure "EXPAND_LINE".
          ihed = iq(llnhed+mlhd)
          idir = 0
          irep = 0
          icel = 0
          ilst = ibeam
          lq(llnhed-2) = llnact
          idirep = 1
 
*==== Procedure "EXPAND_LINE(ILST, IDIREP)".
*---- Replace formal arguments, if any.
          call diname(ldbnk, ibeam, label)
          if (iq(llnhed+mbsp) .eq. 1) then
            if (iq(llnhed+mlf1) .ne. 0  .or.  llnact .ne. 0) then
              iact = iq(llnact+mlhd)
              call lnform(llnhed, label, llnact, iact)
            endif
          endif
 
*---- Stack information to resume an outer line.
  100     continue
          iq(llnhed+ihed+mlrep) = idir * irep
          iq(llnhed+ihed+mlref) = icel
          iq(llnhed+ihed+mlact) = ilst
 
*---- Set up for tracking through this line.
          idir = sign(1,idirep)
          irep = abs(idirep)
          icel = ihed
 
*---- If named line, store entry position.
  110     if (iq(llnhed+icel+mltyp) .eq. 1) then
            call lnxput(2, ilst, iseq, nseq)
          endif
 
*---- Next line member.
  120     if (idir .lt. 0) icel = iq(llnhed+icel+mlprv)
          if (idir .gt. 0) icel = iq(llnhed+icel+mlnxt)
          itype = iq(llnhed+icel+mltyp)
          go to (500, 510, 510, 200, 300, 400, 400), itype
 
*---- Sublist or formal argument reference:
*     Sublist resides in current bank.
  200     continue
          idirep = iq(llnhed+icel+mlrep) * idir
          if (idirep .eq. 0) go to 120
          ihed = iq(llnhed+icel+mlref)
          go to 100
 
*---- Sublist replacing formal argument:
*     Sublist resides in calling bank.
  300     continue
          idirep = iq(llnhed+icel+mlrep) * idir
          if (idirep .eq. 0) go to 120
          llncal = lq(llnhed-2)
          lq(llncal-3) = llnhed
          ihed = iq(llnhed+icel+mlref)
          ilst = ibnk
          llnhed = llncal
          go to 100
 
*---- Enter element, named line or list.
*     <element> | <line> | <line>(<args>) | <list> | <sequence>.
  400     continue
          idirep = iq(llnhed+icel+mlrep) * idir
          if (idirep .eq. 0) go to 120
          ibnk = iq(llnhed+icel+mlref)
          iact = iq(llnhed+icel+mlact)
          llncal = lq(ldbnk(3)-ibnk)
          call diname(ldbnk, ibnk, label)
          call utleng(label, ileng)
 
*---- Undefined item.
          if (llncal .eq. 0) then
            msg(1) = 'Undefined name "' // label(1:ileng)
     +      // '" encountered in beam line.'
            call aafail('LNXPND', 1, msg)
            nerror = nerror + 1
 
*---- Store element position.
          else if (iq(llncal+mbpr) .eq. mpelm) then
            do 410 i = 1, abs(idirep)
              call lnxput(1, ibnk, iseq, nseq)
  410       continue
 
*---- Beam line or list.
          else if (iq(llncal+mbpr) .eq. mplin) then
 
*---- Beam line with or without arguments.
            if (iq(llncal+mbsp) .eq. 1) then
 
*---- Protect against recursive entry to line.
              ihed = iq(llncal+mlhd)
              if (iq(llncal+ihed+mlrep) .ne. 0) then
                msg(1) = 'LINE "' // label(1:ileng)
     +          // '" has a recursive definition.'
                call aafail('LNXPND', 1, msg)
                nerror = nerror + 1
 
*---- Expand named line; switch context.
*     Sublist resides in called bank, actual arguments in current bank.
              else
                if (itype .eq. 7  .or. iq(llncal+mlf1) .ne. 0) then
                  call lnform(llncal, label, llnhed, iact)
                endif
                lq(llncal-2) = llnhed
                llnhed = llncal
                ilst = ibnk
                go to 100
              endif
 
*---- Sequence: Cannot be recursive.
            else if (iq(llncal+mbsp) .eq. 2) then
              call lnxseq(llncal, iseq, nseq)
 
*---- Replacement list: Test for reflection.
            else if (iq(llncal+mbsp) .eq. 3) then
              if (idir .lt. 0) nrefl = nrefl + 1
 
*---- Advance to next member of LIST.
              call lnxlst(idir)
 
*---- Switch context:
*     Sublist resides in LIST bank.
              idirep = 1
              ihed = mlfree
              lq(llncal-2) = llnhed
              llnhed = llncal
              ilst = ibnk
              go to 100
 
*---- Invalid subprocess code.
            else
              msg(1) = 'Name "' // label(1:ileng)
     +        // '" is invalid in a beam line.'
              call aafail('LNXPND', 1, msg)
              nerror = nerror + 1
            endif
 
*---- Invalid process code.
          else
            msg(1) = 'Name "' // label(1:ileng)
     +      // '" is invalid in a beam line.'
            call aafail('LNXPND', 1, msg)
            nerror = nerror + 1
          endif
          go to 120
 
*---- List header reached: Store exit position.
  500     continue
          ilst = iq(llnhed+icel+mlact)
          call lnxput(3, ilst, iseq, nseq)
 
*---- Repetitions wanted?
  510     continue
          irep = irep - 1
          if (irep .gt. 0) go to 110
 
*---- End of line: Switch context to referring line.
          ihed = icel
          idirep = iq(llnhed+ihed+mlrep)
          icel = iq(llnhed+ihed+mlref)
          ilst = iq(llnhed+ihed+mlact)
          iq(llnhed+ihed+mlrep) = 0
          iq(llnhed+ihed+mlref) = 0
          iq(llnhed+ihed+mlact) = 0
          if (itype .eq. 1) then
            llnhed = lq(llnhed-2)
          else if (itype .eq. 3) then
            llnhed = lq(llnhed-3)
          endif
 
*---- Is there an outer line?
          idir = sign(1,idirep)
          irep = abs(idirep)
          if (icel .ne. 0) go to 120
 
*==== Expand a SEQUENCE.
        else if (iq(llnhed+mbsp) .eq. 2) then
          call lnxseq(llnhed, iseq, nseq)
        endif
 
*---- Release unused space.
        call mzpush(0, lsdir, 0, iseq - nseq, 'I')
        call mzpush(0, lsflg, 0, iseq - nseq, 'I')
      endif
 
*---- Terminating message.
      if (nerror .ne. 0) then
        call diname(ldbnk, ibeam, label)
        call utleng(label, ileng)
        write (msg, 910) label(1:ileng), nerror
  910   format('Beam line "',a,'" could not be expanded, ',
     +         i5,' error(s) detected.')
        call aafail('LNXPND', 1, msg)
      else if (nrefl .ne. 0) then
        call aawarn('LNXPND', 1,
     +  'Replacement lists cannot be reflected.')
      endif
 
 9999 end
