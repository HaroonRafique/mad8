      subroutine enlump
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for a MAKELUMP.                                      *
* Attributes:                                                          *
*   NAME      (name)    Name for the new lump.                         *
*   RANGE     (range)   Range for the lump.                            *
* Important common data:                                               *
*   LCELM     /REFER/   Current element bank.                          *
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
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
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer icode,idiff,iek,ienum,iflag,ifm,ifp,ihm,ihp,ikey,il,ilast,
     +ileng,iln,iocc,ipos,ipr,ire,irg1,irg2,irt,isave,isp,ite,itm,itp,
     +itt,jbit,jbyt,llast,lmap,ltemp,mxdrp,nkat,nord,ns
      double precision displ,el,orbl,orbt,suml
 
      parameter         (mxdrp = 9)
 
      character*(mcnam) elmnam, lmpnam
      dimension         displ(6), orbt(6), orbl(6)
      logical           fmap, radsav
 
*---- Check for presence of main beam line.
      call lnchck('ENLUMP', error)
      if (error) go to 9999
 
*---- Get name for LUMP.
      lmpnam = ' '
      call utgnam(lccmd, 1, 1, lmpnam)
      if (lmpnam .eq. ' ') then
        call aafail('ENLUMP', 1, 'LABEL for MAKELUMP missing.')
        go to 9999
      endif
 
*---- Get and check range.
      call utgrng(lq(lccmd-2), lcseq, irg1, irg2, error)
      if (error) then
        call aafail('ENLUMP', 1, 'Invalid range for MAKELUMP')
        go to 9999
      endif
 
*---- Initialize, test for validity.
      radsav = dorad
      if (dorad) then
        dorad = .false.
        call diname(ldbnk, iq(lcelm+mbnam), elmnam)
        call utleng(elmnam, ileng)
        msg(1) = 'LUMP "' // elmnam(1:ileng)
     +  // '" will ignore radiation.'
        call aawarn('ENLUMP', 1, msg)
      endif
 
*---- Extract relevant pointers.
      lsdir = lq(lcseq-msdir)
      lsflg = lq(lcseq-msflg)
      lsali = lq(lcseq-msali)
      lsfld = lq(lcseq-msfld)
      lsnum = lq(lcseq-msnum)
      lscom = lq(lcseq-mscom)
 
*---- Initialize polynomial package.
      call painit(6)
 
*---- Fetch LUMP order.
      nord = 4
      call utgint(lccmd, 3, 3, nord)
      nord = min(nord, 6)
 
*---- Allocate working store.
      isave = iwork
      iek = iwork
      ire = iek + 6
      ite = ire + 36
      irt = ite + 216
      itt = irt + 36
      ifm = itt + 216
      ifp = ifm + 36
      ihm = ifp + itop6(nord)
      ihp = ihm + 36
      itm = ihp + itop6(nord)
      itp = itm + 36
      iwork = itp + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Set up identity maps.
      suml = 0.0
      call uzero(orbt, 1, 6*mwflt)
      call m66one(dq(irt+1))
      call uzero(dq(itt+1), 1, 216*mwflt)
      call uzero(orbl, 1, 6*mwflt)
      call lmone(nord, dq(ifp+1), dq(ifm+1))
 
*---- Build the desired lump.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        icode = jbyt(iflag,1,mcode)
 
*---- Misalignment at entrance.
        if (icode .ne. 3  .and.  lcali .ne. 0) then
          call tmali1(ipos, .true., orbt, orbt,
     +      dq(ire+1), dq(ite+1))
          call tmcat(.true., dq(ire+1), dq(ite+1), dq(irt+1),
     +      dq(itt+1), dq(irt+1), dq(itt+1))
          call ucopy(q(lcali+1), displ, 6 * mwflt)
          call lmdsp1(ipos, nord, displ, dq(itp+1), dq(itm+1))
          call lamove(nord, dq(itp+1), dq(itm+1), orbl,
     +                dq(ihp+1), dq(ihm+1))
          call lmcat(nord, dq(ifp+1), dq(ifm+1), dq(ihp+1), dq(ihm+1),
     +               dq(ifp+1), dq(ifm+1))
        endif
 
*---- Physical element.
        if (icode .eq. 1) then
          call tmmap(.true., .true., orbt, fmap, el,
     +               dq(iek+1), dq(ire+1), dq(ite+1))
          if (fmap) then
            call tmcat(.true., dq(ire+1), dq(ite+1),
     +        dq(irt+1), dq(itt+1), dq(irt+1), dq(itt+1))
            suml = suml + el
          endif
          call lmmap(nord, el, dq(itp+1), dq(itm+1))
          call lamove(nord, dq(itp+1), dq(itm+1), orbl,
     +                dq(ihp+1), dq(ihm+1))
          call lmcat(nord, dq(ifp+1), dq(ifm+1), dq(ihp+1), dq(ihm+1),
     +               dq(ifp+1), dq(ifm+1))
        endif
 
*---- Misalignment at exit.
        if (icode .ne. 2  .and.  lcali .ne. 0) then
          call tmali2(ipos, .true., orbt, orbt,
     +      dq(ire+1), dq(ite+1))
          call tmcat(.true., dq(ire+1), dq(ite+1), dq(irt+1),
     +      dq(itt+1), dq(irt+1), dq(itt+1))
          call ucopy(q(lcali+1), displ, 6 * mwflt)
          call lmdsp2(ipos, nord, displ, dq(itp+1), dq(itm+1))
          call lamove(nord, dq(itp+1), dq(itm+1), orbl,
     +                dq(ihp+1), dq(ihm+1))
          call lmcat(nord, dq(ifp+1), dq(ifm+1), dq(ihp+1), dq(ihm+1),
     +               dq(ifp+1), dq(ifm+1))
        endif
 
*---- Remove banks linked to positions within range.
*       Misalignments.
        if (lsali .ne. 0  .and.  lq(lsali-ipos) .ne. 0) then
          call mzdrop(0, lq(lsali-ipos), '.')
        endif
*       Field errors.
        if (lsfld .ne. 0  .and.  lq(lsfld-ipos) .ne. 0) then
          call mzdrop(0, lq(lsfld-ipos), '.')
        endif
*       Orbit correction data: Just flag and drop later
        if (lscom .ne. 0  .and.  lq(lscom-ipos) .ne. 0) then
          call mzflag(0, lq(lscom-ipos), mxdrp, '.')
        endif
   90 continue
 
*---- Remove flagged element positions.
      llast = 0
      lccom = lq(lcseq-mselm)
  110 if (lccom .ne. 0) then
        ltemp = lq(lccom-1)
        if (jbit(iq(lccom), mxdrp) .ne. 0) then
          if (llast .eq. 0) then
            lq(lcseq-mselm) = ltemp
          else
            lq(llast-1) = ltemp
          endif
          call mzdrop(0, lccom, '.')
        else
          llast = lccom
        endif
        lccom = ltemp
        go to 110
      endif
 
*---- Remove flagged corrector positions.
      ncor(1) = 0
      ncor(2) = 0
      llast = 0
      lccom = lq(lcseq-mscor)
  120 if (lccom .ne. 0) then
        ltemp = lq(lccom-1)
        if (jbit(iq(lccom), mxdrp) .ne. 0) then
          if (llast .eq. 0) then
            lq(lcseq-mselm) = ltemp
          else
            lq(llast-1) = ltemp
          endif
          call mzdrop(0, lccom, '.')
        else
          llast = lccom
          if (jbit(iq(lccom), 1) .ne. 0) ncor(1) = ncor(1) + 1
          if (jbit(iq(lccom), 2) .ne. 0) ncor(2) = ncor(2) + 1
        endif
        lccom = ltemp
        go to 120
      endif
 
*---- Remove flagged monitor positions.
      nmon(1) = 0
      nmon(2) = 0
      llast = 0
      lccom = lq(lcseq-msmon)
  130 if (lccom .ne. 0) then
        ltemp = lq(lccom-1)
        if (jbit(iq(lccom), mxdrp) .ne. 0) then
          if (llast .eq. 0) then
            lq(lcseq-mselm) = ltemp
          else
            lq(llast-1) = ltemp
          endif
          call mzdrop(0, lccom, '.')
        else
          llast = lccom
          if (jbit(iq(lccom), 1) .ne. 0) nmon(1) = nmon(1) + 1
          if (jbit(iq(lccom), 2) .ne. 0) nmon(2) = nmon(2) + 1
        endif
        lccom = ltemp
        go to 130
      endif
 
*---- Build new LUMP element.
      call difind(ldkey, 'LUMP', ikey, lckey)
      call kwget(lckey, iln, ipr, isp, nkat)
      call aabook(lcelm, lmpnam(1:4), ipr, isp, lckey, 1)
      call didefi(ldbnk, lmpnam, lcelm)
      iq(lcelm+mbat+2*mcsiz+mcval) = nord
 
*---- Store TRANSPORT map.
      il = iq(lcelm+mbat) + mbemap
      ns = (6 + 36 + 216) * mwflt
      call mzbook(2, lmap, lcelm, -il, 'LMAP', 0, 0, ns, mreal, 0)
      call utpflt(lcelm, 2, 2, suml)
      call ucopy(orbt, q(lmap+1), 6*mwflt)
      call ucopy(dq(irt+1), q(lmap+6*mwflt+1), 36*mwflt)
      call ucopy(dq(itt+1), q(lmap+42*mwflt+1), 216*mwflt)
 
*---- Store Lie map.
      il = iq(lcelm+mbat) + mbelie
      ns = (36 + itop6(nord)) * mwflt
      call mzbook(2, lmap, lcelm, -il, 'LMAP', 0, 0, ns, mreal, 0)
      call ucopy(dq(ifm+1), q(lmap+1), 36*mwflt)
      call ucopy(dq(ifp+1), q(lmap+36*mwflt+1), itop6(nord)*mwflt)
 
*---- Replace RANGE by new LUMP.
      iq(lsdir+irg1) = iq(lcelm+mbnam)
      iq(lsflg+irg1) = 1
      call sbyt(1, iq(lsflg+irg1), mocc1, mocc2)
      iq(lsnum+irg1) = 1
 
*---- Move information for subequent elements.
      ilast = irg1
      do 150 ipos = irg2 + 1, iq(lsflg-1)
        ilast = ilast + 1
        iq(lsdir+ilast) = iq(lsdir+ipos)
        iq(lsflg+ilast) = iq(lsflg+ipos)
*       Misalignments.
        if (lsali .ne. 0) then
          if (lq(lsali-ipos) .ne. 0) then
            call zshunt(0, lq(lsali-ipos), lsali, -ilast, 0)
            iq(lq(lsali-ilast)-5) = ilast
          else
            lq(lsali-ilast) = 0
          endif
        endif
*       Field errors.
        if (lsfld .ne. 0) then
          if (lq(lsfld-ipos) .ne. 0) then
            call zshunt(0, lq(lsfld-ipos), lsfld, -ilast, 0)
            iq(lq(lsfld-ilast)-5) = ilast
          else
            lq(lsfld-ilast) = 0
          endif
        endif
*       Orbit correction data.
        if (lscom .ne. 0) then
          if (lq(lscom-ipos) .ne. 0) then
            call zshunt(0, lq(lscom-ipos), lscom, -ilast, 0)
            iq(lq(lscom-ilast)-5) = ilast
          else
            lq(lscom-ilast) = 0
          endif
        endif
  150 continue
 
*---- Release unused space.
      idiff = ilast - iq(lsflg-1)
      call mzpush(0, lsdir, 0, idiff, 'I')
      call mzpush(0, lsflg, 0, idiff, 'I')
      call mzpush(0, lsnum, 0, idiff, 'I')
      if (lsali .ne. 0) call mzpush(0, lsali, idiff, 0, 'I')
      if (lsfld .ne. 0) call mzpush(0, lsfld, idiff, 0, 'I')
      if (lscom .ne. 0) call mzpush(0, lscom, idiff, 0, 'I')
 
*---- Update USED range.
      if (irg2 .lt. iq(lcseq+msr1)) then
*       MAKELUMP range completely below USED range.
        iq(lcseq+msr1) = iq(lcseq+msr1) + idiff
        iq(lcseq+msr2) = iq(lcseq+msr2) + idiff
      else if (irg1 .gt. iq(lcseq+msr2)) then
*       MAKELUMP range completely above USED range.
        continue
      else if (irg1 .ge. iq(lcseq+msr1)  .and.
     +         irg2 .le. iq(lcseq+msr2)) then
*       MAKELUMP range completely within USED range.
        iq(lcseq+msr2) = iq(lcseq+msr2) + idiff
      else
*       MAKELUMP range contains one or both ends of USED range.
*       Reset USED range to complete USED line.
        iq(lcseq+msr1) = 1
        iq(lcseq+msr2) = ilast
        msg(1) =
     +    'MAKELUMP range overlaps one or both ends of USED range.'
        msg(2) = 'Please reselect a new range for computation with:'
        msg(3) = '     USE, RANGE=<range>'
        call aawarn('ENLUMP', 3, msg)
      endif
 
*---- Must recompute one-turn map.
      newmap = .true.
 
*---- Restore radiation flag.
      dorad = radsav
 
*---- Release working store.
      iwork = isave
 
 9999 end
