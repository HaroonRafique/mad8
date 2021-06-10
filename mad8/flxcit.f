      subroutine flxcit(iopt)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set parameter definitions from a TFS file.                         *
*   EXCITE or INCREMENT command.                                       *
* Attributes:                                                          *
*   TFSFILE   (string)  Stream name to be read.                        *
*   NAME      (name)    Name of column containing parameter names.     *
*   VALUE     (name)    Name of column for settings or increments.     *
* Input:                                                               *
*   IOPT      (integer) 1: EXCITE, 2: INCREMENT.                       *
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
      parameter         (memmin =   50 000)
      parameter         (memlen =  500 000)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer lflbuf,lfltab
 
*---- Links for closed orbit correction module.
      common /fllink/   lfltab, lflbuf
      save              /fllink/
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer mtbact,mtbbky,mtbbuf,mtbcfm,mtbcnm,mtbcol,mtbcps,mtbdsc,
     +mtbf1,mtbf2,mtbfst,mtblst,mtbmod,mtbnam,mtbrow,mtbseg,mtbsiz,
     +mtbsky,mtbwid
 
*---- Parameters for table manager bank structure.
      parameter         (mtbact = 1, mtbbuf = 2, mtbmod = 1)
      parameter         (mtbf1  = 1,
     +                   mtbseg = 2, mtbrow = 3, mtbcol = 4, mtbwid = 5,
     +                   mtbf2  = 6,
     +                   mtbnam = 7, mtbsiz = mtbnam + mwnam - 1)
      parameter         (mtbsky = 2, mtbbky = 3, mtbcnm = 4,
     +                   mtbcfm = 5, mtbcps = 6, mtbdsc = 7,
     +                   mtbfst = 8, mtblst = 9)
      integer ibias1,ibias2,ibias3,idir,iform1,iform2,iform3,ikat,ikey,
     +ileng,iln,iopt,ipr,isp,iunit,jleng,jrow,leng,nkat,nrow,nset
      double precision factor,valnew,valold
 
      character*(mcnam) colnam(3), parnam, attnam, tabnam
      character*(mcfil) strnam, filnam
      logical           attflg
 
*---- Retrieve attributes.
      strnam = 'excite'
      call utgstr(lccmd, 1, 1, strnam)
      colnam(1) = 'NAME'
      colnam(2) = 'ATTRIBUTE'
      colnam(3) = 'VALUE'
      call utgnam(lccmd, 2, 4, colnam)
      factor = 1.0
      call utgflt(lccmd, 5, 5, factor)
 
*---- Read table file.
      call flopen(strnam, 'SRFD', 0, 0, iunit, error)
      if (.not. error) then
        call flname(iunit, filnam)
        tabnam = '*TEMP TABLE*'
        call tbrtfs(tabnam, iunit)
        call flclos(iunit, error)
 
*---- Find name and value columns.
        call tbopen(tabnam, 1, lfltab)
        call utleng(colnam(1), leng)
        call tbcol(lfltab, colnam(1)(1:leng), iform1, ibias1)
        if (iform1 .ne. 5) then
          call utleng(colnam(1), ileng)
          if (iform1 .eq. 0) then
            msg(1) = 'Column "' // colnam(1)(1:ileng) // '" not found.'
          else
            msg(1) =
     +      'Column "' // colnam(1)(1:ileng) // '" should be string.'
          endif
          call aafail('FLXCIT', 1, msg)
        endif
 
        call utleng(colnam(2), leng)
        call tbcol(lfltab, colnam(2)(1:leng), iform2, ibias2)
        attflg = iform2 .eq. 5
 
        call utleng(colnam(3), leng)
        call tbcol(lfltab, colnam(3)(1:leng), iform3, ibias3)
        if (iform3 .ne. 3  .and.  iform3 .ne. 4) then
          call utleng(colnam(3), ileng)
          if (iform3 .eq. 0) then
            msg(1) = 'Column "' // colnam(3)(1:ileng) // '" not found.'
          else
            msg(1) =
     +      'Column "' // colnam(3)(1:ileng) // '" should be real.'
          endif
          call aafail('FLXCIT', 1, msg)
        endif
 
*---- Find 'PARAMETER' keyword.
        nset = 0
        if (.not. error) then
          call difind(ldkey, 'PARAMETER', ikey, lckey)
          nrow = iq(lfltab+mtbrow)
 
*---- Loop over table rows.
          do 90 jrow = 1, nrow
            call tbset(lfltab, jrow, 1, lflbuf)
            call uhtoc(q(lflbuf+ibias1+1), mcwrd, parnam, mcnam)
            call uhtoc(q(lflbuf+ibias2+1), mcwrd, attnam, mcnam)
            if (iform3 .eq. 3) then
              valnew = q(lflbuf+ibias3+1)
            else
              call ucopy(q(lflbuf+ibias3+1), valnew, mwflt)
            endif
 
*---- Skip unset values.
            if (parnam .eq. '~'  .or.  valnew .gt. 0.9 * fltmax) then
 
*---- Find parameter PARNAM, or make a new one.
            else if (attnam .eq. ' ') then
              call direfe(ldbnk, parnam, idir)
              lcelm = lq(ldbnk(3)-idir)
              if (lcelm .eq. 0) then
                call kwget(lckey, iln, ipr, isp, nkat)
                call aabook(lcelm, parnam, ipr, isp, lckey, 1)
                lq(ldbnk(3)-idir) = lcelm
                iq(lcelm+mbnam) = idir
              endif
 
*---- For INCREMENT command get old value.
              if (iopt .eq. 2) then
                call utgflt(lcelm, 1, 1, valold)
                valnew = valold + valnew * factor
              endif
 
*---- Store new value and mark bank as modified.
              call utpflt(lcelm, 1, 1, valnew)
              call utleng(parnam, ileng)
              nset = nset + 1
              lcexp = lq(lcelm-1)
              if (lcexp .ne. 0) then
                msg(1) = 'Parameter "' // parnam(1:ileng) //
     +                   '" becomes independent.'
                call aawarn('FLXCIT', 1, msg(1))
                call aadrop(lcexp)
              endif
 
*---- Define dump option.
              if (ideffl .eq. 1  .or.  ideffl .eq. 3) then
                call aadump(lcelm)
              endif
              if (ideffl .eq. 2  .or.  ideffl .eq. 3) then
                call dzshow('parameter', 0, lcelm, 'V', 0, 0, 0, 0)
              endif
 
*---- Try to find a known bank with known attribute.
            else if (attflg) then
              call difind(ldbnk, parnam, idir, lcelm)
              if (lcelm .eq. 0) then
                write (msg, 910) parnam
  910           format('Unknown bank name "',a,'".')
                call aafail('FLXCIT', 1, msg)
              else
                call utleng(parnam, ileng)
                call kwget(lq(lcelm+1), iln, ipr, isp, nkat)
                call utleng(attnam, jleng)
                call utlook(attnam(1:jleng), katnam, nkat, ikat)
                if (ikat .eq. 0) then
                  write (msg, 920) parnam(1:ileng), attnam(1:jleng)
  920           format('Unknown attribute "',a,'[',a,']".')
                  call aafail('FLXCIT', 1, msg)
                else if (iatype(ikat) .ne. mtflt) then
                  write (msg, 930) parnam(1:ileng), attnam(1:jleng)
  930           format('Attribute "',a,'[',a,']" is not real.')
                  call aafail('FLXCIT', 1, msg)
                else
                  if (iopt .eq. 2) then
                    call utgflt(lcelm, ikat, ikat, valold)
                    valnew = valold + valnew * factor
                  endif
                  call utpflt(lcelm, ikat, ikat, valnew)
                  nset = nset + 1
                  lcexp = lq(lcelm-1)
                  if (lcexp .ne. 0) then
                    msg(1) = 'Attribute "' // parnam(1:ileng) // '[' //
     +                      attnam(1:jleng) // ']" becomes independent.'
                    call aawarn('FLXCIT', 1, msg(1))
                    call aadrop(lcexp)
                  endif
                endif
              endif
            endif
   90     continue
        endif
 
*---- All done. Delete table from storage.
        call tbdrop(lfltab)
        write (msg, 940) nset, filnam
  940   format(i5,' Parameters set from file: ',a)
        call aainfo('FLXCIT', 1, msg)
      endif
 
      end
