      subroutine tblist
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   List a TFS table, TABLE command.                                   *
* Attribute:                                                           *
*   NAME       (name)    Table to be listed.                           *
*   COLUMN(*)  (string)  Column expressions.                           *
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
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
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
      integer icurr,itabun,itbbuf,itbfil,itbspc,koff,ltable,ltbbuf,
     +ltbcol,ltbcur,ltbdsc,ltbsav,ltbspc,ltbsum,ltbtab,ltbtmp,nblock,
     +nbout,ncmax,nrbmod
 
*---- Communication area for table manager routines.
      integer mleng,mnblck,mstep
      parameter         (mnblck=10, mleng=512*mnblck, mstep=100)
      common /tbcomm/   ltable, ltbcol, ltbsum,
     +                  ltbbuf, ltbspc, ltbdsc, ltbtab, ltbcur, ltbsav,
     +                  ltbtmp,
     +                  nblock, nbout, nrbmod, icurr, ncmax, itbspc,
     +                  koff, itbfil, itabun, itbbuf(mleng,2)
      save              /tbcomm/
      integer icfrm,iclen,maxcol
 
*---- Table header information.
      parameter         (maxcol = 100)
      common /tbhedc/   cname(maxcol), cform(maxcol)
      common /tbhedf/   icfrm(maxcol), iclen(maxcol)
      save              /tbhedc/, /tbhedf/
      character*20      cname, cform
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer icol,ifrm,il,iln,ipos,ipr,irow,iseg,isp,isum,j,
     +ncol,nkat,nl,nsum
      double precision colval,rval,sumval
 
      character*(mcnam) tbname, colnam(100), sumnam(100), xname
      dimension         colval(100), sumval(100)
      integer           iform(100), ibias(100)
      logical           eflag
 
*---- Decode table name.
      tbname = ' '
      call utgnam(lccmd, 1, 1, tbname)
      call tbopen(tbname, 1, ltbtab)
      if (ltbtab .eq. 0) go to 9999
 
*---- Get keyword dimensions.
      call kwget(lckey, iln, ipr, isp, nkat)
 
*---- Book bank for columns.
      ltbcol = 0
      nl = iadim1(2)
      call mzbook(2, ltbcol, ltbcol, 1, 'TEXP', nl, nl, 0, mreal, 0)
 
*---- Fetch column names.
      do 10 icol = 1, nl
        colnam(icol) = ' '
   10 continue
      call utgnam(lccmd, 2, iadim1(2)+1, colnam)
 
*---- Decode column names.
      ncol = 0
      do 20 icol = 1, nl
        if (colnam(icol) .ne. ' ') then
          ncol = ncol + 1
          call tbcol(ltbtab, colnam(icol), iform(icol), ibias(icol))
          if (iform(icol) .eq. 0) then
            call exstrg(colnam(icol), ltbtab, ltbcol, -ncol,
     +                  colval(icol), eflag)
          endif
        endif
   20 continue
 
*---- Book bank for sums.
      ltbsum = 0
      nl = iadim1(3)
      call mzbook(2, ltbsum, ltbsum, 1, 'TSUM', nl, nl, 0, mreal, 0)
 
*---- Fetch sum names.
      do 30 isum = 1, nl
        sumnam(isum) = ' '
   30 continue
      call utgnam(lccmd, iadim1(2)+2, iadim1(2)+iadim1(3)+1, sumnam)
 
*---- Decode expression names.
      nsum = 0
      do 40 isum = 1, nl
        if (sumnam(isum) .ne. ' ') then
          nsum = nsum + 1
          call exstrg(sumnam(isum), ltbtab, ltbsum, -nsum, rval, eflag)
          sumval(nsum) = 0.0
        endif
   40 continue
 
*---- Quit in case of error.
      if (error  .or.  ncol + nsum .eq. 0) go to 9999
 
*---- Loop over table segments.
      do 190 iseg = 1, iq(ltbtab+mtbseg)
        call tbseg(ltbtab, iseg, eflag)
 
*---- Table header.
        if (.not. eflag) then
          if (ncol .ne. 0) then
            call prpage(iqpr2)
            write (iqpr2, 910) tbname, iseg
            call prline(iqpr2)
            write (iqpr2, 920) (colnam(j), j = 1, ncol)
            call prline(iqpr2)
          endif
 
*---- Loop over table rows.
          do 180 irow = 1, iq(ltbtab+mtbrow)
            call tbset(ltbtab, irow, 1, ltbbuf)
            if (ltbbuf .ne. 0) then
              do 160 icol = 1, ncol
                ifrm = iform(icol)
                ipos = ibias(icol)
 
                if (ifrm .eq. 0) then
                  call exevl1(lq(ltbcol-icol), ltbtab, ltbbuf, rval)
                  write (cname(icol), '(G20.12)') rval
                else if (ifrm .le. 2) then
                  write (cname(icol), '(I8)') iq(ltbbuf+ipos+1)
                else if (ifrm .eq. 3) then
                  write (cname(icol), '(G14.6)') q(ltbbuf+ipos+1)
                else if (ifrm .eq. 4) then
                  call ucopy(q(ltbbuf+ipos+1), rval, mwflt)
                  write (cname(icol), '(G20.12)') rval
                else if (ifrm .eq. 5) then
                  call uhtoc(q(ltbbuf+ipos+1), mcwrd, xname, mcnam)
                  call utleng(xname, il)
                  cname(icol) = '  "' // xname(1:il) // '"'
                endif
  160         continue
 
              write (iqpr2, 940) irow, (cname(icol), icol = 1, ncol)
 
*---- Evaluate sum expressions.
              do 170 isum = 1, nsum
                if (lq(ltbsum-isum) .ne. 0) then
                  call exevl1(lq(ltbsum-isum), ltbtab, ltbbuf, rval)
                  sumval(isum) = sumval(isum) + rval
                endif
  170         continue
            endif
  180     continue
        endif
  190 continue
 
*---- Write sums.
      if (nsum .ne. 0) then
        call prline(iqpr2)
        do 290 isum = 1, nsum
          if (double) then
            write (iqpr2, 960) sumnam(isum), sumval(isum)
          else
            write (iqpr2, 970) sumnam(isum), sumval(isum)
          endif
  290   continue
        call prline(iqpr2)
      endif
 
*---- Close table.
      call tbclos(ltbtab)
 
*---- Drop expression banks.
*     Since expressions are not linked, no need to call AADROP.
      call mzdrop(0, ltbcol, 'L')
      call mzdrop(0, ltbsum, 'L')
 
  910 format(' Listing of table "',a,'", segment no. ',i8)
  920 format((t10,6(4x,a16):))
  940 format(' ',i8,' ',6a20:/(t11,6a20:))
  960 format(' ',a,' = ',g20.12)
  970 format(' ',a,' = ',g12.6)
 
 9999 end
