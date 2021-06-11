      subroutine tbwtfs(tnam, iunit)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Write a complete table in TFS format (ASCII or EBCDIC).            *
* Input:                                                               *
*   TNAM      (char)    Name of table to be written.                   *
*   IUNIT     (integer) Logical unit number to be written.             *
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
      integer iform,il,ileng,ipos,iunit,jcol,jleng,jrow,jseg,leng,lfm,
     +lnm,lps,ncol,nrow,nseg
      double precision rval
      character*(mcnam) tnam
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
 
      character*(mcnam) tname, xname
      character*99      sval
      logical           eflag
 
*---- Open table.
      call tbopen(tnam, 1, ltbtab)
      if (ltbtab .eq. 0) go to 9999
 
*---- Prepare column header lines and compute record length.
      lnm = lq(ltbtab-mtbcnm)
      lfm = lq(ltbtab-mtbcfm)
      ncol = iq(ltbtab+mtbcol)
      call ncopy(iq(lfm+1), icfrm, ncol)
      do 10 jcol = 1, ncol
        cname(jcol) = ' '
        call uhtoc(q(lnm+1), mcwrd, cname(jcol), mcnam)
        lnm = lnm + mwnam
        call utleng(cname(jcol), leng)
        if (icfrm(jcol) .eq. 1) then
          cform(jcol) = '%hd'
          iclen(jcol) = max(leng,8)
        else if (icfrm(jcol) .eq. 2) then
          cform(jcol) = '%hd'
          iclen(jcol) = max(leng,8)
        else if (icfrm(jcol) .eq. 3) then
          if (double) then
            cform(jcol) = '%le'
            iclen(jcol) = max(leng,20)
          else
            cform(jcol) = '%e'
            iclen(jcol) = max(leng,14)
          endif
        else if (icfrm(jcol) .eq. 4) then
          cform(jcol) = '%le'
          iclen(jcol) = max(leng,20)
        else if (icfrm(jcol) .eq. 5) then
          write (cform(jcol), '(''%'',I2.2,''s'')') mcnam
          iclen(jcol) = mcnam+2
        endif
   10 continue
 
*---- Write column header lines.
      write (iunit, 960) (cname(jcol)(1:iclen(jcol)), jcol = 1, ncol)
      write (iunit, 970) (cform(jcol)(1:iclen(jcol)), jcol = 1, ncol)
 
*---- Write table descriptor lines.
      ltbdsc = lq(ltbtab-mtbdsc)
   20 if (ltbdsc .ne. 0) then
        call uhtoc(q(ltbdsc+2), mcwrd, tname, mcnam)
        iform = mod(iq(ltbdsc+mwnam+2),16)
        if (iform .eq. 1) then
          if (iq(ltbdsc+mwnam+3) .eq. intmax) then
            write (iunit, 900) tname, '%hd '
          else
            write (iunit, 910) tname, '%hd ', iq(ltbdsc+mwnam+3)
          endif
        else if (iform .eq. 2) then
          if (iq(ltbdsc+mwnam+3) .eq. intmax) then
            write (iunit, 900) tname, '%hd '
          else
            write (iunit, 910) tname, '%hd ', iq(ltbdsc+mwnam+3)
          endif
        else if (iform .eq. 3) then
          if (q(ltbdsc+mwnam+3) .gt. 0.9 * fltmax) then
            write (iunit, 900) tname, '%e  '
          else if (double) then
            write (iunit, 930) tname, '%le ', q(ltbdsc+mwnam+3)
          else
            write (iunit, 920) tname, '%e  ', q(ltbdsc+mwnam+3)
          endif
        else if (iform .eq. 4) then
          call ucopy(q(ltbdsc+mwnam+3), rval, 2)
          if (rval .gt. 0.9 * fltmax) then
            write (iunit, 900) tname, '%le '
          else
            write (iunit, 930) tname, '%le ', rval
          endif
        else if (iform .eq. 5) then
          ileng = mcwrd * (iq(ltbdsc+mwnam+2) / 16)
          call uhtoc(q(ltbdsc+mwnam+3), mcwrd, sval, ileng)
          if (sval(1:ileng) .eq. '~') then
            write (iunit, 940) tname, ileng
          else
            call utleng(sval(1:ileng), jleng)
            write (iunit, 950) tname, ileng, sval(1:jleng)
          endif
        endif
        ltbdsc = lq(ltbdsc)
        go to 20
      endif
 
*---- Output table lines.
      nrow = iq(ltbtab+mtbrow)
      nseg = iq(ltbtab+mtbseg)
      lps = lq(ltbtab-mtbcps)
      do 90 jseg = 1, nseg
        call tbseg(ltbtab, jseg, eflag)
        if (.not. eflag) then
          if (nseg .gt. 1) write (iunit, 980) jseg, nseg, nrow
          do 80 jrow = 1, nrow
            call tbset(ltbtab, jrow, 1, ltbbuf)
            if (ltbbuf .ne. 0) then
              do 70 jcol = 1, ncol
                iform = icfrm(jcol)
                ipos = iq(lps+jcol)
                if (iform .le. 2) then
                  if (iq(ltbbuf+ipos+1) .eq. intmax) then
                    cname(jcol) = '       ~'
                  else
                    write (cname(jcol), '(I8)') iq(ltbbuf+ipos+1)
                  endif
                else if (iform .eq. 3) then
                  if (q(ltbbuf+ipos+1) .gt. 0.9 * fltmax) then
                    cname(jcol) = '   ~'
                  else if (double) then
                    write (cname(jcol), '(G20.12)') q(ltbbuf+ipos+1)
                  else
                    write (cname(jcol), '(G14.6)') q(ltbbuf+ipos+1)
                  endif
                else if (iform .eq. 4) then
                  call ucopy(q(ltbbuf+ipos+1), rval, 2)
                  if (rval .gt. 0.9 * fltmax) then
                    cname(jcol) = '   ~'
                  else
                    write (cname(jcol), '(G20.12)') rval
                  endif
                else if (iform .eq. 5) then
                  call uhtoc(q(ltbbuf+ipos+1), mcwrd, xname, mcnam)
                  call utleng(xname, il)
                  cname(jcol) = '"' // xname(1:il) // '"'
                endif
   70         continue
              write (iunit, 990)
     +          (cname(jcol)(1:iclen(jcol)), jcol = 1, ncol)
            endif
   80     continue
        endif
   90 continue
 
*---- Close TFS table.
      call tbclos(ltbtab)
 
  900 format('@ ',a,' ',a4,' ~')
  910 format('@ ',a,' ',a4,' ',i8)
  920 format('@ ',a,' ',a4,' ',g12.6)
  930 format('@ ',a,' ',a4,' ',g20.12)
  940 format('@ ',a,' %',i2.2,'s ~')
  950 format('@ ',a,' %',i2.2,'s "',a,'"')
  960 format('*',50(' ',a:))
  970 format('$',50(' ',a:))
  980 format('!Segment',3i8)
  990 format(' ',50(' ',a:))
 
 9999 end
