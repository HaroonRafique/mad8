      subroutine plmain (ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Switch routine (subprocess code) for plot section.                 *
* Input:                                                               *
*   IPR       (integer) Process code.                                  *
*   ISP       (integer) Subprocess code.                               *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer maux,maxitp,maxppt,mdsv,mint,mksmax,mntmax,mnvar,mpanno,
     +mpascl,mpbars,mpbtit,mpcolr,mpfelm,mpfont,mpfram,mplscl,mplscw,
     +mpmax,mpmin,mpmxvr,mpname,mpparn,mppcrn,mpsclf,mpspli,
     +mpsscl,mpstyl,mpsymb,mpsymf,mptscl,mpttit,mpvaxr,mpxsiz,mpysiz,
     +mqadd,mtbv,mtitl,musrv,mxdep,mxipar,mxlabl,mxqbnk,mxqcnd
 
      real              pflmax
 
      parameter         (mpparn = 11, mppcrn = 170)
      parameter         (mpmxvr = 5,  mxipar = 8, mtitl  = 128)
      parameter         (mxlabl = 40, pflmax = 1.e20)
      parameter         (mtbv = 6, mdsv = 3, musrv = 3)
      parameter         (maxppt = 20000, mnvar = 74, mxdep = 2)
      parameter         (mint = 10, maux = mint + 1, maxitp = 5000)
      parameter         (mxqcnd = 10, mxqbnk = 1000, mqadd = 100000)
      parameter         (mntmax = 20, mksmax = 10)
 
      parameter         (mpfont = 1, mpxsiz = 3, mpysiz = 4)
      parameter         (mplscl = 6, mptscl = 8, mpascl = 5)
      parameter         (mplscw = 2, mpsscl = 7, mpfelm = 9)
      parameter         (mpfram = 2, mpmin  = 1, mpmax  = 2)
      parameter         (mpsclf = 3, mpvaxr = 4, mpname = 5)
      parameter         (mpstyl = 1, mpspli = 2, mpbars = 3)
      parameter         (mpsymf = 4, mpcolr = 5, mpsymb = 6)
      parameter         (mpanno = 7)
      parameter         (mpttit = mpname + mtitl / mcwrd)
      parameter         (mpbtit = mpttit + mtitl / mcwrd)
 
*--- preceding parameters: see LPMAIN description (routine PLPLOT)
      integer idsbis,idsfrm,ihpntr,iqrang,irg1,irg2,irpos,itbv,ivpar,
     +ivpntr,laux,lbias,lbuf,lcnt,lexpv,lform,lframe,lhval,lindx,lm1,
     +lm2,locc,lpint,lpmain,lpparl,lproc,lqv1,lrvc,lrvv,ltab,ltbr,ltmp,
     +lvcurv,lvrw,lvsg,lvval,lvvar,nexpvr,nform,nntv,nocc,ntmax,ntvvar,
     +nvvar
      double precision usrv
      common /plcomm/      lpmain, ltbr, lexpv, ltab, lvsg, lvrw, locc,
     +                     lcnt, lproc, lform, lbias, lpint, lm1, lm2,
     +                     ltmp, lframe, lvvar, lvcurv, lhval, lvval,
     +                     lindx, lpparl, lrvv(4), laux(maux), lqv1,
     +                     lrvc(4*mpmxvr), lbuf
      save   /plcomm/
      common /plcoms/ haxis, vaxis, type, table, sparm, title,
     +                plfnam, plpnam, qcond(mxqcnd)
      save   /plcoms/
      character*(mcnam) haxis, type, table, sparm,
     +                  vaxis(mpmxvr,4)
      character*(mtitl) title
      character*(mcstr) qcond, plfnam, plpnam
      common /plcomp/      nntv(musrv), ntvvar, ihpntr, nocc, nform,
     +                     idsbis(mtbv), idsfrm(mtbv), irg1, irg2, itbv,
     +                     ntmax, nexpvr,
     +                     sortfl, splifl, multfl, fftfl, dumpfl,
     +                     helpfl,
     +                     ivpntr(mpmxvr,4), nvvar(4), ivpar(mxipar),
     +                     irpos(2), iqrang(3,mxqcnd), hrange(2),
     +                     vrange(2,4), qsval
      save   /plcomp/
 
      real                 hrange, vrange, qsval
 
      logical              sortfl, splifl, multfl, fftfl, dumpfl, helpfl
 
      common /plcomd/      usrv(25, musrv)
      save   /plcomd/
      integer icvref,iframe,ipar,ipxval,ipyval,ivnarw,nptval
      common /plotcl/   fpmach
      save   /plotcl/
 
      logical           fpmach
      common /plotcr/   yvtop, fdum, chh,
     +vpt(4), window(4,4), actwin(4,4), range(2), xax(2), yax(8)
      save   /plotcr/
 
      real              yvtop, fdum, chh
      real              vpt, window, actwin, range, xax, yax
 
      common /plotci/   iframe, ivnarw,
     +                  ipar(50), nptval(4), ipxval(4), ipyval(4),
     +                  icvref(4)
      save   /plotci/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer i,ierr,ifdum,ifunam,ind,inmeta,interm,ipict,ipmfst,ipmfsu,
     +ipr,iprrep,isp,itp,ival,ivdum,kstart,l,lm,mmpntg,
     +nf,npict,nplist,nunloc,nvrw,nvsg
      double precision f,rvdum,splist,spstep,tval,zero
      logical intrac
 
      parameter (zero = 0.d0, mmpntg = 5000)
      integer           ityp(mpparn)
      real              rdummy
      dimension         tval(mppcrn)
      dimension         splist(mmpntg)
      character         fname * 8, dummy * 8
      character         * (mcstr) strstr, sval
      character         spvars(mint) * 16, spcomm(mmpntg) * 16
      character         * (mcfil) spfnam, spinam, sout(2)
 
      data ipmfst / 0 /, ipmfsu / 0 /, ifunam / 0 /
 
      if (ipr .ne. 14)  then
        call aawarn('PLMAIN', 1, 'No plot command.')
        goto 999
      endif
*---- User-defined services.
      if (isp .le. 0  .or.  isp .gt. 10) then
        call usercm(ipr, isp)
*---- Predefined services.
      else
        inter = intrac()
        if (ipmfst .eq. 0)  then
*--- book banks for SETPLOT parameters
          if (lroot .eq. 0)  then
            call aafail('PLMAIN', 1, 'LROOT bank not present.')
            goto 999
          endif
          l = lq(lroot - mpparl)
          if (l .eq. 0)
     +    call mzbook (2, l, lroot, -mpparl, 'PLOT', 0, 0, mpparn, 3, 0)
          ipmfst = 1
        endif
        if (isp .eq. 1) then
          if (ipmfsu .eq. 0)  then
            call mzlink (0, '/PLCOMM/', lpmain, ltab, lbuf)
            ipmfsu = 1
            plpnam = 'mad'
          endif
*--- decode command
          call plgcmd(ierr)
          if (ierr .ne. 0) goto 999
*--- get table
          if(table.eq.' ')  then
*--- no user-specified table - take last one
            if (ltable .eq. 0)  then
              call aawarn('PLMAIN', 1, 'No table exists.')
              goto 999
            endif
            call uhtoc (iq(ltable + mtbnam), mcwrd, table, mcnam)
          endif
          call tbopen (table, 1, ltab)
          if (ltab .eq. 0)  then
            call aawarn('PLMAIN', 1, 'Table not found.')
            goto 999
          endif
*--- get table name
          call tbgdsc (ltab, 'TYPE', ifdum, ivdum, rvdum, sval)
*--- get all table biasses, etc.
          call plgtbs (sval, ierr)
          if (ierr .ne. 0) goto 999
*--- paper size from SETPLOT command
          if (lroot .eq. 0) then
            call aafail('PLMAIN', 1, 'LROOT bank not present.')
            goto 999
          endif
          l = lq(lroot - mpparl)
          if (l .eq. 0) then
            call aafail('PLMAIN', 1, 'Plot parameter bank not found.')
            goto 999
          endif
          if (q(l+3) .ne. 0. .and. q(l+4) .ne. 0.) then
            call gxsvar ('XMETAF', 0, q(l+3), ' ')
            call gxsvar ('YMETAF', 0, q(l+4), ' ')
          endif
*--- set PostScript format (.ps or .eps)
          if (iq(l+9) .gt. 0)  call gxsvar('IPSEPS', iq(l+9), 0., ' ')
*--- set plot file name for each plot
          call gxsvar ('SMETNM', 0, 0., plfnam)
          if (ifunam .eq. 0)  then
*--- first time
            plpnam = plfnam
            ifunam = 1
          elseif (plpnam .ne. plfnam)  then
*--- close current .ps file if any
            call gxterm
            plpnam = plfnam
            call gxinit
          endif
          if (iplflg .eq. 0 .and. .not. dumpfl) then
*--- initalize plotting (workstations etc.)
            iplflg = 1
*--- metafile or postscript = unit 16, name='metafile' or 'mad.ps' etc.
            call flnset ('metafile', fname)
            call gxsvar ('IMETUN', 16, 0., ' ')
*--- error output on echo file - already open
            call gxeopn ('ERRORFILE', iqlog)
*--- if interactive, inquire user via IQTTIN (input) and IQTYPE
            if (inter) then
              call gxsvar ('INUNIT', iqttin, 0., ' ')
              call gxsvar ('IOUNIT', iqtype, 0., ' ')
*--- set wait time to 1 sec. (Higz only)
              call gxsvar ('WTTIME', 0, 1., ' ')
              call gxasku
            endif
*--- inhibit second MZEBRA call (Higz only)
            call gxsvar ('ICZEBR', 0, 0., ' ')
*--- set or reduce window size (only X11)
            if (iq(l+10) .gt. 0) then
              call gxsvar('NXPIX', iq(l+10), 0., ' ')
            endif
            if (iq(l+11) .gt. 0) then
              call gxsvar('NYPIX', iq(l+11), 0., ' ')
            else
              call gxsvar('NYPIX', 670, 0., ' ')
            endif
*--- inhibit initial X-Window (only X11)
            call gxsvar('ITSEOP', 1, 0., ' ')
            call gxinit
            call gxclos
          endif
*--- plot only if a workstation is open
          if (.not. dumpfl)  then
            call gxqvar ('INTERM', interm, rdummy, fname)
            call gxqvar ('INMETA', inmeta, rdummy, fname)
            if (interm .eq. 0 .and. inmeta .eq. 0) then
              call aawarn('PLMAIN', 1,
     +        'Neither terminal nor metafile open.')
              goto 999
            endif
*--- open workstation, metafile etc.
            call gxopen
          endif
*--- loop over parameter values, one plot for each, limit
          nvrw = iq(ltbr+1)
          nvsg = iq(ltbr+2)
          itp  = iq(ltbr+3)
          if (itp .eq. 0)  then
            npict = 1
          elseif (itp .eq. 1)  then
            npict = nvrw
          else
            npict = nvsg
          endif
*--- limit no. of frames to user request or default
          npict = min (npict, ivpar(4))
          do 10 ipict = 1, npict
*--- book top plot bank
            call mzbook (1, lm, lpmain, 1, 'MAIN', mpfram+3, mpfram+3,
     +      mpfelm, 0, -1)
*--- Proceed to fill plot banks.
            call plprep (ipict, itp, ierr)
            if (ierr .ne. 0) goto 20
            if (dumpfl)  then
*--- dump the plot bank to output
              call pldump
            else
*--- plot
            if (ipict .gt. 1)  call gxopen
              call plplot
              call gxwait
*WNT          CALL GXCLRW
              call gxclos
            endif
            call mzdrop (0, lpmain, ' ')
   10     continue
   20     continue
          call mzwipe (1)
        elseif (isp .eq. 2)  then
*--- SETPLOT command - store parameters
          if (lroot .eq. 0)  then
            call aafail('PLMAIN', 1, 'LROOT bank not present.')
            goto 999
          endif
          l = lq(lroot - mpparl)
          if (l .eq. 0)  then
            call aafail('PLMAIN', 1, 'Plot parameter bank not found.')
            goto 999
          endif
          call utgtyp (lccmd, ityp)
          call utgint (lccmd, 1, 1, ival)
          if (ityp(1) .ne. 0)  iq(l + 1) = ival
          call utgflt (lccmd, 2, 8, tval)
          do 30 i = 2, 8
            if (ityp(i) .ne. 0) q(l + i) = tval(i - 1)
   30     continue
          call utgint (lccmd, 9, 10, ival)
          if (ityp(9) .ne. 0)  iq(l + 9) = max(0, min(2, ival))
          call utgint (lccmd, 10, 11, iq(l+10))
        elseif (isp .eq. 3)  then
*--- RESPLOT command - restore defaults for plotting
          if (lroot .eq. 0)  then
            call aafail('PLMAIN', 1, 'LROOT bank not present.')
            goto 999
          endif
          l = lq(lroot - mpparl)
          if (l .eq. 0)  then
            call aafail('PLMAIN', 1, 'Plot parameter bank not found.')
            goto 999
          endif
        if (inter)  then
*--- font
          iq(l+1) = 1
*--- linewidth
          q(l+2) = 1.
*--- xsize
          q(l+3) = 0.
*--- ysize
          q(l+4) = 0.
*--- ascale, lscale, sscale, rscale
          q(l+5) = 1.
          q(l+6) = 1.
          q(l+7) = 1.
          q(l+8) = 1.
*--- pixels
          iq(l+10) = 1000
          iq(l+11) = 670
        else
*--- font
          iq(l+1) = 1
*--- linewidth
          q(l+2) = 5.
*--- xsize
          q(l+3) = 0.
*--- ysize
          q(l+4) = 0.
*--- ascale, lscale, sscale, rscale
          q(l+5) = 1.5
          q(l+6) = 2.
          q(l+7) = 2.
          q(l+8) = 1.8
*--- pixels
          iq(l+10) = 1000
          iq(l+11) = 670
        endif
        elseif (isp .eq. 4)  then
*--- SPRINT command - store parameters and execute
          if (lroot .eq. 0)  then
            call aafail('PLMAIN', 1, 'LROOT bank not present.')
            goto 999
          endif
          l = lq(lroot - mpparl)
          if (l .eq. 0)  then
            call aafail('PLMAIN', 1, 'Plot parameter bank not found.')
            goto 999
          endif
          spfnam = 'sprint'
          call utgstr(lccmd, 1, 1, spfnam)
          spinam = ' '
          call utgstr(lccmd, 2, 2, spinam)
          kstart = 2
          do 41 i = 1, mint-1
   41     spvars(i) = ' '
          call utgnam(lccmd, kstart+1, kstart+mint-1, spvars)
          iprrep = 0
          spstep = zero
          if (spinam .eq. ' ')  then
            call utgint(lccmd, kstart+mint, kstart+mint, iprrep)
            call utgflt(lccmd, kstart+mint+1, kstart+mint+1, spstep)
            nplist = 50
            do 42  i = 1, nplist
              spcomm(i) = ' '
   42       splist(i) = -1.d0
            call utgflt(lccmd, kstart+mint+2, kstart+mint+51, splist)
            if (iprrep .lt. 0 .or. spstep .eq. zero)  iprrep = 0
          else
            call flopen(spinam, 'SRFD', 0, 0, nunloc, error)
            if (error)  then
              call aawarn('sprint', 1, 'unable to open file :'//spinam)
              goto 999
            endif
            nplist = 0
   44       read (nunloc, '(a)', end = 45) strstr
            if (strstr(1:1) .ne. '#')  then
              call plgitn(2, strstr, nf, sout)
              if (nf .gt. 0)  then
                call plgfia(sout(1), ind, f, i, dummy)
                nplist = nplist + 1
                if (ind .eq. 1)  then
                  splist(nplist) = i
                elseif (ind .eq. 2 .or. ind .eq. 3)  then
                  splist(nplist) = f
                else
                  dummy = ' '
                  write(dummy, '(i8)')  nplist
                  call aawarn('sprint', 1,
     +            'illegal format, line:'//dummy)
                  nplist = nplist - 1
                  goto 45
                endif
                if (nf .eq. 2)  then
                  spcomm(nplist) = sout(2)
                else
                  spcomm(nplist) = ' '
                endif
              endif
            endif
            if (nplist .lt. mmpntg)  goto 44
   45       call flclos(nunloc, error)
          endif
*--- no user-specified table - take last one
          if (ltable .eq. 0)  then
            call aawarn('sprint', 1, 'no table exists.')
            goto 999
          endif
          call uhtoc (iq(ltable + mtbnam), mcwrd, table, mcnam)
          call tbopen (table, 1, ltab)
          if (ltab .eq. 0)  then
            call aawarn('sprint', 1, 'table not found.')
            goto 999
          endif
*--- get table name
          call tbgdsc (ltab, 'TYPE', ifdum, ivdum, rvdum, sval)
          if (sval(:5) .ne. 'TWISS')  then
            call aawarn('sprint', 1, 'last table not TWISS.')
            goto 999
          endif
          call flopen(spfnam, 'SWFD', 0, 0, nunloc, error)
          call plprpr(spvars, nunloc, iprrep, spstep, nplist,
     +    splist, spcomm)
          call flclos(nunloc, error)
          call mzwipe (1)
        endif
      endif
  999 end
