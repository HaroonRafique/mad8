      subroutine mtend
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Final matching print-out, ENDMATCH command.                        *
* Attribute:                                                           *
*   INCREMENT    (string)  Save increments made in matching.           *
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
      integer mmbias,mmbnam,mmcode,mmdata,mmf1,mmf2,mmf3,mmold,mmsiz,
     +mmvnam
 
*---- Bias for variable parameters group.
      parameter         (mmf1   = 1, mmcode = 2, mmbias = 3,
     +                   mmf2   = 4, mmold = 5, mmdata = mmold+mwflt,
     +                   mmf3   = mmdata+4*mwflt, mmbnam = mmf3+1,
     +                   mmvnam = mmbnam+mwnam, mmsiz = mmf3+2*mwnam)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer lflbuf,lfltab
 
*---- Links for closed orbit correction module.
      common /fllink/   lfltab, lflbuf
      save              /fllink/
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer iflag,iref,iunit,jrow,ltemp,mcol,nrow
      double precision dval,vnew,vold,zero
 
      parameter         (mcol = 3)
      character*(mcnam) tabnam, cnam(mcol)
      character*(mcfil) filnam
      integer           icfrm(mcol)
      real              dt
 
      data cnam         / 'NAME', 'ATTRIBUTE', 'VALUE' /
 
*---- Get attribute.
      tabnam = ' '
      call utgnam(lccmd, 1, 1, tabnam)
 
*---- Assign working space (no need to keep previous working space).
      iwork = ncon
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Compute and print matching conditions.
      call mtcond(.true., ncon, dq(1), iflag)
 
*---- Drop links from sequence to constraint banks.
      if (lsmat .ne. 0) call mzdrop(0, lsmat, ' ')
 
*---- Drop initial values bank; may contain expressions.
      if (lmtbet .ne. 0) call aadrop(lmtbet)
 
*---- Drop constraints; may contain expanded lines and expressions.
  110 if (lmcon .ne. 0) then
 
*---- Drop line expansion for "LINE" constraint.
        if (lq(lmcon-1) .ne. 0) call lndrop(lq(lmcon-1))
 
*---- Drop expression chained to this constraint.
        lcexp = lq(lmcon-2)
  120   if (lcexp .ne. 0) then
          ltemp = lq(lcexp)
          call aadrop(lcexp)
          lcexp = ltemp
          go to 120
        endif
 
*---- Drop constraint.
        ltemp = lq(lmcon)
        call aadrop(lmcon)
        lmcon = ltemp
        go to 110
      endif
 
*---- Write increment file.
      if (tabnam .ne. ' ') then
 
*---- Build internal table.
        nrow  = 0
        lvar = lmvar
  130   if (lvar .ne. 0) then
          nrow  = nrow + 1
          lvar = lq(lvar)
          go to 130
        endif
 
        icfrm(1) = 5
        icfrm(2) = 5
        icfrm(3) = 3
        if (double) icfrm(3) = mreal
        call tbcrea(tabnam, 1, nrow, mcol, cnam, icfrm, 1, lfltab)
        call tbpdsc(lfltab, 'TYPE', 5, 0, zero, 'INCREMENTS')
        lvar = lmvar
 
        do 140 jrow = 1, nrow
          call tbset(lfltab, jrow, 3, lflbuf)
          call ucopy(q(lvar+mmbnam), q(lflbuf+1), mwnam)
          call ucopy(q(lvar+mmvnam), q(lflbuf+mwnam+1), mwnam)
          lref = lq(lvar-1)
          iref = iq(lvar+mmbias)
          call utgflt(lref, iref, iref, vnew)
          call ucopy(q(lvar+mmold), vold, mwflt)
 
          if (double) then
            dval = vnew - vold
            call ucopy(dval, q(lflbuf+2*mwnam+1), mwflt)
          else
            q(lflbuf+2*mwnam+1) = vnew - vold
          endif
 
          lvar = lq(lvar)
  140   continue
 
*---- Write table in TFS format.
        call tbclos(lfltab)
        filnam = tabnam
        call flopen(filnam, 'SWFD', 0, 0, iunit, error)
        if (.not. error) then
          call tbwtfs(tabnam, iunit)
          call flname(iunit, filnam)
          call flclos(iunit, error)
          if (.not. error) then
            msg(1) = 'Increments written on file: ' // filnam
            call aainfo('MTEND', 1, msg)
          endif
        endif
        call tbopen(tabnam, 0, lfltab)
        call tbdrop(lfltab)
      endif
 
*---- Drop variables.
      if (lmvar .ne. 0) call mzdrop(0, lmvar, 'L')
 
*---- Drop initial value line.
      if (lmtlin .ne. 0) call lndrop(lmtlin)
 
*---- Release working space.
      iwork = 0
      nwork = 0
      call mzwork(0, dq(1), dq(1), - 1)
      if (trace) then
        call timex(time2)
        dt = time2 - time1
        write (msg, 920) time2, dt
        call aainfo('MTEND', 1, msg)
      endif
 
  910 format('Last value of the penalty function:',1p,e14.6)
  920 format('End matching mode, time = ',f12.3,
     +       ', time for matching = ',f12.3,' seconds')
 
 9999 end
