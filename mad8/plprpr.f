      subroutine plprpr (spvars, nunloc, iprrep, spstep, nplist, splist,
     +spcomm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:
*   Print Twiss paramters at selected s values
*
*--- Input:
*    spvars  Twiss parameters to be printed
*    nunloc  output unit number
*    iprrep  repetition of step
*    spstep  step
*    nplist  no. of values in splist
*    splist  list of s values
*--- Warning: the output unit is unit 16, risk of mix with plot
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
      integer meangb,meangg,meangr,mechg,mee1b,mee1g,mee2b,mee2g,meflde,
     +mefrqc,megapb,megapg,meh1b,meh1g,meh2b,meh2g,mehrmc,meintb,meintg,
     +mek1b,mek1g,mek1q,mek2b,mek2s,mek3b,mek3o,mekick,meklm,meksg,
     +mekss,melagc,melen,mesigx,mesigy,metltb,metlte,metltg,metltm,
     +metlto,metltq,metlts,metyp,mevltc,mexcol,mexma,meycol,meyma
      integer meintbx,meintgx,meapro,mek0lm,met0m,mek1lm,met1m,
     +mek2lm,met2m,mek3lm,met3m,meaprm,meapss,melosc,meaprc,mee0l,
     +medel,mephil,mefrql,melosl,mevoll,melagl,meaprl
 
*---- Bias for element attribute values.
*     These statements MUST be consistent with the command dictionary.
*     Routines using this group must also include BANKHEAD and CMDGROUP.
*     Common to all elements: TYPE and L attributes.
      parameter    (metyp  = mbat   + mcval, melen  = metyp  + mcsiz)
*     Common to RBEND and SBEND.
      parameter    (meangb = melen  + mcsiz, mek1b  = meangb + mcsiz,
     +              mee1b  = mek1b  + mcsiz, mee2b  = mee1b  + mcsiz,
     +              metltb = mee2b  + mcsiz, mek2b  = metltb + mcsiz,
     +              meh1b  = mek2b  + mcsiz, meh2b  = meh1b  + mcsiz,
     +              megapb = meh2b  + mcsiz, meintb = megapb + mcsiz)
      parameter (meintbx = meintb + mcsiz, mek3b  = meintbx + mcsiz)
*     QUADRUPO.
      parameter    (mek1q  = melen  + mcsiz, metltq = mek1q  + mcsiz)
      integer meaprq
      parameter    (meaprq = metltq + mcsiz)
*     SEXTUPOL.
      parameter    (mek2s  = melen  + mcsiz, metlts = mek2s  + mcsiz)
      integer meaprs
      parameter    (meaprs = metlts + mcsiz)
*     OCTUPOLE.
      parameter    (mek3o  = melen  + mcsiz, metlto = mek3o  + mcsiz)
      parameter    (meapro = metlto + mcsiz)
*     MULTIPOL.
      parameter    (mek0lm = melen  + mcsiz, met0m  = mek0lm + mcsiz,
     +              mek1lm = met0m  + mcsiz, met1m  = mek1lm + mcsiz,
     +              mek2lm = met1m  + mcsiz, met2m  = mek2lm + mcsiz,
     +              mek3lm = met2m  + mcsiz, met3m  = mek3lm + mcsiz,
     +              meaprm = melen  + 21*mcsiz)
*     MULTIPOL.
      parameter    (meklm  = melen  + mcsiz, metltm = meklm  + mcsiz)
*     SOLENOID.
      parameter    (mekss  = melen  + mcsiz, meapss = mekss  + mcsiz)
*     RFCAVITY.
      parameter    (mevltc = melen  + mcsiz, melagc = mevltc + mcsiz,
     +              mefrqc = melagc + mcsiz, mehrmc = mefrqc + mcsiz)
      parameter    (melosc = mehrmc + 5*mcsiz,
     +              meaprc = melosc + 3*mcsiz)
*     ELSEPARA.
      parameter    (meflde = melen  + mcsiz, metlte = meflde + mcsiz)
*     Common to SROT and YROT.
      parameter    (meangr = melen  + mcsiz)
*     Common to KICK, HKICK, and VKICK.
      parameter    (mekick = melen  + mcsiz)
*     Common to ECOLLIMA and RCOLLIMA.
      parameter    (mexcol = melen  + mcsiz, meycol = mexcol + mcsiz)
*     BEAMBEAM.
      parameter    (mesigx = melen  + mcsiz, mesigy = mesigx + mcsiz,
     +              mexma  = mesigy + mcsiz, meyma  = mexma  + mcsiz,
     +              mechg  = meyma  + mcsiz)
*     GBEND.
      parameter    (meangg = melen  + mcsiz, mek1g  = meangg + mcsiz,
     +              mee1g  = mek1g  + mcsiz, mee2g  = mee1g  + mcsiz,
     +              metltg = mee2g  + mcsiz, meksg  = metltg + mcsiz,
     +              meh1g  = meksg  + mcsiz, meh2g  = meh1g  + mcsiz,
     +              megapg = meh2g  + mcsiz, meintg = megapg + mcsiz)
*     lcavity.
      parameter    (mee0l  = melen  + mcsiz, medel  = mee0l  + mcsiz,
     +              mephil = medel  + mcsiz, mefrql = mephil + mcsiz,
     +              melosl = mefrql + mcsiz, mevoll = melosl + mcsiz,
     +              melagl = mevoll + mcsiz, meaprl = melagl + mcsiz)
      parameter (meintgx = meintg + mcsiz)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
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
      double precision pi
      parameter         (pi = 3.141592653589793d0)
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer i,icurr,ienum,iflag,iocc,ip,iprrep,irp,is,isp,itp,j,jbyt,
     +jrow,jseg,k,lastnb,ll,maxsv,nplist,nunloc,nvrw
      double precision am,ax0,ax1,ay0,ay1,bx0,bx1,by0,by1,diff,dmu,dpx0,
     +dpx1,dpy0,dpy1,dx0,dx1,dy0,dy1,gamx,gamy,one,scurr,sl,slv,splist,
     +spstep,ss,temp,tw0,tw1,two,twopi,ux0,ux1,uy0,uy1,zero
      parameter         (zero = 0.d0)
      parameter         (one = 1.d0, two = 2.d0, twopi = two * pi)
      parameter         (maxsv = 15000)
 
      dimension         splist(*)
      character * (*)   spcomm(*)
      dimension         slv(maxsv)
      dimension         tw0(mint), tw1(mint), temp(9)
      dimension         am(6,6)
      equivalence (bx0, tw0(1)), (ax0, tw0(2)), (ux0, tw0(3)), (dx0, tw0
     +(4)), (dpx0, tw0(5)), (by0, tw0(6)), (ay0, tw0(7)), (uy0, tw0(8)),
     +(dy0, tw0(9)), (dpy0, tw0(10))
      equivalence (bx1, tw1(1)), (ax1, tw1(2)), (ux1, tw1(3)), (dx1, tw1
     +(4)), (dpx1, tw1(5)), (by1, tw1(6)), (ay1, tw1(7)), (uy1, tw1(8)),
     +(dy1, tw1(9)), (dpy1, tw1(10))
 
      logical           eflag, pnflag
 
      integer           ibs(mint), ifm(mint), isl(mint), iref(mint)
      double precision edg1, edg2, sk1, an, h, corr, hgap, fint,
     +ek0(6), enfr(6,6), tw(6,6,6)
      double precision an2
 
      character         spvars(*) * 16
      character*(mcnam) spv(mint)
      character*(mcnam) eln
      character*(mcstr) strout
 
      data spv / 'BETX', 'ALFX', 'MUX', 'DX', 'DPX', 'BETY', 'ALFY',
     +'MUY', 'DY', 'DPY' /
      data iref / 1, 6, 2, 7, 3, 8, 4, 9, 5, 10 /
 
*--- book work bank
      call mzbook (1, ll, ltbr, 1, 'WORK', 9, 9, 3, 0, 0)
*--- book bank for row numbers
      call mzbook (1, lvrw, ltbr, -1, 'VRW ', 0, 0, maxsv, 0, -1)
      nvrw = 1
      ss = zero
      slv(1) = zero
      iq(lvrw+1) = 1
      do 10 i = 1, mint
   10 isl(i) = 0
      isl(1) = 1
      isl(6) = 1
      do 30 jseg = 1, iq(ltab+mtbseg)
        call tbseg (ltab, jseg, eflag)
        if (eflag)  then
          call aawarn('sprint', 1, 'internal table management error')
          goto 999
        endif
        do 20 jrow = 1, iq(ltab+mtbrow)
          call tbset(ltab, jrow, 1, lbuf)
          if (lbuf .ne. 0) then
            call utelem (lcseq, jrow, iflag, eln, iocc, ienum)
*--- keep only element end points
            if (jbyt(iflag, 1, mcode) .eq. 1) then
              call bmgelm(lcelm, temp)
*--- keep only first s, i.e. drop zero length elements
              if (temp(1) .gt. zero)  then
*--- store row number and position
                nvrw = nvrw + 1
                iq(lvrw+nvrw) = jrow
                ss = ss + temp(1)
                slv(nvrw) = ss
              endif
            endif
          endif
   20   continue
   30 continue
*--- get formats and biasses
      do 40 i = 1, mint
        call tbcol (ltab, spv(i), ifm(i), ibs(i))
   40 continue
*--- find selected variables
      pnflag = .false.
      do 60 j = 1, 9
        pnflag = pnflag .or. spvars(j) .eq. 'NAME'
        do 50 i = 1, mint
          if (spvars(j) .eq. spv(i))  then
            isl(i) = 1
            goto 60
          endif
   50   continue
   60 continue
*--- print header
      strout = '#       s       '
      k = 16
      do 70 i = 1, mint
        if (isl(iref(i)) .ne. 0)  then
          strout(k+1:k+14) = '      ' // spv(iref(i))
          k = k + 14
        endif
   70 continue
      if (pnflag)  then
        strout(k+5:k+10) = 'name'
      endif
      write(nunloc, '(a)') strout(:lastnb(strout))
      scurr = zero
      icurr = 2
      do 160 is = 1, nplist
        if (splist(is) .ge. zero .and. splist(is) .le. slv(nvrw))
     +  then
          do 150 irp = 0, iprrep
            sl = splist(is) + irp * spstep
            if (sl .ge. zero .and. sl .le. slv(nvrw))  then
              if (sl .lt. scurr) icurr = 2
              do 100 ip = icurr, nvrw
                if (sl .le. slv(ip)) then
*--- get Twiss parameters at start of element
                  call tbset(ltab, iq(lvrw+ip-1), 1, lbuf)
                  do 80 i = 1, mint
                    if (ifm(i) .eq. 3) then
                      tw0(i) = q(lbuf + ibs(i) + 1)
                    else
                      call ucopy(q(lbuf+ibs(i)+1), tw0(i), mwflt)
                    endif
   80             continue
                  icurr = ip
                  scurr = sl
                  diff = sl - slv(ip-1)
                  call tbset(ltab, iq(lvrw+ip), 1, lbuf)
                  do 90 i = 1, mint
                    if (ifm(i) .eq. 3) then
                      tw1(i) = q(lbuf + ibs(i) + 1)
                    else
                      call ucopy(q(lbuf+ibs(i)+1), tw1(i), mwflt)
                    endif
   90             continue
*--- get element parameters and Twiss parameters
                  call utelem (lcseq, iq(lvrw+ip), iflag, eln, iocc,
     +            ienum)
                  isp = iq(lcelm + mbsp)
                  call bmgelm(lcelm, temp)
                  if (isp .eq. 1) then
*--- drift
                    itp = 0
                  elseif (isp .eq. 2 .or. isp .eq. 3) then
*--- bend
                    itp = 1
                    call ucopy(q(lcelm+mee1g), edg1, mwflt)
                    call ucopy(q(lcelm+mee2g), edg2, mwflt)
                    call ucopy(q(lcelm+mek1g), sk1, mwflt)
                    call ucopy(q(lcelm+meangg), an,   mwflt)
                    call ucopy(q(lcelm+megapg), hgap, mwflt)
                    call ucopy(q(lcelm+meintg), fint, mwflt)
                    if (isp .eq. 2) then
*--- HG001026: arc length to rectangular bend
                      an2 = an / 2.d0
                      if (an2 .ne. 0.d0 .and. rbarc)
     +                temp(1) = temp(1) * an2 / sin(an2)
                      edg1 = edg1 + an2
                      edg2 = edg2 + an2
                    endif
                    h = an / temp(1)
                    corr = (h + h) * hgap * fint
                    call tmfrng(.false.,h,sk1,edg1,zero,+one,corr,
     +              ek0,enfr,tw)
                  elseif (isp .eq. 5) then
*--- quad
                    itp = 2
                  else
                    itp = 0
                  endif
                  if (bx0 .ne. zero) then
                    gamx = (one + ax0**2) / bx0
                  else
                    gamx = zero
                  endif
                  if (by0 .ne. zero) then
                    gamy = (one + ay0**2) / by0
                  else
                    gamy = zero
                  endif
*--- element matrix 6x6
                  call plelma(itp, temp, diff, am)
                  if (itp .eq. 1)  call m66mpy(am, enfr, am)
*--- interpolate Twiss parameters
*    beta_x, beta_y
                  bx1 = -two * am(1,1) * am(1,2) * ax0 + am(1,1) **2 *
     +            bx0 + am(1,2)**2 * gamx
                  by1 = -two * am(3,3) * am(3,4) * ay0 + am(3,3) **2 *
     +            by0 + am(3,4)**2 * gamy
*--- alfa_x, alfa_y
                  ax1 = (am(1,1) * am(2,2) + am(1,2) * am(2,1)) * ax0 -
     +            am (1,1) * am(2,1) * bx0 - am(1,2) * am(2,2) * gamx
                  ay1 = (am(3,3) * am(4,4) + am(3,4) * am(4,3)) * ay0 -
     +            am (3,3) * am(4,3) * by0 - am(3,4) * am(4,4) * gamy
*--- mu_x, mu_y
                  dmu = atan2(am(1,2), bx0 * am(1,1) - ax0 * am(1,2))
                  if (dmu .lt. zero) dmu = dmu + twopi
                  ux1 = ux0 + dmu / twopi
                  dmu = atan2(am(3,4), by0 * am(3,3) - ay0 * am(3,4))
                  if (dmu .lt. zero) dmu = dmu + twopi
                  uy1 = uy0 + dmu / twopi
*--- D-x, D-y
                  dx1 = am(1,1) * dx0 + am(1,2) * dpx0 + am(1,6)
                  dy1 = am(3,3) * dy0 + am(3,4) * dpy0 + am(3,6)
*--- D'-x, D'-y
                  dpx1 = am(2,1) * dx0 + am(2,2) * dpx0 + am(2,6)
                  dpy1 = am(4,3) * dy0 + am(4,4) * dpy0 + am(4,6)
                  goto 110
                endif
  100         continue
  110         continue
              strout = ' '
              write(strout, '(1p, e16.8)') sl
              k = 16
              do 120 i = 1, mint
                if (isl(iref(i)) .ne. 0) then
                  write(strout(k+1:k+14), '(1p, e14.6)') tw1(iref(i))
                  k = k + 14
                endif
  120         continue
              if (pnflag) then
                strout(k+5:) = eln
                k = k + mcnam + 4
              endif
              strout(k+1:) = spcomm(is)
              write(nunloc, '(a)') strout(:lastnb(strout))
            endif
  150     continue
        endif
  160 continue
  999 end
