      subroutine plintp(iep, npnt, nmax, step, vmin, vmax, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Interpolate variables plotted against s                            *
* Input:                                                               *
*   IEP         (int)   row number of first element                    *
*   NPNT        (int)   number of points  (updated !)                  *
*   NMAX        (int)   max. no. of points that should possibly appear *
*   STEP        (real)  max. dist. between two successive hor. values  *
*   VMIN        (real)  min. for 4 axes (updated!)                     *
*   VMAX        (real)  max. for 4 axes (updated!)                     *
* Output:                                                              *
*   IERR        (int)   0 if OK, else > 0                              *
*                                                                      *
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
      integer i,iecub,ienum,iep,ierr,iflag,iocc,ip,ipc,isp,itp,iv,j,jv,
     +k,ki,new,nmax,npnt,nrg,nst
      double precision am,ax0,ax1,ay0,ay1,bx0,bx1,by0,by1,dmu,dpx0,dpx1,
     +dpy0,dpy1,dstp,dx0,dx1,dy0,dy1,gamx,gamy,one,stpl,temp,tw0,tw1,
     +two,twopi,ux0,ux1,uy0,uy1,zero
      parameter         (zero = 0.d0)
      parameter         (one = 1.d0, two = 2.d0, twopi = two * pi)
 
      dimension         tw0(mint), tw1(mint), temp(9)
      dimension         am(6,6)
      equivalence       (bx0, tw0(1)), (ax0, tw0(2)), (ux0, tw0(3)),
     +                  (dx0, tw0(4)), (dpx0, tw0(5)),
     +                  (by0, tw0(6)), (ay0, tw0(7)), (uy0, tw0(8)),
     +                  (dy0, tw0(9)), (dpy0, tw0(10))
      equivalence       (bx1, tw1(1)), (ax1, tw1(2)), (ux1, tw1(3)),
     +                  (dx1, tw1(4)), (dpx1, tw1(5)),
     +                  (by1, tw1(6)), (ay1, tw1(7)), (uy1, tw1(8)),
     +                  (dy1, tw1(9)), (dpy1, tw1(10))
 
      real              step, vmin(4), vmax(4), s0, diff, gxcubv
 
      integer           ifm(mint), ibs(mint)
 
      logical           elmflg, vanflg, sqflag
      double precision edg1, edg2, sk1, an, h, corr, hgap, fint,
     +ek0(6), enfr(6,6), tw(6,6,6)
      double precision an2
 
      character*(mcnam) dum1
      character*(mcnam) spv(mint)
 
      data spv / 'BETX', 'ALFX', 'MUX', 'DX', 'DPX',
     +           'BETY', 'ALFY', 'MUY', 'DY', 'DPY' /
 
      ierr = 0
 
*--- book auxiliary banks
      do 10 i = 1, maux
        call mzbook (1, laux(i), lpint, -i, 'AUX ', 0, 0, nmax, 0, -1)
   10 continue
*---  set flags for correct, and spline interpolation
      elmflg = .false.
      vanflg = .false.
      k = 0
      do 20 iv = 1, ntvvar
        do 20 jv = 1, nvvar(iv)
          k = k + 1
          elmflg = elmflg .or. iq(lpint+k) .gt. 0
          vanflg = vanflg .or. iq(lpint+k) .eq. 0
   20 continue
      if (elmflg)  then
*--- get formats and biasses
        do 30 i = 1, mint
          call tbcol (ltab, spv(i), ifm(i), ibs(i))
   30   continue
*--- get initial Twiss parameters
        call tbset(ltab, iq(lvrw+1), 1, lbuf)
        do 40 i = 1, mint
          if (ifm(i) .eq. 3) then
            tw0(i) = q(lbuf + ibs(i) + 1)
          else
            call ucopy(q(lbuf+ibs(i)+1), tw0(i), mwflt)
          endif
   40   continue
*--- get intermediate s values, and interpolate Twiss parameters
        new = 1
*--- collect s values for final plot in bank LAUX(MAUX)
        q(laux(maux)+1) = q(lhval+1)
*--- set first point of Twiss parameters
        do 50 i = 1, mint
   50   q(laux(i)+1) = tw0(i)
        do 90 ip = 2, npnt
          s0    = q(laux(maux)+new)
          diff  = (q(lhval+ip) - s0)
          if (diff .gt. zero) then
*--- keep point - calculate number of steps for element interpol.
            call tbset(ltab, iq(lvrw+ip), 1, lbuf)
            do 60 i = 1, mint
              if (ifm(i) .eq. 3) then
                tw1(i) = q(lbuf + ibs(i) + 1)
              else
                call ucopy(q(lbuf+ibs(i)+1), tw1(i), mwflt)
              endif
   60       continue
            nst  = diff / step
            if (nst .gt. 0)  then
*--- get element parameters and Twiss parameters
              call utelem (lcseq, iq(lvrw+ip), iflag, dum1, iocc, ienum)
              isp = iq(lcelm + mbsp)
              call bmgelm(lcelm, temp)
              if (isp .eq. 1)  then
*--- drift
                itp = 0
              elseif (isp .eq. 2 .or. isp .eq. 3)  then
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
     +            temp(1) = temp(1) * an2 / sin(an2)
                  edg1 = edg1 + an2
                  edg2 = edg2 + an2
                endif
                h = an / temp(1)
                corr = (h + h) * hgap * fint
                call tmfrng(.false.,h,sk1,edg1,zero,+one,corr,
     +          ek0,enfr,tw)
                if (temp(5) .ne. zero)  then
                  h = enfr(2,1)
                  enfr(2,1) = enfr(4,3)
                  enfr(4,3) = h
                endif
              elseif (isp .eq. 5)  then
*--- quad
                itp = 2
              else
                itp = 0
              endif
              stpl = diff / (nst + 1)
              if (bx0 .ne. zero)  then
                gamx = (one + ax0**2) / bx0
              else
                gamx = zero
              endif
              if (by0 .ne. zero)  then
                gamy = (one + ay0**2) / by0
              else
                gamy = zero
              endif
              do 70 j = 1, nst
                new = new + 1
                if (new .gt. nmax)  then
                  ierr = 1
                  goto 999
                endif
                dstp = j * stpl
                q(laux(maux)+new) = s0 + dstp
*--- element matrix 6x6
                call plelma(itp, temp, dstp, am)
                if (itp .eq. 1)  call m66mpy(am, enfr, am)
*--- interpolate Twiss parameters
*    beta_x, beta_y
                q(laux(1)+new) = -two * am(1,1) * am(1,2) * ax0
     +          + am(1,1)**2 * bx0 + am(1,2)**2 * gamx
                q(laux(6)+new) = -two * am(3,3) * am(3,4) * ay0
     +          + am(3,3)**2 * by0 + am(3,4)**2 * gamy
*--- alfa_x, alfa_y
                q(laux(2)+new) = (am(1,1) * am(2,2) + am(1,2) * am(2,1))
     +          * ax0 - am(1,1) * am(2,1) * bx0
     +          - am(1,2) * am(2,2) * gamx
                q(laux(7)+new) = (am(3,3) * am(4,4) + am(3,4) * am(4,3))
     +          * ay0 - am(3,3) * am(4,3) * by0
     +          - am(3,4) * am(4,4) * gamy
*--- mu_x, mu_y
                dmu = atan2(am(1,2), bx0 * am(1,1) - ax0 * am(1,2))
                if (dmu .lt. zero)  dmu = dmu + twopi
                q(laux(3)+new) = ux0 + dmu / twopi
                dmu = atan2(am(3,4), by0 * am(3,3) - ay0 * am(3,4))
                if (dmu .lt. zero)  dmu = dmu + twopi
                q(laux(8)+new) = uy0 + dmu / twopi
*--- D-x, D-y
                q(laux(4)+new) = am(1,1) * dx0 + am(1,2) * dpx0
     +          + am(1,6)
                q(laux(9)+new) = am(3,3) * dy0 + am(3,4) * dpy0
     +          + am(3,6)
*--- D'-x, D'-y
                q(laux(5)+new)  = am(2,1) * dx0 + am(2,2) * dpx0
     +          + am(2,6)
                q(laux(10)+new) = am(4,3) * dy0 + am(4,4) * dpy0
     +          + am(4,6)
   70         continue
            endif
            new = new + 1
            if (new .gt. nmax)  then
              ierr = 1
              goto 999
            endif
            q(laux(maux)+new) = q(lhval+ip)
            do 80 i = 1, mint
              tw0(i) = tw1(i)
   80       q(laux(i)+new) = tw1(i)
          endif
   90   continue
*--- loop over variables, replace those with codes
        i = 0
        do 110 iv = 1, ntvvar
          do 110 jv = 1, nvvar(iv)
            i = i + 1
            ipc = iq(lpint + i)
            sqflag = ipc .gt. 100
            ipc = mod(ipc, 100)
            if (ipc .gt. 0)  then
              k  = laux(ipc)
              ki = lrvc(i)
              do 100 j = 1, new
                if (sqflag)  then
                  q(ki+j) = sqrt(q(k+j))
                else
                  q(ki+j) = q(k+j)
                endif
*--- adapt min. and max.
                vmin(iv) = min(vmin(iv), q(ki+j))
                vmax(iv) = max(vmax(iv), q(ki+j))
  100         continue
            endif
  110   continue
      endif
      if (vanflg)  then
*--- get intermediate s values, and original without multiple s
        new = 1
        nrg = 1
        iq(laux(1)+1) = 1
        q(laux(2)+1)  = q(lhval+1)
*--- collect s values for final plot in bank LAUX(MAUX)
        q(laux(maux)+1) = q(lhval+1)
        do 130 ip = 2, npnt
          s0    = q(laux(maux)+new)
          diff  = (q(lhval+ip) - s0)
          if (diff .gt. zero) then
*--- keep point - calculate number of steps for interpol.
            nrg = nrg + 1
            iq(laux(1)+nrg) = ip
            q(laux(2)+nrg)  = q(lhval+ip)
            nst  = diff / step
            stpl = diff / (nst + 1)
            do 120 j = 1, nst
              new = new + 1
              if (new .gt. nmax)  then
                ierr = 1
                goto 999
              endif
              dstp = j * stpl
              q(laux(maux)+new) = s0 + dstp
  120       continue
            new = new + 1
            if (new .gt. nmax)  then
              ierr = 1
              goto 999
            endif
            q(laux(maux)+new) = q(lhval+ip)
          endif
  130   continue
        if (nrg .lt. 3)  then
          ierr = 2
          goto 999
        endif
*--- loop over variables, spline those without codes
        i  = 0
        do 160 iv = 1, ntvvar
          do 160 jv = 1, nvvar(iv)
            i = i + 1
            ipc = iq(lpint + i)
            if (ipc .eq. 0)  then
              ki = lrvc(i)
              do 140 j = 1, nrg
  140         q(laux(3)+j) = q(ki+iq(laux(1)+j))
              call gxcubi (nrg, q(laux(2)+1), q(laux(3)+1),
     +        q(laux(4)+1), q(laux(5)+1), iecub)
              do 150 j = 1, new
                q(laux(6)+j) = gxcubv(q(laux(maux)+j), nrg,
     +          q(laux(2)+1), q(laux(3)+1), q(laux(4)+1), q(laux(5)+1))
*--- adapt min. and max.
                vmin(iv) = min(vmin(iv), q(laux(6)+j))
                vmax(iv) = max(vmax(iv), q(laux(6)+j))
  150         continue
              call ucopy(q(laux(6)+1), q(ki+1), new)
            endif
  160   continue
      endif
*--- replace s values
      npnt = new
      call ucopy(q(laux(maux)+1), q(lhval+1), npnt)
  999 end
