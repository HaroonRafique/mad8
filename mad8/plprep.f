      subroutine plprep (ipict, ityp, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Prepare plot banks for routine PLPLOT                              *
*                                                                      *
*--- Input:                                                            *
*    IPICT   picture number                                            *
*    ITYP    picture type:                                             *
*            0: all points in one frame                                *
*            1: parameter is row variable                              *
*            2: parameter is segment variable                          *
*--- Output:                                                           *
*    IERR    0 if everything OK                                        *
*            else > 0                                                  *
*                                                                      *
*----------------------------------------------------------------------*
* Modified: 13-JAN-1999, M. Woodley (SLAC)                             *
*   Add sextupoles and octupoles to machine layout                     *
* Modified: 17-MAR-1999, M. Woodley (SLAC)                             *
*   Add lcavity elements to machine layout                             *
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
      integer ietflg,ipnflg, liftbeam,currbeam
      double precision alfa,amass,arad,beta,betas,bunch,bxmax,bymax,
     +charge,cosmux,cosmuy,currnt,deltas,deltat,dtbyds,dxmax,dymax,
     +et,ex,exn,ey,eyn,freq0,gamma,gammas,gamtr,parnum,pc,pdamp,
     +qs,qx,qy,sigdx,sigdy,sige,sigt,sigx,sigxco,sigy,sigyco,sinmux,
     +sinmuy,u0,xcomax,xix,xiy,ycomax,en0,beambv,elkfact,elmfact
 
*---- Particles, emittances and sigmas.
      integer mfact, mbmult
      parameter (mfact = 50, mbmult = 20)
      common /beanam/   prtnam, bsequnam,
     +                  prtnames(mttact), bseqnames(mttact)
      common /beaflt/   amass, charge, en0, pc, gamma,
     +                  ex, exn, ey, eyn, et, sigt, sige,
     +                  bunch, parnum, currnt
      common /beaaux/   sigx, qx, xix, cosmux, sinmux, bxmax, dxmax,
     +                  xcomax, sigxco, sigdx,
     +                  sigy, qy, xiy, cosmuy, sinmuy, bymax, dymax,
     +                  ycomax, sigyco, sigdy,
     +                  qs, alfa, gamtr, deltas, dtbyds, deltat,
     +                  freq0, beta, u0, arad, beambv, pdamp(3),
     +                  gammas, betas,
     +                  elkfact(mfact), elmfact(0:mbmult)
      common /beaint/   ietflg, ipnflg, liftbeam, currbeam
      save   /beaint/
      common /bealog/   fbch, frad
      save              /beanam/, /beaflt/, /beaaux/, /bealog/
      logical           fbch, frad
      character*(mcnam) prtnam, bsequnam, prtnames, bseqnames
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
      integer i,i1,i2,ic,icopy,idum,ienum,ierp,ierr,iflag,ihbias,ihc,
     +ihf,ihform,ihp,ii,ilo,index,iocc,ipelm,ipict,ipush,irdum1,irdum2,
     +is,isp,isup,itp,ityp,iup,iv,ivbias,ivc,ivf,ivform,ivp,ivpl,j,jbyt,
     +jlo,jrow,jseg,jup,jv,k,k1,k2,kf,kl,l,lstitl,lzlast,m,n,natl,nbook,
     +nbtl,nctl,nel,npnt,nqcnd,ns,nstep,nvc,nvrw,nvsg
      double precision plpval,tval
 
      integer              ivord(4)
 
*--- dummy common to align TVAL
      common / tvdumm /    tval
      integer              ieqd(2)
      equivalence          (tval, ieqd(1))
 
      real                 hmin, hmax, rval, vmin(4), vmax(4)
      real                 screen, selem, xs
 
      logical              eflag, symm
 
      character*(mcnam) linnam
      character*(mcrng) rngnam
      character*(mtitl) s, stitl, sbottl
      character*(mxlabl) slab
      character*(mcnam) sym, elmnam, saxis(mpmxvr), sdum(mxdep)
      character*26      supcas,slocas
 
      data ivord /1, 4, 2, 3/
      data screen / 29. /, selem / 0.2 /
      data supcas /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      data slocas /'abcdefghijklmnopqrstuvwxyz'/
 
      ierr   = 0
*--- flag for machine layout plot
*--- temporary definition of fpmach !!
      fpmach = haxis(:2) .eq. 'S ' .and. itbv .eq. 1
*--- flag for Arnold web plot (set true if constraints read)
      if (ivnarw .ne. 0)  then
        ivnarw = 0
        do 1  i = 1, 4
          do 1  j = 1, nvvar(i)
          if (vaxis(j,i)(1:1) .eq. 'Q')  ivnarw = i
    1   continue
        if (itbv .ne. 2 .or. haxis(1:1) .ne. 'Q') ivnarw = 0
      endif
      lpparl = lq(lroot - mpparl)
      if (lpparl .eq. 0) then
        call aafail('PLPREP', 1, 'PLOT parameter bank not found.')
        ierr = 1
        goto 999
      endif
      if (fpmach .and. splifl .and. itbv .lt. mtbv)  then
*--- calculate min. no. of points across (see interpolation)
        call gxqvar('XMETAF', idum, xs, s)
        nstep = max(screen, xs) / selem + 2.
        nstep = min(nstep, maxitp)
      else
        nstep = 0
      endif
*--- final definition of fpmach (the HELPFL contains the NOLINE option)
      fpmach = fpmach .and. .not. helpfl
      nvrw = iq(ltbr + 1)
      nvsg = iq(ltbr + 2)
      if (ityp .eq. 0)  then
*--- all points in one frame
        ilo = 1
        iup = nvsg
        jlo = 1
        jup = nvrw
        stitl = ' '
        lstitl  = 1
      else
        call plgetn(1, sparm, itbv, idum, sdum, slab)
        call gxpnbl (slab, kf, kl)
        stitl  = slab(:kl) // ' = '
        lstitl = kl + 3
        if (ityp .eq. 1) then
*--- parameter is row variable
          ilo = 1
          iup = nvsg
          jlo = ipict
          jup = ipict
        else
*--- parameter is segment variable
          ilo = ipict
          iup = ipict
          jlo = 1
          jup = nvrw
        endif
      endif
      npnt = (iup + 1 - ilo) * (jup + 1 - jlo)
      if (npnt .le. 1 .and. itbv .eq. 1)  then
        call aawarn('PLPREP', 1, 'Zero or one point only.')
        ierr = 2
        goto 999
      elseif (npnt .le. 0)  then
        call aawarn('PLPREP', 1, 'No points to plot.')
        ierr = 2
        goto 999
      elseif (npnt .gt. maxppt)  then
        s = '         points cut at maximum'
        write (s(:8),'(I8)')  npnt
        write (s(32:39),'(I8)') maxppt
        call aawarn('PLPREP', 1, s)
        npnt = maxppt
      endif
*--- initial length for coord. banks
      nbook = npnt + nstep
*--- fill in SETPLOT options
      iq(lpmain + mpfont) = iq(lpparl + 1)
      q(lpmain + mplscw) = q(lpparl + 2)
      q(lpmain + mpxsiz) = q(lpparl + 3)
      q(lpmain + mpysiz) = q(lpparl + 4)
      q(lpmain + mpascl) = q(lpparl + 5)
      q(lpmain + mplscl) = q(lpparl + 6)
      q(lpmain + mpsscl) = q(lpparl + 7)
      q(lpmain + mptscl) = q(lpparl + 8)
*--- Prepare frames
      iframe = 1
      natl   = mpname + mtitl / mcwrd
      nbtl   = mpbtit + mtitl / mcwrd
      nctl   = mpanno + mtitl / mcwrd
        call mzbook (1, lframe, lpmain, -iframe, 'FRAM', 3, 3, nbtl, 0,
     +  -1)
        call uctoh (haxis, q(lframe + mpname), mcwrd, mcnam)
*--- copy top title from standard title
        call uctoh (title, q(lframe + mpttit), mcwrd, mtitl)
*--- compose bottom title and store
        call plptit(2, sbottl)
        call uctoh (sbottl, q(lframe + mpbtit), mcwrd, mtitl)
*--- book bank for horizontal coord.s
        call mzbook(1, lhval, lframe, -2, 'HVAL', 0, 0, nbook, 0, -1)
*--- book bank for horizontal coord.s sequence index (sorting)
        call mzbook(1, lindx, lframe, -3, 'INDX', 0, 0, nbook, 0, -1)
*--- book bank for temporary storage
        call mzbook(1, ltmp, ltbr, -9, 'TMP ', 0, 0, nbook, 0, -1)
*--- zero count for total number of vert. var.
        nvc = 0
*--- book mother banks for vert. axes
        do 30 iv = 1, ntvvar
          if (iv .eq. 1)  then
            call mzbook (1, lrvv(iv), lframe, -1, 'VVAR', 1, 1, natl, 0,
     +      -1)
          else
            l = lzlast (0, lframe - 1)
            call mzbook (1, lrvv(iv), l, 0, 'VVAR', 1, 1, natl, 0, -1)
          endif
*--- set axis reference number
          iq(lrvv(iv) + mpvaxr) = ivord(iv)
*--- get axis annotation
          if (nvvar(iv) .eq. 1)  then
            call plgetn (1, vaxis(1,iv), itbv, idum, sdum, slab)
            ns = 1
          else
            call plgaxn (nvvar(iv), vaxis(1,iv), saxis, ns)
            call plgetn (1, saxis(1), itbv, idum, sdum, slab)
          endif
          call gxpnbl (slab, k1, k2)
          s  = '<#>' // slab
          k2 = k2 + 3
          do 10 i = 2, ns
            call plgetn (1, saxis(i), itbv, idum, sdum, slab)
            call gxpnbl (slab, i1, i2)
            if (index(s(:k2),slab(:i2)) .eq. 0)  then
              s(k2 + 1:) = ', ' // slab(:i2)
              k2 = k2 + i2 + 2
            endif
   10     continue
          call uctoh (s, q(lrvv(iv) + mpname), mcwrd, mtitl)
          do 20 ic = 1, nvvar(iv)
            nvc = nvc + 1
*--- book curve banks per axis
            if (ic .eq. 1)  then
              call mzbook (1, lvcurv, lrvv(iv), -1, 'CURV', 4, 2, nctl,
     +        0, -1)
            else
              l = lzlast (0,lrvv(iv)-1)
              call mzbook (1, lvcurv, l, 0, 'CURV', 4, 2, nctl, 0, -1)
            endif
*--- set style, symbol, colour, spline flag
            iq(lvcurv + mpstyl) = ivpar(2)
            iq(lvcurv + mpbars) = ivpar(1)
            iq(lvcurv + mpcolr) = ivpar(5)
            if (splifl .and. nstep .eq. 0)  then
*--- use splines in PLCURV
              iq(lvcurv + mpspli) = 1
            else
*--- use routine PLINTP for interpolation, if any
              iq(lvcurv + mpspli) = 0
            endif
            is = ivpar(3)
            sym = ' '
            if (is .gt. 0 .and. is .le. 5 .or. is .eq. 100)  then
              continue
            elseif (is .eq. 200)  then
              m = mod(nvc - 1, 26) + 1
              sym(:1) = supcas(m:m)
            elseif (is .eq. 300)  then
              m = mod(nvc - 1, 26) + 1
              sym(:1) = slocas(m:m)
              is = 200
            elseif (is .gt. 200 .and. is .le. 226)  then
              m = is - 200
              sym(:1) = supcas(m:m)
              is = 200
            elseif (is .gt. 300 .and. is .le. 326)  then
              m = is - 300
              sym(:1) = slocas(m:m)
              is = 200
            elseif (is .ge. 400 .and. is .le. 409)  then
              write (sym(:1), '(I1)')  is - 400
              is = 200
            else
              is = 0
            endif
            iq(lvcurv + mpsymf) = is
            call uctoh (sym, q(lvcurv + mpsymb), mcwrd, mcnam)
*--- curve annotation
            call uctoh (vaxis(ic,iv), q(lvcurv + mpanno), mcwrd, mcnam)
*--- set ref. link to hor. coord.s
            lq(lvcurv-3) = lhval
*--- set ref. link to hor. coord. index
            lq(lvcurv-4) = lindx
*--- book bank for vert. var. values
            call mzbook (1, lrvc(nvc), lvcurv, -1, 'VCOR', 0, 0, nbook,
     +      0, -1)
   20     continue
   30   continue
*--- get links for valid deltap, segments, rows
        lvrw = lq(ltbr - 1)
        lvsg = lq(ltbr - 2)
*--- fill hor. and all vert. var.
        n = 0
        do 100 i = ilo, iup
          jseg = iq(lvsg + i)
          call tbseg (ltab, jseg, eflag)
          do 90 j = jlo, jup
            jrow = iq(lvrw + j)
            call tbset (ltab, jrow, 1, lbuf)
            if (n .eq. 0)  then
              if (ityp .ne. 0) then
*--- put parameter value in bottom title
                if (idsfrm(ityp) .eq. 2) then
                  ii = iq(lbuf+idsbis(ityp)+1)
                  write (stitl(lstitl+1:lstitl+12), '(I8)') ii
                else
                  if (idsfrm(ityp) .eq. 3) then
                    rval = q(lbuf+idsbis(ityp)+1)
                  else
                    do 40 icopy = 1, mwflt
   40               ieqd(icopy) = iq(lbuf+idsbis(ityp)+icopy)
                    rval = tval
                  endif
                  if (rval .ne. 0.) then
                    stitl(lstitl+1:lstitl+2) = '0.'
                  else
                    write (stitl(lstitl+1:lstitl+12), '(G12.6)') rval
                  endif
                endif
                call uctoh (stitl, q(lframe + mpbtit), mcwrd, mtitl)
              endif
            endif
            n = n + 1
            if (n .gt. npnt)  goto 100
*--- get hor. variable value
            if (ihpntr .lt. 0)  then
*--- expression variable
              call exevl1(lq(lexpv+ihpntr), ltab, lbuf, tval)
*--- RVAL is single prec. always !
              rval = tval
            else
              ihp = iq(locc+ihpntr)
              ihc = iq(lcnt+ihpntr)
              ihf = iq(lproc+ihpntr)
              if (ihf .le. 1)  then
                ihform = iq(lform+ihp+1)
                ihbias = iq(lbias+ihp+1)
                if (ihform .eq. 2) then
                  rval = iq(lbuf + ihbias + 1)
                elseif (ihform .eq. 3) then
                  rval = q(lbuf + ihbias + 1)
                else
                  do 50 icopy = 1, mwflt
   50            ieqd(icopy) = iq(lbuf+ihbias+icopy)
                  rval = tval
                endif
                if (ihf .eq. 1)  rval = sqrt (abs (rval))
              else
*--- composit variable
                rval = plpval(ihf, ihp, ihc)
              endif
            endif
            if (ityp .eq. 0)  then
              q(lhval + (j-1)*nvsg+i) = rval
            else
              q(lhval + n) = rval
            endif
            if (n .eq. 1)  then
              hmin = rval
              hmax = rval
            else
              hmin = min (hmin, rval)
              hmax = max (hmax, rval)
            endif
*--- get vert. var. values
            k = 0
            do 80 iv = 1, ntvvar
              do 70 jv = 1, nvvar(iv)
                k = k + 1
                ivpl = ivpntr(jv,iv)
                if (ivpl .lt. 0)  then
*--- expression variable
                  call exevl1(lq(lexpv+ivpl), ltab, lbuf, tval)
                  rval = tval
                else
                  ivp = iq(locc+ivpl)
                  ivc = iq(lcnt+ivpl)
                  ivf = iq(lproc+ivpl)
                  if (ivf .le. 1)  then
                    ivform = iq(lform+ivp+1)
                    ivbias = iq(lbias+ivp+1)
                    if (ivform .eq. 2) then
                      rval = iq(lbuf + ivbias + 1)
                    elseif (ivform .eq. 3) then
                      rval = q(lbuf + ivbias + 1)
                    else
                      do 60 icopy = 1, mwflt
   60                 ieqd(icopy) = iq(lbuf+ivbias+icopy)
                      rval = tval
                    endif
                    if (ivf .eq. 1)  rval = sqrt (abs (rval))
                  else
*--- composit variable
                    rval = plpval(ivf, ivp, ivc)
                  endif
                endif
                if (ityp .eq. 0)  then
                  q(lrvc(k) + (j-1)*nvsg+i) = rval
                else
                  q(lrvc(k) + n) = rval
                endif
                if (n .eq. 1 .and. jv .eq. 1)  then
                  vmin(iv) = rval
                  vmax(iv) = rval
                else
                  vmin(iv) = min (vmin(iv), rval)
                  vmax(iv) = max (vmax(iv), rval)
                endif
   70         continue
   80       continue
   90     continue
  100   continue
*--- reset position of first element
      if (fpmach)  then
*--- Book two banks for machine description
        nel = irg2 - irg1
        call mzbook (1, lm1, lpmain, -(mpfram + 1), 'PTYP', 0, 0, nel,
     +  0, -1)
        call mzbook (1, lm2, lpmain, -(mpfram + 2), 'PLEN', 0, 0, nel,
     +  0, -1)
*--- s position at start of first element
        q(lpmain + mpfelm) = q(lhval + 1)
*--- Fill banks with machine description
        i = 0
        do 130 jrow = irg1, irg2
          call utelem (lcseq, jrow, iflag, elmnam, iocc, ienum)
          if (jbyt (iflag, 1, mcode) .eq. 1)  then
            if (jrow .eq. irg1) goto 130
            i = i + 1
            isp = iq(lcelm + mbsp)
            if (isp .eq. 1)  then
*--- drift
              itp = 0
            elseif (isp .eq. 2 .or. isp .eq. 3)  then
*--- bend
              do 109 icopy = 1, mwflt
  109         ieqd(icopy) = iq(lcelm+metltb-1+icopy)
              rval = tval
              if (rval .eq. 0.)  then
                itp = 1
              else
                itp = 7
              endif
            elseif (isp .eq. 5)  then
*--- quad
              do 110 icopy = 1, mwflt
  110         ieqd(icopy) = iq(lcelm+mek1q-1+icopy)
              rval = tval * elkfact(5)
              if (rval .gt. 0.)  then
*--- focussing quad
                itp = 2
              elseif (rval .lt. 0.)  then
                itp = 3
              else
                itp = 0
              endif
*--- start addition from TR
            elseif (isp .eq. 6)  then
*--- sext
              do 111 icopy = 1, mwflt
  111         ieqd(icopy) = iq(lcelm+mek2s-1+icopy)
              rval = tval * elkfact(6)
              if (rval .gt. 0.)  then
*--- positive sext
                itp = 10
              elseif (rval .lt. 0.)  then
                itp = 11
              else
                itp = 0
              endif
            elseif (isp .eq. 7)  then
*--- oct
              do 112 icopy = 1, mwflt
  112         ieqd(icopy) = iq(lcelm+mek3o-1+icopy)
              rval = tval * elkfact(7)
              if (rval .gt. 0.)  then
*--- positive oct
                itp = 12
              elseif (rval .lt. 0.)  then
                itp = 13
              else
                itp = 0
              endif
*--- end addition from TR
            elseif (isp .eq. 14)  then
*--- horizontal kicker
              itp = 1
            elseif (isp .eq. 15 .or. isp .eq. 16)  then
*--- kicker or vertical kicker
              itp = 7
            elseif (isp .ge. 17 .and. isp .le. 19)  then
*--- monitor
              itp = 4
            elseif (isp .eq. 20 .or. isp .eq. 21)  then
*--- collimator
              itp = 5
            elseif (isp .eq. 11)  then
*--- electrostatic separator
              itp = 6
            elseif (isp .eq. 8)  then
*--- (thin) multipole - plot like dipole
              itp = 8
            elseif (isp .eq. 10)  then
*--- RF cavity
              itp = 9
            elseif (isp .eq. 27)  then
*--- lcavity
              itp = 14
            else
              itp = 0
            endif
            do 120 icopy = 1, mwflt
  120       ieqd(icopy) = iq(lcelm+melen-1+icopy)
            iq(lm1 + i) = itp
            if(itp .eq. 8)  tval = 0
            q(lm2 + i)  = tval
          endif
  130   continue
        call mzpush(0, lm1, 0, i - nel, 'I')
        call mzpush(0, lm2, 0, i - nel, 'I')
      endif
*--- if tunes table and Q-plot, decode constraints for Arnold web
      if (ivnarw .ne. 0)  then
        call mzbook (1, lqv1, lpmain, -(mpfram+3), 'ARWE', 0, 0,
     +  mxqbnk, 0, -1)
*---  # sup.-per., and symm. flag of current line sequence
        call utbeam(lcseq, irdum1, irdum2, symm, isup, linnam,
     +  rngnam)
        if (symm) isup = 2 * isup
        iq(lqv1+1) = isup
        call plgarw (mxqbnk, mxqcnd, qcond, iqrang, nqcnd,
     +  iq(lqv1+1), ierr)
        if (ierr .ne. 0)  then
          call aawarn('PLPREP', 1,
     +    'Error in Arnold web constraint --- no web.')
          call mzdrop (0, lqv1, ' ')
          ivnarw = 0
        else
          call mzpush (0, lqv1, 0, nqcnd - mxqbnk, 'I')
        endif
      endif
*--- sort if requested  and if necessary
        if (sortfl .and. .not. fpmach)  then
          call sortzv(q(lhval + 1), iq(lindx + 1), n, 1, 0, 0)
          do 140 j = 1, n
  140     q(ltmp+j) = q(lhval+iq(lindx+j))
          call ucopy(q(ltmp+1), q(lhval+1), n)
          do 160 i = 1, k
            do 150 j = 1, n
  150       q(ltmp+j) = q(lrvc(i)+iq(lindx+j))
            call ucopy(q(ltmp+1), q(lrvc(i)+1), n)
  160     continue
        endif
        call mzdrop(0, ltmp, ' ')
*--- if hor. var. = 's' and alfa, beta, mu, or D plotted, interpolate
*    with correct formulae.
        if (nstep .gt. 0 .and. npnt .gt. 2)  then
*--- interpolate
          call plintp(ipelm, npnt, nbook, (hmax - hmin) / nstep,
     +    vmin, vmax, ierp)
          if (ierp .ne. 0)  then
            call aawarn('PLPREP', 1,
     +      'Error in interpolation --- SPLINE flag ignored.')
          endif
        endif
*--- reduce bank lengths to correct values
        do 170 i = 1, k
          ipush = npnt - iq(lrvc(i)-1)
          if (ipush .lt. 0) call mzpush(0, lrvc(i), 0, ipush, 'I')
  170   continue
        ipush = npnt - iq(lhval-1)
        if (ipush .lt. 0)  call mzpush(0, lhval, 0, ipush, ' ')
        ipush = npnt - iq(lindx-1)
        if (ipush .lt. 0)  call mzpush(0, lindx, 0, ipush, ' ')
*--- set index array to standard order
        do 180 j = 1, npnt
  180   iq(lindx + j) = j
*--- if FFT requested call special FFT routine
      if (fftfl)  then
        call plfft
      else
*--- follow user options:
*    both min and max specified:  use as range
*    only min  =  0. specified :  use 0. as upper (or lower) range value
*    only max  =  0. specified :  make symmetric around 0.
*    else                      :  use fully automatic scaling
        if (hrange(1) .ne. pflmax .and. hrange(2) .ne. pflmax
     +  .and. hrange(2) .gt. hrange(1))  then
          q(lframe + mpmin) = hrange(1)
          q(lframe + mpmax) = hrange(2)
          iq(lframe + mpsclf) = 3
        else
          q(lframe + mpmin) = hmin
          q(lframe + mpmax) = hmax
          if (hrange(1) .eq. 0. .and. hrange(2) .eq. pflmax)  then
            iq(lframe + mpsclf) = 1
          elseif (hrange(1) .eq. pflmax .and. hrange(2) .eq. 0.)  then
            iq(lframe + mpsclf) = 2
          else
            iq(lframe + mpsclf) = 0
          endif
        endif
        do 190 iv = 1,ntvvar
          if (vrange(1,iv) .ne. pflmax .and. vrange(2,iv) .ne. pflmax
     +    .and. vrange(2,iv) .gt. vrange(1,iv)) then
            q(lrvv(iv) + mpmin)  = vrange(1,iv)
            q(lrvv(iv) + mpmax)  = vrange(2,iv)
            q(lrvv(iv) + mpsclf) = 3
          else
            q(lrvv(iv) + mpmin)  = vmin(iv)
            q(lrvv(iv) + mpmax)  = vmax(iv)
            if (vrange(1,iv) .eq. 0. .and. vrange(2,iv) .eq. pflmax)
     +      then
              iq(lrvv(iv) + mpsclf) = 1
            elseif (vrange(1,iv) .eq. pflmax
     +      .and. vrange(2,iv) .eq. 0.) then
              iq(lrvv(iv) + mpsclf) = 2
            else
              iq(lrvv(iv) + mpsclf) = 0
            endif
          endif
  190   continue
      endif
  999 end
