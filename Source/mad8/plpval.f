      function plpval(ipflg, ipt, ict)
      implicit none
*----------------------------------------------------------------------*
*                                                                      *
* Purpose: calculates composite plot variables derived from TRACK table*
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
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
      integer i,icd,ict,ifdum,ipflg,ipt,ivdum,mct,mxvar
      double precision d,one,plpval,ptn,pxn,pyn,tn,tv,two,twopi,xn,yn,
     +zero
 
      parameter         (zero = 0.d0)
      parameter         (one = 1.d0, two = 2.d0, twopi = two * pi)
      parameter         (mxvar = 19, mct = 6)
 
      dimension         d(12,mxvar), tv(mct)
      character*(mcstr) sdum
      integer           iptp(mxvar)
      save              d
      data iptp / mxvar * 999999 /
 
      icd = ipflg
      plpval = zero
      if (itbv .ne. 3)  goto 999
      if (icd .le. 1 .or. icd .gt. mxvar)  goto 999
 
*---- Copy complete track vector.
      if (iq(lform+ipt+1) .eq. 3) then
        do 10  i = 1, mct
          tv(i) = q(lbuf+i+2)
   10   continue
      else
        call ucopy(q(lbuf+3), tv, 6*mwflt)
      endif
 
      goto (999,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +      110, 120, 130, 140, 150, 160, 170, 180, 190), icd
 
*==== Horizontal plane.
*---- x_n, x_ns
   20 continue
  140 continue
      call tbgdsc(ltab, 'E12', ifdum, ivdum, d(1,icd), sdum)
      call tbgdsc(ltab, 'E22', ifdum, ivdum, d(2,icd), sdum)
      call tbgdsc(ltab, 'E32', ifdum, ivdum, d(3,icd), sdum)
      call tbgdsc(ltab, 'E42', ifdum, ivdum, d(4,icd), sdum)
      call tbgdsc(ltab, 'E52', ifdum, ivdum, d(5,icd), sdum)
      call tbgdsc(ltab, 'E62', ifdum, ivdum, d(6,icd), sdum)
      plpval = d(2,icd) * tv(1) - d(1,icd) * tv(2)
     +       + d(4,icd) * tv(3) - d(3,icd) * tv(4)
     +       + d(6,icd) * tv(5) - d(5,icd) * tv(6)
 
      if (icd .eq. 14) then
        call tbgdsc(ltab, 'EX', ifdum, ivdum, ex, sdum)
        if (ex .ne. zero) plpval = plpval / sqrt(ex)
      endif
      goto 1000
 
*---- px_n, px_ns
   40 continue
  150 continue
      call tbgdsc(ltab, 'E11', ifdum, ivdum, d(1,icd), sdum)
      call tbgdsc(ltab, 'E21', ifdum, ivdum, d(2,icd), sdum)
      call tbgdsc(ltab, 'E31', ifdum, ivdum, d(3,icd), sdum)
      call tbgdsc(ltab, 'E41', ifdum, ivdum, d(4,icd), sdum)
      call tbgdsc(ltab, 'E51', ifdum, ivdum, d(5,icd), sdum)
      call tbgdsc(ltab, 'E61', ifdum, ivdum, d(6,icd), sdum)
      plpval = d(1,icd) * tv(2) - d(2,icd) * tv(1)
     +       + d(3,icd) * tv(4) - d(4,icd) * tv(3)
     +       + d(5,icd) * tv(6) - d(6,icd) * tv(5)
 
      if (icd .eq. 15) then
        call tbgdsc(ltab, 'EX', ifdum, ivdum, ex, sdum)
        if (ex .ne. zero) plpval = plpval / sqrt(ex)
      endif
      goto 1000
 
*---- W_x, W_xs, phi_x
  100 continue
  110 continue
  160 continue
      call tbgdsc(ltab, 'E11', ifdum, ivdum, d(1, icd), sdum)
      call tbgdsc(ltab, 'E21', ifdum, ivdum, d(2, icd), sdum)
      call tbgdsc(ltab, 'E31', ifdum, ivdum, d(3, icd), sdum)
      call tbgdsc(ltab, 'E41', ifdum, ivdum, d(4, icd), sdum)
      call tbgdsc(ltab, 'E51', ifdum, ivdum, d(5, icd), sdum)
      call tbgdsc(ltab, 'E61', ifdum, ivdum, d(6, icd), sdum)
      call tbgdsc(ltab, 'E12', ifdum, ivdum, d(7, icd), sdum)
      call tbgdsc(ltab, 'E22', ifdum, ivdum, d(8, icd), sdum)
      call tbgdsc(ltab, 'E32', ifdum, ivdum, d(9, icd), sdum)
      call tbgdsc(ltab, 'E42', ifdum, ivdum, d(10,icd), sdum)
      call tbgdsc(ltab, 'E52', ifdum, ivdum, d(11,icd), sdum)
      call tbgdsc(ltab, 'E62', ifdum, ivdum, d(12,icd), sdum)
      xn  = d(8, icd) * tv(1) - d(7, icd) * tv(2)
     +    + d(10,icd) * tv(3) - d(9, icd) * tv(4)
     +    + d(12,icd) * tv(5) - d(11,icd) * tv(6)
      pxn = d(1, icd) * tv(2) - d(2, icd) * tv(1)
     +    + d(3, icd) * tv(4) - d(4, icd) * tv(3)
     +    + d(5, icd) * tv(6) - d(6, icd) * tv(5)
 
      if (icd .eq. 10) then
        plpval = xn**2 + pxn**2
      else if (icd .eq. 16) then
        call tbgdsc(ltab, 'EX', ifdum, ivdum, ex, sdum)
        if (ex .ne. zero) plpval = (xn**2 + pxn**2) / ex
      else if (xn .ne. zero  .or.  pxn .ne. zero) then
        plpval = atan2(pxn, xn) / twopi
        if (plpval .lt. zero) plpval = plpval + one
      endif
      goto 1000
 
*==== Vertical plane.
*---- y_n, y_ns
   30 continue
  170 continue
      call tbgdsc(ltab, 'E14', ifdum, ivdum, d(1,icd), sdum)
      call tbgdsc(ltab, 'E24', ifdum, ivdum, d(2,icd), sdum)
      call tbgdsc(ltab, 'E34', ifdum, ivdum, d(3,icd), sdum)
      call tbgdsc(ltab, 'E44', ifdum, ivdum, d(4,icd), sdum)
      call tbgdsc(ltab, 'E54', ifdum, ivdum, d(5,icd), sdum)
      call tbgdsc(ltab, 'E64', ifdum, ivdum, d(6,icd), sdum)
      plpval = d(2,icd) * tv(1) - d(1,icd) * tv(2)
     +       + d(4,icd) * tv(3) - d(3,icd) * tv(4)
     +       + d(6,icd) * tv(5) - d(5,icd) * tv(6)
 
      if (icd .eq. 17) then
        call tbgdsc(ltab, 'EY', ifdum, ivdum, ey, sdum)
        if (ex .ne. zero) plpval = plpval / sqrt(ey)
      endif
      goto 1000
 
*---- py_n, py_ns
   50 continue
  180 continue
      call tbgdsc(ltab, 'E13', ifdum, ivdum, d(1,icd), sdum)
      call tbgdsc(ltab, 'E23', ifdum, ivdum, d(2,icd), sdum)
      call tbgdsc(ltab, 'E33', ifdum, ivdum, d(3,icd), sdum)
      call tbgdsc(ltab, 'E43', ifdum, ivdum, d(4,icd), sdum)
      call tbgdsc(ltab, 'E53', ifdum, ivdum, d(5,icd), sdum)
      call tbgdsc(ltab, 'E63', ifdum, ivdum, d(6,icd), sdum)
      plpval = d(1,icd) * tv(2) - d(2,icd) * tv(1)
     +       + d(3,icd) * tv(4) - d(4,icd) * tv(3)
     +       + d(5,icd) * tv(6) - d(6,icd) * tv(5)
 
      if (icd .eq. 18) then
        call tbgdsc(ltab, 'EY', ifdum, ivdum, ey, sdum)
        if (ey .ne. zero) plpval = plpval / sqrt(ey)
      endif
      goto 1000
 
*---- W_y, W_ys, phi_y
  120 continue
  130 continue
  190 continue
      call tbgdsc(ltab, 'E13', ifdum, ivdum, d(1, icd), sdum)
      call tbgdsc(ltab, 'E23', ifdum, ivdum, d(2, icd), sdum)
      call tbgdsc(ltab, 'E33', ifdum, ivdum, d(3, icd), sdum)
      call tbgdsc(ltab, 'E43', ifdum, ivdum, d(4, icd), sdum)
      call tbgdsc(ltab, 'E53', ifdum, ivdum, d(5, icd), sdum)
      call tbgdsc(ltab, 'E63', ifdum, ivdum, d(6, icd), sdum)
      call tbgdsc(ltab, 'E14', ifdum, ivdum, d(7, icd), sdum)
      call tbgdsc(ltab, 'E24', ifdum, ivdum, d(8, icd), sdum)
      call tbgdsc(ltab, 'E34', ifdum, ivdum, d(9, icd), sdum)
      call tbgdsc(ltab, 'E44', ifdum, ivdum, d(10,icd), sdum)
      call tbgdsc(ltab, 'E54', ifdum, ivdum, d(11,icd), sdum)
      call tbgdsc(ltab, 'E64', ifdum, ivdum, d(12,icd), sdum)
      yn  = d(8, icd) * tv(1) - d(7, icd) * tv(2)
     +    + d(10,icd) * tv(3) - d(9, icd) * tv(4)
     +    + d(12,icd) * tv(5) - d(11,icd) * tv(6)
      pyn = d(1, icd) * tv(2) - d(2, icd) * tv(1)
     +    + d(3, icd) * tv(4) - d(4, icd) * tv(3)
     +    + d(5, icd) * tv(6) - d(6, icd) * tv(5)
 
      if (icd .eq. 12) then
        plpval = yn**2 + pyn**2
      else if (icd .eq. 19) then
        call tbgdsc(ltab, 'EY', ifdum, ivdum, ey, sdum)
        if (ey .ne. zero) plpval = (yn**2 + pyn**2) / ey
      else if (yn .ne. zero  .or.  pyn .ne. zero) then
        plpval = atan2(pyn, yn) / twopi
        if (plpval .lt. zero) plpval = plpval + one
      endif
      goto 1000
 
*==== Longitudinal plane.
*---- t_n
   60 continue
      call tbgdsc(ltab, 'E16', ifdum, ivdum, d(1,icd), sdum)
      call tbgdsc(ltab, 'E26', ifdum, ivdum, d(2,icd), sdum)
      call tbgdsc(ltab, 'E36', ifdum, ivdum, d(3,icd), sdum)
      call tbgdsc(ltab, 'E46', ifdum, ivdum, d(4,icd), sdum)
      call tbgdsc(ltab, 'E56', ifdum, ivdum, d(5,icd), sdum)
      call tbgdsc(ltab, 'E66', ifdum, ivdum, d(6,icd), sdum)
      plpval = d(2,icd) * tv(1) - d(1,icd) * tv(2)
     +       + d(4,icd) * tv(3) - d(3,icd) * tv(4)
     +       + d(6,icd) * tv(5) - d(5,icd) * tv(6)
      goto 1000
 
*---- pt_n
   70 continue
      call tbgdsc(ltab, 'E15', ifdum, ivdum, d(1,icd), sdum)
      call tbgdsc(ltab, 'E25', ifdum, ivdum, d(2,icd), sdum)
      call tbgdsc(ltab, 'E35', ifdum, ivdum, d(3,icd), sdum)
      call tbgdsc(ltab, 'E45', ifdum, ivdum, d(4,icd), sdum)
      call tbgdsc(ltab, 'E55', ifdum, ivdum, d(5,icd), sdum)
      call tbgdsc(ltab, 'E65', ifdum, ivdum, d(6,icd), sdum)
      plpval = d(1,icd) * tv(2) - d(2,icd) * tv(1)
     +       + d(3,icd) * tv(4) - d(4,icd) * tv(3)
     +       + d(5,icd) * tv(6) - d(6,icd) * tv(5)
      goto 1000
 
*---- W_t, phi_t
   80 continue
   90 continue
      call tbgdsc(ltab, 'E15', ifdum, ivdum, d(1, icd), sdum)
      call tbgdsc(ltab, 'E25', ifdum, ivdum, d(2, icd), sdum)
      call tbgdsc(ltab, 'E35', ifdum, ivdum, d(3, icd), sdum)
      call tbgdsc(ltab, 'E45', ifdum, ivdum, d(4, icd), sdum)
      call tbgdsc(ltab, 'E55', ifdum, ivdum, d(5, icd), sdum)
      call tbgdsc(ltab, 'E65', ifdum, ivdum, d(6, icd), sdum)
      call tbgdsc(ltab, 'E16', ifdum, ivdum, d(7, icd), sdum)
      call tbgdsc(ltab, 'E26', ifdum, ivdum, d(8, icd), sdum)
      call tbgdsc(ltab, 'E36', ifdum, ivdum, d(9, icd), sdum)
      call tbgdsc(ltab, 'E46', ifdum, ivdum, d(10,icd), sdum)
      call tbgdsc(ltab, 'E56', ifdum, ivdum, d(11,icd), sdum)
      call tbgdsc(ltab, 'E66', ifdum, ivdum, d(12,icd), sdum)
      tn  = d(8, icd) * tv(1) - d(7, icd) * tv(2)
     +    + d(10,icd) * tv(3) - d(9, icd) * tv(4)
     +    + d(12,icd) * tv(5) - d(11,icd) * tv(6)
      ptn = d(1, icd) * tv(2) - d(2, icd) * tv(1)
     +    + d(3, icd) * tv(4) - d(4, icd) * tv(3)
     +    + d(5, icd) * tv(6) - d(6, icd) * tv(5)
 
      if (icd .eq. 8) then
        plpval = tn**2 + ptn**2
      else if (tn .ne. zero  .or.  ptn .ne. zero) then
        plpval = atan2(ptn, tn) / twopi
        if (plpval .lt. zero) plpval = plpval + one
      endif
      goto 1000
 
 1000 iptp(icd) = ipt
  999 end
