      subroutine twcpgo(list, tape, idisk)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track Twiss parameters with optional output, version with coupling.*
* Input:                                                               *
*   LIST      (logical) List desired.                                  *
*   TAPE      (logical) TAPE option.                                   *
* Important common data:                                               *
*             /MAPELM/  Element transfer map.                          *
*             /MAPTRN/  One turn transfer map.                         *
*             /OPTIC0/  Initial values.                                *
*             /OPTIC1/  Current values.                                *
*   LCELM     /REFER/   Current element bank.                          *
*   LCALI     /REFER/   Current misalignment pointer.                  *
*   LCFLD     /REFER/   Current field error pointer.                   *
*   LCSEQ     /REFER/   Current beam line sequence bank.               *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Added ENER1 to set the initial energy value for twiss calculation   *
* Modified: 01-APR-1999, M. Woodley (SLAC)                             *
*   Set ENER1=EN0  if ENER0=0; set ENERGY (from                        *
*   COMMON /BEAFLT/) to ENER1 value if doing tape file output           *
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
      integer icode,idisk,iecnt,ienum,iflag,iocc,ipos,jbit,jbyt
      double precision el,twopi,utwopi,wgt,zero,two
      logical           list, tape
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
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
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0 * pi, utwopi = 1.0 / twopi)
      parameter         (zero = 0.d0, two = 2.0d0)
      logical           fmap, fprt
      character*(mcnam) elmnam
 
*---- Retrieve beam line description.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Initial values for lattice functions.
      betx = betx0
      alfx = alfx0
      amux = amux0
      bety = bety0
      alfy = alfy0
      amuy = amuy0
      call ucopy(orbit0, orbit, 6*mwflt)
      call ucopy(disp0, disp, 6*mwflt)
      call ucopy(r0mat, rmat, 4*mwflt)
      suml = zero
      ener1 = ener0
      if (ener1 .eq. zero .or. .not. ereset) ener1 = en0
      if (tape) en0 = ener1
 
*---- Maximum and r.m.s. values.
      bxmax = betx
      dxmax = disp(1)
      bymax = bety
      dymax = disp(3)
      xcomax = zero
      ycomax = zero
      sigxco = zero
      sigyco = zero
      sigdx = zero
      sigdy = zero
 
*---- Initial output.
      if (list) call twcppr(' ', 1, 0, 0, 0)
      if (tape) then
        lcelm = 0
        call twbttp(1, ' ', idisk)
      endif
 
*---- Loop over positions.
      iecnt = 0
      cplxy = .false.
      cplxt = .false.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        fprt = (ipos .eq. irg1  .or.  ipos .eq. irg2  .or.
     +          jbit(iflag,mprnt) .ne. 0) .and. list
        icode = jbyt(iflag,1,mcode)
 
*---- Physical element.
        if (icode .eq. 1) then
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            call twcptk(.false.)
          endif
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
            call twcptk(.true.)
          endif
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            call twcptk(.false.)
          endif
          suml = suml + el
          bxmax = max(betx, bxmax)
          bymax = max(bety, bymax)
          dxmax = max(abs(disp(1)), dxmax)
          dymax = max(abs(disp(3)), dymax)
          xcomax = max(abs(orbit(1)), xcomax)
          ycomax = max(abs(orbit(3)), ycomax)
          sigxco = sigxco + orbit(1)**2
          sigyco = sigyco + orbit(3)**2
          sigdx = sigdx + disp(1)**2
          sigdy = sigdy + disp(3)**2
          iecnt = iecnt + 1
          if (tape) call twbttp(2, elmnam, idisk)
          if (fprt) call twcppr(elmnam, 4, ipos, ienum, iocc)
 
*---- Entrance of line.
        else if (icode .eq. 2) then
          if (fprt) call twcppr(elmnam, 5, ipos, ienum, iocc)
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            call twcptk(.false.)
          endif
 
*---- Exit of line.
        else
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            call twcptk(.false.)
          endif
          if (fprt) call twcppr(elmnam, 6, ipos, ienum, iocc)
        endif
 
*---- SAVE option.
        if (ltwfun .ne. 0) call twbtsv(2, ipos)
 
*---- Fill in SAVEBETA command, if any.
        if (jbit(iflag,msbet) .ne. 0) call twsbet(ipos, .false.)
   90 continue
 
*---- Compute summary.
      call twsumm(symm, nsup)
      wgt = max(iecnt, 1)
      sigxco = sqrt(sigxco / wgt)
      sigyco = sqrt(sigyco / wgt)
      sigdx = sqrt(sigdx / wgt)
      sigdy = sqrt(sigdy / wgt)
      cosmux = (rt(1,1) + rt(2,2)) / 2.0
      cosmuy = (rt(3,3) + rt(4,4)) / 2.0
 
*---- Final output.
      if (list) call twcppr(' ', 7, 0, 0, 0)
      if (tape) call twbttp(3, ' ', idisk)
 
*---- Warning messages.
      if (cplxt .or. dorad) then
        write (msg, 910)
        call aawarn('TWCPGO', 2, msg)
      endif
 
  910 format('TWISS uses the RF system and synchrotron radiation ',
     +       'only to find the closed orbit.'/
     +       'for optical calculations it ignores both.')
 
      end