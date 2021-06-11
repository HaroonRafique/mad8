      subroutine twopgo(cent, list)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Selective Twiss output.                                            *
* Input:                                                               *
*   CENT      (logical) True if values desired at centres.             *
*   LIST      (logical) True if listing is desired.                    *
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
*   Added ENER1 to set the initial energy value for twiss calculation;  *
*   did not include energy values in output; modified optics tracking  *
*   to include energy                                                  *
* Modified: 01-APR-1999, M. Woodley (SLAC)                             *
*   Set ENER1=EN0                       if ENER0=0                     *
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
      integer icode,ienum,iflag,iform,iocc,ipos,jbit,jbyt
      double precision el,fract,half,twopi,utwopi,zero
      logical           cent, list
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
 
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
      parameter         (zero  = 0.0d0, half = 0.5d0)
      logical           fmap, fprt
      character*(mcnam) elmnam, eltab(4), extab(4)
 
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
      suml = zero
 
*---- Initial values for chromatic functions.
      wx   = wx0
      phix = phix0
      dmux = dmux0
      wy   = wy0
      phiy = phiy0
      dmuy = dmuy0
      call ucopy(ddisp0, ddisp, 6*mwflt)
 
      ener1 = ener0
      if (ener1 .eq. zero .or. .not. ereset) ener1 = en0
 
*---- Loop over positions.
      cplxy = .false.
      cplxt = .false.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        fprt = jbit(iflag,moptc) .ne. 0
        icode = jbyt(iflag,1,mcode)
 
*==== Physical elements.
        if (icode .eq. 1) then
 
*---- Names for CENTRE or EXIT.
          eltab(1) = elmnam
          lckey = lq(lcelm+1)
          call diname(ldkey, iq(lckey+mbnam), eltab(2))
          lccls = lq(lcelm-iq(lcelm+mbat)-mbecls)
          if (jbit(iq(lcelm),mxcls) .ne. 0  .or.  lccls .eq. 0)
     +      lccls = lcelm
          call diname(ldbnk, iq(lccls+mbnam), eltab(3))
          eltab(4) = '~'
 
*---- Names for SPLIT.
          call utgnam(lcelm, 1, 1, eltab(4))
          extab(1) = '~'
          extab(2) = 'SPLIT'
          extab(3) = elmnam
          extab(4) = '~'
 
*---- Misalignment at entrance.
          if (lcali .ne. 0) then
            call tmali1(ipos, .true., orbit, orbit, re, te)
            call twbttk(.true., .false.)
          endif
 
*---- Output at selected split position(s).
          lcspl = 0
          if (lq(lcseq-msspl) .ne. 0) lcspl = lq(lq(lcseq-msspl)-ipos)
 
*---- Output for SPLIT fractions in first half of element.
   10     if (lcspl .ne. 0) then
            call ucopy(q(lcspl+mwnam+3), fract, mwflt)
            if (fract .lt. half) then
              call uhtoc(q(lcspl+2), mcwrd, extab(1), mcnam)
              call twopsv(2, 3, extab, fract)
              lcspl = lq(lcspl)
              go to 10
            endif
          endif
 
*---- Output for CENTRE option.
          if (fprt .and. cent) then
            call twopsv(2, 3, eltab, half)
          endif
 
*---- Output for SPLIT fractions in second half of element.
   20     if (lcspl .ne. 0) then
            call ucopy(q(lcspl+mwnam+3), fract, mwflt)
            call uhtoc(q(lcspl+2), mcwrd, extab(1), mcnam)
            call twopsv(2, 3, extab, fract)
            lcspl = lq(lcspl)
            go to 20
          endif
 
*---- Find transfer map and move through element.
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
            suml = suml + el
            call twbttk(.true., .true.)
          endif
 
*---- Misalignment at exit.
          if (lcali .ne. 0) then
            call tmali2(ipos, .true., orbit, orbit, re, te)
            call twbttk(.true., .false.)
          endif
 
*---- Output at exit of element.
          if (fprt  .and.  .not. cent) call twopsv(2, 2, eltab, zero)
 
*==== Entrance of line.
        else if (icode .eq. 2) then
          if (fprt) then
            eltab(1) = elmnam
            eltab(2) = 'LINE'
            eltab(3) = 'LINE'
            eltab(4) = '~'
            call twopsv(2, 1, eltab, zero)
          endif
          if (lcali .ne. 0) then
            call tmali1(ipos, .true., orbit, orbit, re, te)
            call twbttk(.true., .false.)
          endif
 
*==== Exit of line.
        else
          if (lcali .ne. 0) then
            call tmali2(ipos, .true., orbit, orbit, re, te)
            call twbttk(.true., .false.)
          endif
          if (fprt) then
            eltab(1) = elmnam
            eltab(2) = 'LINE'
            eltab(3) = 'LINE'
            eltab(4) = '~'
            call twopsv(2, 1, eltab, zero)
          endif
        endif
 
*---- Fill in SAVEBETA command, if any.
        if (jbit(iflag,msbet) .ne. 0) call twsbet(ipos, .false.)
   90 continue
 
*---- Compute summary data.
      call twsumm(symm, nsup)
      iform = 3
      if (double) iform = mreal
      call tbpdsc(ltwopt, 'DELTA',  iform, 0, deltas, ' ')
      call tbpdsc(ltwopt, 'CIRCUM', mreal, 0, circ,   ' ')
      call tbpdsc(ltwopt, 'QX',     iform, 0, qx,     ' ')
      call tbpdsc(ltwopt, 'QY',     iform, 0, qy,     ' ')
      call tbpdsc(ltwopt, 'XIX',    iform, 0, xix,    ' ')
      call tbpdsc(ltwopt, 'XIY',    iform, 0, xiy,    ' ')
      if (.not. inval) then
        call tbpdsc(ltwopt, 'ALFA',   iform, 0, alfa,   ' ')
        call tbpdsc(ltwopt, 'GAMTR',  iform, 0, gamtr,  ' ')
      endif
 
*---- Warn if machine is coupled.
      if (cplxy) then
        write (msg, 910) deltas
        call aawarn('TWOPGO', 2, msg)
      endif
      if (cplxt .or. dorad) then
        write (msg, 920)
        call aawarn('TWOPGO', 2, msg)
      endif
 
  910 format('OPTICS found transverse coupling for delta(p)/p =',f12.6/
     +       'results may be wrong.')
  920 format('OPTICS uses the RF system and synchrotron radiation ',
     +       'only to find the closed orbit.'/
     +       'for optical calculations it ignores both.')
 
      end
