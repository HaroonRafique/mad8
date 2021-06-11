      subroutine mtpini(iflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initial conditions for penalty function computation.               *
* Output:                                                              *
*   IFLAG     (integer) Stability flag.                                *
*----------------------------------------------------------------------*
* Modified: 07-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 26 -> MAXVAL = 27; added energy constraint to     *
*   CMIN; added energy constraint for lines; copied ENER0 data to ENER1 *
*   at end                                                             *
* Modified: 04-MAR-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 27 -> MAXVAL = 28                                 *
* Modified: 14-JUL-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 28 -> MAXVAL = 34; added /SYNCH/ common block;    *
*   zero all synchrotron integrals                                     *
* Modified: 09-SEP-1999, M. Woodley (SLAC)                             *
*   Enforce consistency between initial energy values in /BEAFLT/,     *
*   /OPTIC0/, and /OPTIC1/ common blocks                               *
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
      integer maxlin,maxval,mconf1,mconf2,mconmn,mconmx,mcontp,mconvl,
     +mconwt
 
*---- Parameters for matching module.
      parameter         (maxlin = 16, maxval = 36)
      parameter         (mconf1 = 1, mcontp = 2, mconf2 = maxval + 2)
      parameter         (mconmn = mconf2 + 1)
      parameter         (mconmx = mconmn + maxval * mwflt)
      parameter         (mconvl = mconmx + maxval * mwflt)
      parameter         (mconwt = mconvl + maxval * mwflt)
      integer energy_val, chrom_val
      parameter         (energy_val = 27, chrom_val = 26)
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
      integer icc
      double precision cmax,cmin,cval,cwgt
 
*---- Working area for a single matching constraint.
      common /mtccon/   icc(maxval), cmin(maxval), cmax(maxval),
     +                  cwgt(maxval), cval(maxval)
      save   /mtccon/
 
*---- Flags for matching.
      common /mtcflg/   flbeta, florb, flrmat, fltmat, flchrm
      save              /mtcflg/
      logical           flbeta, florb, flrmat, fltmat, flchrm
      character *(mcnam)  sequd, betnm
      common / dmatchc / sequd(2), betnm(2)
      integer mtdbfl, imsequ
      common / dmatchi / mtdbfl, imsequ
      logical bdtflg
      common / dmatchl / bdtflg(2)
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
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
*---- Synchrotron integrals, etc.
      common /synch/    synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision  synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      integer i,iflag
      double precision zero, one, twopi
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (zero = 0.d0, one = 1.d0, twopi  = 2.0d0 * pi)
      logical           eflag
 
*---- Set flags to use ideal orbit for calculation,
*     if ORBIT flag was not set.
      if (.not. florb) then
        do 5 i = 1, maxdof
          doflag(i) = .false.
    5   continue
      endif
 
*---- Fill in constraints.
      iflag = 1
      eflag = .false.
      lcon = lmcon
  100 if (lcon .ne. 0) then
 
*---- CONSTRAINT with LINE option?
*     link 1 of constraint bank is structural link to sequence bank.
        lmtseq = lq(lcon-1)
        if (lmtseq .ne. 0) then
          if (florb) then
            call tmclor(lmtseq, deltas, .false., error)
            if (error) go to 800
          else
            call tmrefe(lmtseq)
            call uzero(orbit0, 1, 6*mwflt)
          endif
          if (flchrm) then
            call tmscnd(lmtseq)
            call twbtin(lmtseq, .true., eflag)
            iflag = 0
            if (eflag) iflag = 1
          else
            call mtbtin(iflag)
          endif
          if (iflag .ne. 0) go to 9999
          call ucopy(q(lcon+mconmn), cmin, maxval*mwflt)
          cmin(1) = betx0
          cmin(2) = alfx0
          cmin(4) = bety0
          cmin(5) = alfy0
          call ucopy(orbit0, cmin(7), 6*mwflt)
          call ucopy(disp0, cmin(13), 4*mwflt)
          if (flchrm) then
            cmin(14) = wx0
            cmin(15) = phix0
            cmin(16) = dmux0 * twopi
            cmin(17) = wy0
            cmin(18) = phiy0
            cmin(19) = dmuy0 * twopi
            call ucopy(disp, cmin(23), 4*mwflt)
          endif
          cmin(27) = ener0
          cmin(28) = circ
          cmin(29) = zero
          cmin(30) = zero
          cmin(31) = zero
          cmin(32) = zero
          cmin(33) = zero
          cmin(34) = zero
          cmin(35) = zero
          cmin(36) = zero
          call ucopy(cmin, q(lcon+mconmn), maxval*mwflt)
        endif
        lcon = lq(lcon)
        go to 100
      endif
 
*---- Initial values for beam line.
      r0mat(1,1) = zero
      r0mat(1,2) = zero
      r0mat(2,1) = zero
      r0mat(2,2) = zero
*---- IMODE=1 -> CELL command; IMODE=2 -> MATCH command
      if (imode .eq. 2) then
 
*---- "LINE =..." initial condition.
        if (lmtlin .ne. 0) then
          if (florb) then
            call tmclor(lmtlin, deltas, .false., error)
            if (error) go to 800
          else
            call tmrefe(lmtlin)
            call uzero(orbit0, 1, 6*mwflt)
          endif
          if (flchrm) then
            call tmscnd(lmtlin)
            call twbtin(lmtlin, .true., eflag)
            iflag = 0
            if (eflag) iflag = 1
          else
            call mtbtin(iflag)
          endif
          if (iflag .ne. 0) go to 9999
 
*---- Otherwise set up defaults.
        else
          call uzero(betx0, 1, 36*mwflt)
          betx0 = one
          bety0 = one
          disp0(6) = one
        endif
 
*---- Overwrite with given values.
        call utgflt(lmtbet,  1,  6, betx0)
        amux0 = amux0 * twopi
        amuy0 = amuy0 * twopi
        call utgflt(lmtbet,  7, 12, orbit0)
        call utgflt(lmtbet, 13, 16, disp0)
        if (flchrm) then
          call utgflt(lmtbet, 17, 22, wx0)
          dmux0 = dmux0 * twopi
          dmuy0 = dmuy0 * twopi
          call utgflt(lmtbet, 23, 26, ddisp0)
        endif
        call utgflt(lmtbet, 27, 27, ener0)
        call utgflt(lmtbet, 28, 28, circ)
        iflag = 0
        if (betx0 .eq. zero  .or.  bety0 .eq. zero) iflag = 1
*---- Set up the initial energy value.
*     NOTE: if the initial energy value is not specified (either
*           explicitly by command attribute or implicitly via a
*           "BETA0="), reset the BEAM bank energy and use it; if the
*           initial energy value has been specified, set the BEAM bank
*           energy equal to it and recompute the relativistic
*           quantities
        if (ener0 .eq. zero) then
          call enget
          ener0 = en0
        else
          en0 = ener0
          pc = sqrt(en0**2 - amass**2)
          gamma = en0 / amass
          beta = pc / en0
        endif
 
*---- DELTAP command attribute doesn't seem to be used ... see MTMTCH
        deltas = zero
        gammas = gamma
        betas = beta
*---- Periodic initial conditions.
      else
        if (florb) then
          call tmclor(lcseq, deltas, .false., error)
          if (error) go to 800
        else
          call tmrefe(lcseq)
          call uzero(orbit0, 1, 6*mwflt)
        endif
        if (iq(lcseq+msym) .ne. 0) call tmmksm(.false.)
        if (flchrm) then
          call tmscnd(lcseq)
          call twbtin(lcseq, .true., eflag)
          iflag = 0
          if (eflag) iflag = 1
        else
          call mtbtin(iflag)
        endif
        if (iflag .ne. 0) go to 9999
      endif
 
*---- Copy to current values.
      call ucopy(betx0, betx, 18*mwflt)
      if (flchrm) call ucopy(wx0, wx, 12*mwflt)
      suml = circ
      ener1 = ener0
      if (ener1 .eq. zero .or. .not. ereset) ener1 = en0
*---- initialize synchrotron integrals to zero
      synch_1 = zero
      synch_2 = zero
      synch_3 = zero
      synch_4 = zero
      synch_5 = zero
      rmat(1,1) = r0mat(1,1)
      rmat(1,2) = r0mat(1,2)
      rmat(2,1) = r0mat(2,1)
      rmat(2,2) = r0mat(2,2)
 
*---- Failure to find closed orbit.
  800 error = .false.
 
 9999 end
