      subroutine twiss
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TWISS command: Track linear lattice parameters.                    *
* Attributes, must be given in this order in the dictionary:           *
*   BETX      (real)    Horizontal beta.                        ( 1)   *
*   ALFX      (real)    Horizontal alpha.                       ( 2)   *
*   MUX       (real)    Horizontal phase.                       ( 3)   *
*   BETY      (real)    Vertical beta.                          ( 4)   *
*   ALFY      (real)    Vertical alpha.                         ( 5)   *
*   MUY       (real)    Vertical phase.                         ( 6)   *
*   X         (real)    Horizontal position for closed orbit.   ( 7)   *
*   PX        (real)    Horizontal momentum for closed orbit.   ( 8)   *
*   Y         (real)    Vertical position for closed orbit.     ( 9)   *
*   PY        (real)    Vertical momentum for closed orbit.     (10)   *
*   T         (real)    Longitudinal position for closed orbit. (11)   *
*   PT        (real)    Momentum error for closed orbit.        (12)   *
*   DX        (real)    Horizontal dispersion.                  (13)   *
*   DPX       (real)    Disperstion for horizontal momentum.    (14)   *
*   DY        (real)    Vertical dispersion.                    (15)   *
*   DPY       (real)    Dispersion for vertical momentum.       (16)   *
*----------------------------------------------------------------------*
*   WX        (real)    Horizontal chromatic amplitude.         (17)   *
*   PHIX      (real)    Horizontal chromatic phase.             (18)   *
*   DMUX      (real)    Chromatic derivative of hor. phase.     (19)   *
*   WY        (real)    Vertical chromatic amplitude.           (20)   *
*   PHIY      (real)    Vertical chromatic phase.               (21)   *
*   DMUY      (real)    Chromatic derivative of vert. phase.    (22)   *
*   DDX       (real)    Second-order horizontal dispersion.     (23)   *
*   DDPX      (real)    Second-order dispersion for hor. mom.   (24)   *
*   DDY       (real)    Second-order vertical dispersion.       (25)   *
*   DDPY      (real)    Second-order dispersion for vert. mom.  (26)   *
*----------------------------------------------------------------------*
*   ENERGY    (real)    Energy value                            (27)   *
*----------------------------------------------------------------------*
*   CHROM     (logical) CHROM option.                           (28)   *
*   COUPLE    (logical) COUPLE option.                          (29)   *
*----------------------------------------------------------------------*
*   TAPE      (string)  TAPE option: File name.                 (30)   *
*   SAVE      (name)    SAVE option: Table name.                (31)   *
*   TUNES     (name)    TUNES option: Table name.               (32)   *
*   BETA0     (name)    Bank for initial conditions.            (33)   *
*   LINE      (line)    Line for initial conditions.            (34)   *
*   DELTAP(25)(real)    Momentum error values.                  (35)   *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Add ENERGY attribute; change MAXTWS=26 to MAXTWS=27                *
* Modified: 02-APR-1999, M. Woodley (SLAC)                             *
*   Set ENER1 (in COMMON /OPTIC1/) before call to TMSCND if there are   *
*   LCAVITY elements present in the current beamline; reset data in    *
*   BEAM common via a call to ENGET after call to TMSCND.  If initial  *
*   conditions come from a BETA0, update initial values from command   *
*   attributes via a second call to TWFILL                             *
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
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
 
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
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
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
      integer ndelta
 
*---- Common for Twiss module.
      common /twchar/   funnam, optnam, sumnam, betnam
      common /twdata/   ndelta, chrom, couple
      save              /twdata/, /twchar/
      character*(mcnam) funnam, optnam, sumnam, betnam
      logical           chrom, couple
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer ibeta,ibetx,ibety,idata,idelta,idisk,ileng,iline,jdelta,
     +maxdel,maxnam,maxtws,mdelta,mflag,mline,mname,mtape
      double precision dval, zero, ener_st
 
      parameter         (maxtws=27)
      parameter (zero = 0.d0)
      parameter         (maxnam=4, maxdel=25)
      character*(mcfil) strnam, filnam
      character*(mcnam) seqnm, twsnam(maxnam)
      dimension         dval(maxdel)
      logical           betflg, eflag, linflg, tape, twsflg(2)
      parameter         (mflag = maxtws + 1, mtape = mflag + 2,
     +                   mname = mtape + 1, mline = mname + maxnam,
     +                   mdelta = mline  + 1)
 
      logical           anylcav
 
*==== PHASE 1: Check data.
*---- Check main beam line.
      call lnchck('TWISS ', error)
      if (error) go to 9999
 
*---- Option flags.
      twsflg(1) = .false.
      twsflg(2) = .false.
      call utglog(lccmd, maxtws + 1, maxtws + 2, twsflg)
      chrom  = twsflg(1)
      couple = twsflg(2)
 
*---- File name for TAPE option.
      strnam = ' '
      call utgstr(lccmd, mtape, mtape, strnam)
      tape = strnam .ne. ' '
 
*---- BETA0 and table names.
      twsnam(1) = ' '
      twsnam(2) = ' '
      twsnam(3) = ' '
      twsnam(4) = ' '
      call utgnam(lccmd, mname, mname + maxnam - 1, twsnam)
      funnam = twsnam(1)
      sumnam = twsnam(2)
      betnam = twsnam(3)
      seqnm = twsnam(4)
      betflg = betnam .ne. ' '
      if (seqnm .ne. ' ')  call get_active(seqnm, 'TWISS')
*---- LINE attribute.
      iline = mbat + (mline - 1) * mcsiz
      linflg = mod(iq(lccmd+iline+mctyp),10) .ne. 0
      if (betflg) then
        call utleng(betnam, ileng)
        call difind(ldbnk, betnam(1:ileng), ibeta, ltwbet)
        if (ltwbet .eq. 0) then
          msg(1) = 'Initial BETA0 bank "' // betnam(1:ileng)
     +    // '" not found.'
          call aafail('TWISS', 1, msg)
        else if (iq(ltwbet+mbpr) .ne. mpenv  .or.
     +           iq(ltwbet+mbsp) .ne. 2) then
          msg(1) = '"' // betnam(1:ileng) // '" is not a "BETA0" bank.'
          call aafail('TWISS ', 1, msg)
        else if (linflg) then
          call aafail('TWISS', 1,
     +    'Conflicting options BETA0 and LINE have been specified.')
        endif
      endif
 
*---- Momentum error values.
      ndelta = 0
      idata = mbat + mcsiz * (mdelta - 1)
      do 30 jdelta = 1, maxdel
        if (mod(iq(lccmd+idata+mctyp),10) .ne. 0) then
          ndelta = ndelta + 1
          call ucopy(q(lccmd+idata+mcval), dval(ndelta), mwflt)
        endif
        idata = idata + mcsiz
   30 continue
*---- If no value given, default is zero.
      if (ndelta .eq. 0) then
        dval(1) = 0.0
        ndelta = 1
      endif
*==== PHASE 2: Initialize flags, tables, and files.
      if (error) go to 9999
 
*---- Prepare sequence for LINE option.
      if (linflg) then
        call lnrefe(lccmd, mline, ltwlin, lroot, -minit)
      endif
 
*---- Initial value flag.
      ibetx = mbat
      ibety = mbat + 3 * mcsiz
      inval = (mod(iq(lccmd+ibetx+mctyp),10) .ne. 0  .and.
     +         mod(iq(lccmd+ibety+mctyp),10) .ne. 0)  .or.
     +        betflg  .or.  linflg
 
*---- Open file for "TAPE" option.
      if (tape) then
        call flopen(strnam, 'SWFD', 0, 0, idisk, eflag)
        tape = .not. eflag
      endif
 
*---- Create internal table for lattice functions.
      ltwfun = 0
      if (funnam .ne. ' ') then
        call twbtsv(1, 0)
      endif
 
*---- Create internal table for summary data.
      ltwsum = 0
      if (sumnam .ne. ' ') call twsmsv(1, 0)
 
*---- Set up RF system.
      call enfix
 
*==== PHASE 3: Perform computation for all momentum errors requested.
      if (error) go to 9999
      do 90 idelta = 1, ndelta
        eflag = .false.
        if (ltwfun .ne. 0) then
          call tbseg(ltwfun, idelta, eflag)
          if (eflag) then
            stabx = .false.
            staby = .false.
*            if (ltwfun .ne. 0) call mzdrop(0, ltwfun, '.')
*            if (ltwsum .ne. 0) call mzdrop(0, ltwsum, '.')
            goto 80
          endif
        endif
        call uzero(betx0, 1, 30*mwflt)
        call uzero(qx, 1, 19*mwflt)
        disp0(5) = 0.0
        disp0(6) = 1.0
        deltas = dval(idelta)
        call enfreq(deltas)
 
        if (anylcav()) then
          ener1 = ener0
          if (ener1 .eq. zero .or. .not. ereset) ener1 = en0
        endif
        ener_st = en0
*---- Initial values from command attributes.
        if (inval) then
          stabx = .true.
          staby = .true.
 
*---- LINE attribute.
          if (linflg) then
            call tmturn(ltwlin, deltas, eflag)
            if (eflag) go to 80
            if (couple) then
              call twcpin(ltwlin, eflag)
              call tmderi(tt, disp0, rtp)
            else
              call twbtin(ltwlin, .true., eflag)
            endif
            if (eflag) go to 80
 
*---- BETA0 attribute.
          else if (betflg) then
            call twfill(ltwbet)
*--- override BETA0 stuff if necessary
            call twfill(lccmd)
          endif
          if (anylcav()) then
            call tmscnd(lcseq)
            call enget
          else
            call tmscnd(lcseq)
          endif
          call tmderi(tt, disp0, rtp)
 
*---- Initial values from periodic solution.
        else
          call tmturn(lcseq, deltas, eflag)
          if (iq(lcseq+msym) .ne. 0) call tmmksm(.true.)
          if (couple) then
            call twcpin(lcseq, eflag)
            call tmderi(tt, disp0, rtp)
          else
            call twbtin(lcseq, .true., eflag)
          endif
          if (eflag) go to 80
        endif
 
*---- Update initial values from command attributes.
        call twfill(lccmd)
 
*---- Initial values of BETX and BETY must not be zero.
        if (betx0 .eq. 0.0  .or. bety0 .eq. 0.0) then
          call aafail('TWISS', 1,
     +      'Both initial BETX and BETY should be non-zero.')
          go to 80
        endif
        en0 = ener_st
*---- Build table of lattice functions, coupled.
        if (couple) then
          call twcpgo(.true., tape, idisk)
 
*---- Build table of lattice functions, uncoupled.
        else
          call twbtgo(.true., tape, idisk)
 
*---- List chromatic functions.
          if (chrom) then
            call twchgo(.true., tape, idisk)
          endif
        endif
 
*---- Build global values table.
        if (ltwsum .ne. 0) call twsmsv(2, idelta)
   80   continue
   90 continue
 
*---- Reset delta(p)/p to zero.
      deltas = 0.0
      call enfreq(deltas)
 
*---- Close disk file.
      if (tape) then
        call flclos(idisk, error)
        if (.not. error) then
          call flname(idisk, filnam)
          msg(1) = 'Lattice functions written on file: ' // filnam
          call aainfo('TWISS', 1, msg)
        endif
      endif
*---- Drop LINE condition bank.
      if (ltwlin .ne. 0) call lndrop(ltwlin)
 
*---- Close lattice function table.
      if (ltwfun .ne. 0) call twbtsv(4, 0)
 
*---- Close global values table.
      if (ltwsum .ne. 0) call twsmsv(4, 0)
 9999 end
