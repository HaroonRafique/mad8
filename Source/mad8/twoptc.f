      subroutine twoptc
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   OPTICS command: Selective output of linear lattice parameters.     *
* Attributes, must be given in this order in the dictionary:           *
*   BETX      (real)    Horizontal beta.                        ( 1)   *
*   ALFX      (real)    Horizontal alfa.                        ( 2)   *
*   MUX       (real)    Horizontal phase.                       ( 3)   *
*   BETY      (real)    Vertical beta.                          ( 4)   *
*   ALFY      (real)    Vertical alfa.                          ( 5)   *
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
*   ENER1      (real)    Energy value                            (27)   *
*----------------------------------------------------------------------*
*   CENTRE    (logical) True, if values are desired at centres. (28)   *
*   LINE      (line)    Line for initial conditions.            (29)   *
*   BETA0     (name)    Bank for initial conditions.            (30)   *
*   FILENAME  (string)  File name for OPTICS results.           (31)   *
*   DELTAP    (real)    Momentum error (single value).          (32)   *
*   COLUMN(50)(name)    Selected table column names.            (33)   *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Add ENERGY attribute; change MAXTWS=26 to MAXTWS=27                *
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
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
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
      integer ibeta,ibetx,ibety,idum,ileng,iline,iunit,jtb,maxtws,mbeta,
     +mcent,mcols,mdelta,mfile,mline
      double precision deltap,rdum
 
      parameter         (maxtws = 27)
      logical           betflg, cent, eflag, linflg
      character *(mcnam) seqnm
      character*(mcnam) ctab(50), twsnam(2)
      character*(mcfil) strnam, filnam
      parameter         (mcent  = maxtws + 1, mline  = maxtws + 2,
     +                   mbeta  = maxtws + 3, mfile  = maxtws + 5,
     +                   mdelta = maxtws + 6, mcols  = maxtws + 7)
 
*==== PHASE 1: Check data.
*---- Check main beam line.
      call lnchck('OPTICS', error)
      if (error) go to 9999
 
*---- CENTRE flag.
      cent = .false.
      call utglog(lccmd, mcent, mcent, cent)
 
*---- Names for BETA0.
      twsnam(1) = ' '
      twsnam(2) = ' '
      call utgnam(lccmd, mbeta, mbeta+1, twsnam)
      betnam = twsnam(1)
      betflg = betnam .ne. ' '
      seqnm = twsnam(2)
      if (seqnm .ne. ' ')  call get_active(seqnm, 'TWOPTC')
 
*---- File for TFS output.
      strnam = 'optics'
      call utgstr(lccmd, mfile, mfile, strnam)
      optnam = 'OPTICS'
 
*---- Flag BETA0 and LINE attributes.
      iline = mbat + (mline - 1) * mcsiz
      linflg = mod(iq(lccmd+iline+mctyp),10) .ne. 0
      if (betflg) then
        call utleng(betnam, ileng)
        call difind(ldbnk, betnam(1:ileng), ibeta, ltwbet)
        if (ltwbet .eq. 0) then
          msg(1) = 'Initial BETA0 bank "' // betnam(1:ileng)
     +    // '" not found.'
          call aafail('TWOPTC', 1, msg)
        else if (iq(ltwbet+mbpr) .ne. mpenv  .or.
     +           iq(ltwbet+mbsp) .ne. 2) then
          msg(1) = '"' // betnam(1:ileng) // '" is not a "BETA0" bank.'
          call aafail('TWOPTC', 1, msg)
        else if (linflg) then
          call aafail('TWOPTC', 1,
     +    'Conflicting options BETA0 and LINE have been specified.')
        endif
      endif
 
*---- Momentum error value.
      deltap = 0.0
      call utgflt(lccmd, mdelta, mdelta, deltap)
      ndelta = 1
      chrom  = .false.
      couple = .false.
 
*---- Selected column names.
      do 30 jtb = 1, 50
        ctab(jtb) = ' '
   30 continue
      call utgnam(lccmd, mcols, mcols+49, ctab)
 
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
 
*---- Open table to save selected functions.
      call twopsv(1, idum, ctab, rdum)
 
*---- Set up RF system.
      call enfix
      call enfreq(deltap)
 
*==== PHASE 3: Perform computation for all momentum errors requested.
      if (error) go to 9999
      call uzero(betx0, 1, 30*mwflt)
      disp0(5) = 0.0
      disp0(6) = 1.0
 
*---- Initial values from command attributes.
      if (inval) then
        stabx = .true.
        staby = .true.
 
*---- LINE attribute.
        if (linflg) then
          call tmturn(ltwlin, deltas, eflag)
          if (eflag) go to 90
          call twbtin(ltwlin, .true., eflag)
          if (eflag) go to 90
 
*---- BETA0 attribute.
        else if (betflg) then
          call twfill(ltwbet)
        endif
        call tmscnd(lcseq)
        call tmderi(tt, disp0, rtp)
 
*---- Initial values from periodic solution.
      else
        call tmturn(lcseq, deltas, eflag)
        if (iq(lcseq+msym) .ne. 0) call tmmksm(.true.)
        call twbtin(lcseq, .true., eflag)
        if (eflag) go to 90
      endif
 
*---- Update initial values from command attributes.
      call twfill(lccmd)
 
*---- Track lattice functions and build table.
      call twopgo(cent, .true.)
 
*---- Drop LINE condition bank.
      if (ltwlin .ne. 0) call lndrop(ltwlin)
 
*---- Close table.
   90 continue
      call twopsv(4, idum, ctab, rdum)
 
*---- Write table on TFS file.
      call flopen(strnam, 'SWFD', 0, 0, iunit, eflag)
      if (.not. eflag) then
        call flname(iunit, filnam)
        call tbwtfs(optnam, iunit)
        call flclos(iunit, error)
        if (.not. error) then
        msg(1) = 'Lattice functions written on file: ' // filnam
        call aainfo('TWOPTC', 1, msg)
        endif
        call tbopen(optnam, 0, ltwopt)
        call tbdrop(ltwopt)
      endif
 
 9999 end
