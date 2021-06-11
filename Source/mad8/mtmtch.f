      subroutine mtmtch
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Main routine for insertion matching.                               *
* Attributes:                                                          *
*   BETX     (real)    Horizontal beta.                                *
*   ALFX     (real)    Horizontal alpha.                               *
*   MUX      (real)    Horizontal phase.                               *
*   BETY     (real)    Vertical beta.                                  *
*   ALFY     (real)    Vertical alpha.                                 *
*   MUY      (real)    Vertical phase.                                 *
*   X        (real)    Horizontal orbit.                               *
*   PX       (real)    Horizontal orbit slope.                         *
*   Y        (real)    Vertical orbit.                                 *
*   PY       (real)    Vertical orbit slope.                           *
*   T        (real)    Longitudinal orbit.                             *
*   PT       (real)    Momentum error.                                 *
*   DX       (real)    Horizontal dispersion.                          *
*   DPX      (real)    Horizontal dispersion slope.                    *
*   DY       (real)    Vertical dispersion.                            *
*   DPY      (real)    Vertical dispersion slope.                      *
*   WX       (real)    Horizontal chromatic amplitude.                 *
*   PHIX     (real)    Horizontal chromatic phase.                     *
*   DMUX     (real)    Horizontal chromatic derivative of MUX.         *
*   WY       (real)    Vertical chromatic amplitude.                   *
*   PHIY     (real)    Vertical chromatic phase.                       *
*   DMUY     (real)    Vertical chromatic derivative of MUY.           *
*   DDX      (real)    Horizontal 2nd dispersion.                      *
*   DDPX     (real)    Horizontal 2nd dispersion slope.                *
*   DDY      (real)    Vertical 2nd dispersion.                        *
*   DDPY     (real)    Vertical 2nd dispersion slope.                  *
*   ENER1     (real)                                                    *
*   CIRC     (real)                                                    *
*   I1       (real)                                                    *
*   I2       (real)                                                    *
*   I3       (real)                                                    *
*   I4       (real)                                                    *
*   I5       (real)                                                    *
*   I5I2     (real)                                                    *
*   I5I1     (real)                                                    *
*   DUMM     (real)                                                    *
*   sequence (2 names) match to these (1 or 2) sequences
*   BETA0    (name)    BETA0 module containing initial conditions.     *
*----------------------------------------------------------------------*
* Modified: 07-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Added energy attribute; noted additional chromatic attributes in   *
*   header comments; changed MAXVAL = 26 -> MAXVAL = 27                *
* Modified: 04-MAR-1999, T. Raubenheimer (SLAC)                        *
*   Added CIRC attribute; changed MAXVAL = 27 -> MAXVAL = 28           *
* Modified: 14-JUL-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 28 -> MAXVAL = 34; added /SYNCH/ common block     *
*   Changed MAXVAL = 34 -> MAXVAL = 36; T.R. ?                         *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      integer srange
      common / tmptmp/ srange(2,10)
 
      double precision deltap
      integer ibeta,icat,idata,ikey,ileng,iline,ipr,isp,mbeta,mdelta,
     +mline,morbit,msequ
      parameter         (mline  = maxval + 1, msequ = maxval + 2,
     +                   mbeta  = maxval + 4,
     +                   mdelta = maxval + 6, morbit = maxval + 7)
      logical           linflg
      integer i
 
*---- Is main beam line set?
      call lnchck('MTMTCH', error)
      if (error) go to 9999
 
*---- Set up for insertion matching.
      do i = 1, 2
        sequd(i) = ' '
      enddo
*---- Name of sequence(s).
      call utgnam(lccmd, msequ, msequ+1, sequd)
      if (sequd(2) .eq. ' ')  then
        mtdbfl = 1
      else
        mtdbfl = 2
      endif
      call mtinit
      imode = 2
      do i = 1, 2
        betnm(i) = ' '
      enddo
*---- Name of BETA0 module(s).
      call utgnam(lccmd, mbeta, mbeta+1, betnm)
      iline = mbat + (mline - 1) * mcsiz
      linflg = mod(iq(lccmd+iline+mctyp),10) .ne. 0
      do i = 1, mtdbfl
        bdtflg(i) = betnm(i) .ne. ' '
*---- Check consistency.
        if (bdtflg(i)) then
          call utleng(betnm(i), ileng)
          call difind(ldbnk, betnm(i)(1:ileng), ibeta, ldummy(i))
          if (ldummy(i) .eq. 0) then
            msg(1) = 'Initial BETA0 bank "' // betnm(i)(1:ileng)
     +      // '" not found.'
            call aafail('MTMTCH', 1, msg)
          else if (iq(ldummy(i)+mbpr).ne.mpenv
     +    .or. iq(ldummy(i)+mbsp).ne.2) then
            msg(1) = '"' // betnm(i)(1:ileng)
     +               // '" is not a "BETA0" bank.'
            call aafail('MTMTCH', 1, msg)
          else if (linflg) then
            call aafail('MTMTCH', 1,
     +      'Conflicting options BETA0 and LINE have been specified.')
          endif
        else
          ldummy(i) = 0
        endif
      enddo
*---- Line attribute.
      if (error) go to 9999
      if (linflg) call lnrefe(lccmd, mline, lmtlin, lmtlin, 1)
 
*---- Energy error.
      deltap = 0.0
      call utgflt(lccmd, mdelta, mdelta, deltap)
      florb = .false.
      call utglog(lccmd, morbit, morbit, florb)
 
*---- Build temporary BETA0 bank to hold initial conditions.
      call difind(ldkey, 'BETA0', ikey, lckey)
      ipr = iq(lckey+mbpr)
      isp = iq(lckey+mbsp)
      do i = 1, mtdbfl
        call aabook(lbeta0(i), 'BETA', ipr, isp, lckey, 1)
*---- If BETA0 given, use it, but overwrite with data from command bank.
        if (ldummy(i) .ne. 0) then
          idata = mbat
          do icat = 1, maxval
            if (mod(iq(lccmd+idata+mctyp),10) .ne. 0) then
              if (icat .gt. 6  .and.  icat .le. 12) florb = .true.
              call aacopy(lccmd, icat, lbeta0(i))
            else
              call aacopy(ldummy(i), icat, lbeta0(i))
            endif
            idata = idata + mcsiz
          enddo
*---- Otherwise copy command bank.
        else
          do icat = 1, maxval
            call aacopy(lccmd, icat, lbeta0(i))
          enddo
        endif
      enddo
*---- Initial output.
      if (error) go to 9999
      do i = 1, mtdbfl
        if (sequd(i) .ne. ' ' .and. sequd(i) .ne. sequnam)
     +  call get_active(sequd(i), 'MTMTCH')
        call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
        srange(1,i) = irg1
        srange(2,i) = irg2
        if (i .eq. 1) call prpage(iqpr2)
        call utleng(linnam, ileng)
        write (iqpr2, 910) linnam(1:ileng), rngnam, deltap
        call prline(iqpr2)
      enddo
      call aainfo('MTMTCH', 1, 'Begin insertion matching mode.')
 
*---- Make sure environment is set up.
      call enfix
 
  910 format(' Matching beam line "',a,'" as an insertion',t84,
     +       'Range:',a/' delta(p)/p = ',f12.6)
 
 9999 end
