      subroutine emengo(itype, savnam, signam, iline, tape, idisk)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track beam envelope.                                               *
* Input:                                                               *
*   ITYPE(3)  (integer) Flag array: SAVE, SIGMA0, LINE.                *
*   SAVNAM    (name)    Name for SAVE table.                           *
*   SIGNAM    (name)    Name for SIGMA0 option.                        *
*   ILINE     (integer) Number for LINE attribute.                     *
*   TAPE      (logical) True: Write tape file output on disk.          *
*   IDISK     (integer) Logical unit number for tape file output.      *
*----------------------------------------------------------------------*
* Modified: 18-MAR-1999, M. Woodley (SLAC)                             *
*   Add support for tape file output                                   *
* Modified: 01-APR-1999, M. Woodley (SLAC)                             *
*   Initialize ENER1 (in COMMON /OPTIC1/) using ENERGY from BEAM common *
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
      integer ibeta,icode,ienum,iflag,ileng,iline,iocc,ipos,j,jbit,jbyt
      double precision aival,corr,dismax,disrms,el,em,one,orbmax,orbrms,
     +pos,reval,sigma,sigmax,sigrms,temp,tol,zero
      integer           itype(4)
      character*(mcnam) savnam, signam
      logical tape
      integer idisk
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      parameter         (tol = 1.000001d0, zero = 0.0d0, one = 1.0d0)
 
      character*(mcnam) elmnam
      logical           fmap, fprt, fsav, m66sta
      dimension         em(6,6), sigma(6,6), reval(6), aival(6)
      dimension         corr(6,6), temp(6,6)
      dimension         dismax(6), disrms(6)
      dimension         orbmax(6), orbrms(6), sigmax(6), sigrms(6)
 
*---- Get data for beam.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Deal with SIGMA0; First check consistency.
      if (signam .ne. ' ') then
        call utleng(signam, ileng)
        call difind(ldbnk, signam(1:ileng), ibeta, ltwbet)
        if (ltwbet .eq. 0) then
          msg(1) = 'Initial SIGMA0 bank "' // signam(1:ileng)
     +    // '" not found.'
          call aafail('EMENGO', 1, msg)
          go to 9999
        else if (itype(iline) .ne. 0) then
          call aafail('EMENGO', 1,
     +    'Conflicting options SIGMA0 and LINE have been specified.')
          go to 9999
        endif
 
*---- Fetch SIGMA0 values.
        call uzero(orbit, 1, 6*mwflt)
        call uzero(disp,  1, 6*mwflt)
        call uzero(corr,  1, 36*mwflt)
        call utgflt(ltwbet,  1,  6, orbit)
        call utgflt(ltwbet,  7, 12, disp)
        call utgflt(ltwbet, 13, 18, corr(1,1))
        call utgflt(ltwbet, 19, 23, corr(2,2))
        call utgflt(ltwbet, 24, 27, corr(3,3))
        call utgflt(ltwbet, 28, 30, corr(4,4))
        call utgflt(ltwbet, 31, 32, corr(5,5))
        call utgflt(ltwbet, 33, 33, corr(6,6))
        call emct2i(corr, sigma)
        stabx = .true.
        staby = .true.
        stabt = .true.
 
*---- No SIGMA0 seen; maybe use LINE attribute.
      else
        if (itype(iline) .ne. 0) then
          call lnrefe(lccmd, iline, ltwlin, lroot, -minit)
          if (error) go to 9999
          call tmturn(ltwlin, deltas, error)
        else
          call tmturn(lcseq, deltas, error)
        endif
        if (error) go to 9999
        call ucopy(orbit0, orbit, 6*mwflt)
 
*---- Find eigenvectors at initial position.
        if (m66sta(rt)) then
          call twdisp(rt, rt(1,6), disp)
          disp(5) = 0.0
          disp(6) = 1.0
          call laseig(rt, reval, aival, em)
          stabt = .false.
        else
          call uzero(disp, 1, 6*mwflt)
          call ladeig(rt, reval, aival, em)
          if (error) go to 9999
          stabt = reval(5)**2 + aival(5)**2 .le. tol  .and.
     +            reval(6)**2 + aival(6)**2 .le. tol
        endif
        if (error) go to 9999
        stabx = reval(1)**2 + aival(1)**2 .le. tol  .and.
     +          reval(2)**2 + aival(2)**2 .le. tol
        staby = reval(3)**2 + aival(3)**2 .le. tol  .and.
     +          reval(4)**2 + aival(4)**2 .le. tol
        call emce2i(em, ex, ey, et, sigma)
      endif
 
*---- Create internal table for lattice functions.
      ltwfun = 0
      if (savnam .ne. ' ') call emensv(1, savnam, 0, sigma)
      if (error) go to 9999
      do 40 j = 1, 6
        dismax(j) = abs(disp(j))
        disrms(j) = disp(j)**2
        orbmax(j) = abs(orbit(j))
        orbrms(j) = orbit(j)**2
        sigmax(j) = sqrt(sigma(j,j))
        sigrms(j) = sigma(j,j)
   40 continue
      pos = one
 
*---- Initialize beam energy value ENER1 ... use ENERGY value from BEAM
*     common since ENVELOPE command doesn't have an explicit energy
*     attribute.
      ener1 = en0
*---- Loop through element sequence.
      suml = 0.0
      fsav = .false.
      call emenpr('ENVELOPE', 0, ' ', 0, 0, sigma)
      if (tape) call ementp(1, ' ', idisk, sigma, suml)
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        fprt = jbit(iflag,mprnt) .ne. 0  .or.
     +         ipos .eq. irg1  .or.  ipos .eq. irg2
        if (ltwfun .ne. 0) then
          fsav = jbit(iflag,moptc) .ne. 0
        endif
        icode = jbyt(iflag, 1, mcode)
*---- Physical element.
        if (icode .eq. 1) then
 
*---- Misalignment at entrance.
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, sigma, temp)
            call m66mtr(temp, re, sigma)
          endif
 
*---- Track through element.
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
            if (.not. stabt) call m66byv(re, disp, disp)
            suml = suml + el
            call m66mpy(re, sigma, temp)
            call m66mtr(temp, re, sigma)
 
*---- Compute maximum extents.
            do 80 j = 1, 6
              dismax(j) = max(abs(disp(j)),dismax(j))
              disrms(j) = disrms(j) + disp(j)**2
              orbmax(j) = max(abs(orbit(j)),orbmax(j))
              orbrms(j) = orbrms(j) + orbit(j)**2
              sigmax(j) = max(sqrt(sigma(j,j)),sigmax(j))
              sigrms(j) = sigrms(j) + sigma(j,j)
   80       continue
            pos  = pos  + one
          endif
 
*---- Misalignment at exit.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, sigma, temp)
            call m66mtr(temp, re, sigma)
          endif
 
*---- Print and save at exit.
          if (fprt) then
            call emenpr('ENVELOPE', icode, elmnam, ienum, iocc, sigma)
          endif
          if (fsav) then
            call emensv(2, elmnam, ipos, sigma)
          endif
          if (tape) call ementp(2, elmnam, idisk, sigma, suml)
 
*---- Entrance of line.
        else if (icode .eq. 2) then
 
*---- Output before entering.
          if (fprt) then
            call emenpr('ENVELOPE', icode, elmnam, 0, iocc, sigma)
          endif
          if (fsav) then
            call emensv(2, elmnam, ipos, sigma)
          endif
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, sigma, temp)
            call m66mtr(temp, re, sigma)
          endif
 
*---- Exit of line.
        else
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, sigma, temp)
            call m66mtr(temp, re, sigma)
          endif
 
*---- Output after exiting.
          if (fprt) then
            call emenpr('ENVELOPE', icode, elmnam, 0, iocc, sigma)
          endif
          if (fsav) then
            call emensv(2, elmnam, ipos, sigma)
          endif
        endif
 
*---- Fill in SAVESIGA command, if any.
        if (jbit(iflag,msbet) .ne. 0) then
          call emssig(ipos, orbit, disp, sigma)
        endif
   90 continue
 
*---- Summary.
      call prline(iqpr2)
      if (stabt) then
        do 210 j = 1, 6
          orbmax(j) = 1000.0 * orbmax(j)
          orbrms(j) = 1000.0 * sqrt(orbrms(j)/pos)
          sigmax(j) = 1000.0 * sigmax(j)
          sigrms(j) = 1000.0 * sqrt(sigrms(j)/pos)
  210   continue
        write (iqpr2, 910) orbmax, orbrms, sigmax, sigrms
      else
        do 220 j = 1, 4
          disrms(j) = sqrt(disrms(j)/pos)
          orbmax(j) = 1000.0 * orbmax(j)
          orbrms(j) = 1000.0 * sqrt(orbrms(j)/pos)
          sigmax(j) = 1000.0 * sigmax(j)
          sigrms(j) = 1000.0 * sqrt(sigrms(j)/pos)
  220   continue
        write (iqpr2, 920) (dismax(j), j=1,4), (disrms(j), j=1,4),
     +                     (orbmax(j), j=1,4), (orbrms(j), j=1,4),
     +                     (sigmax(j), j=1,4), (sigrms(j), j=1,4)
      endif
      call prline(iqpr2)
      if (tape) call ementp(3, ' ', idisk, sigma, suml)
 
*---- Drop LINE condition bank.
      if (ltwlin .ne. 0) call lndrop(ltwlin)
 
*---- Close lattice function table.
      if (ltwfun .ne. 0) call emensv(3, savnam, 0, sigma)
 
  910 format(t46,'x [mm]',t59,'px [mrad]',t78,'y [mm]',t91,'py [mrad]',
     +       t110,'t [mm]',t123,'pt [mrad]'/
     +       ' orbit',t13,'(abs. max.)',t36,6f16.6/
     +       t13,'(r.m.s.)',t36,6f16.6/
     +       ' sigma',t13,'(abs. max.)',t36,6f16.6/
     +       t13,'(r.m.s.)',t36,6f16.6)
  920 format(t47,'x [m]',t60,'px [rad]',t79,'y [m]',t92,'py [rad]'/
     +       ' dispersion',t13,'(abs. max.)',t36,4f16.6/
     +       t13,'(r.m.s.)',t36,4f16.6/
     +       t46,'x [mm]',t59,'px [mrad]',t78,'y [mm]',t91,'py [mrad]'/
     +       ' orbit',t13,'(abs. max.)',t36,4f16.6/
     +       t13,'(r.m.s.)',t36,4f16.6/
     +       ' sigma',t13,'(abs. max.)',t36,4f16.6/
     +       t13,'(r.m.s.)',t36,4f16.6)
 
 9999 end
