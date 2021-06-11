      subroutine emevgo(savnam)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   EIGEN command: Track eigenvectors for periodic solution.           *
* Attributes:                                                          *
*   SAVNAM    (name)    SAVE option: Table name.                       *
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
      integer icode,ienum,iflag,iocc,ipos,j,jbit,jbyt
      double precision aival,amu,amuj,el,em,one,reval,sigma,tol,utwopi,
     +zero
      character*(mcnam) savnam
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0d0 / (2.0d0 * pi))
      parameter         (tol = 1.000001d0, zero = 0.0d0, one = 1.0d0)
 
      character*(mcnam) elmnam
      logical           fmap, fprt, fsav, m66sta
      dimension         em(6,6), reval(6), aival(6), sigma(6,6)
      dimension         amu(3)
 
*---- Get data for beam.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Find periodic solution.
      call tmturn(lcseq, deltas, error)
      if (error) go to 9999
      call ucopy(orbit0, orbit, 6*mwflt)
      amu(1) = 0.0
      amu(2) = 0.0
      amu(3) = 0.0
 
*---- Find eigenvectors at initial position.
      if (m66sta(rt)) then
        call laseig(rt, reval, aival, em)
        stabt = .false.
      else
        call ladeig(rt, reval, aival, em)
        stabt = reval(5)**2 + aival(5)**2 .le. tol  .and.
     +          reval(6)**2 + aival(6)**2 .le. tol
      endif
      if (error) go to 9999
      stabx = reval(1)**2 + aival(1)**2 .le. tol  .and.
     +        reval(2)**2 + aival(2)**2 .le. tol
      staby = reval(3)**2 + aival(3)**2 .le. tol  .and.
     +        reval(4)**2 + aival(4)**2 .le. tol
 
*---- Create internal table for lattice functions.
      ltwfun = 0
      if (savnam .ne. ' ') call emevsv(1, savnam, 0, em, amu)
      if (error) go to 9999
 
*---- Loop through element sequence.
      suml = 0.0
      fsav = .false.
      call emevpr('EIGEN', 0, ' ', 0, 0, em, amu)
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
            call m66mpy(re, em, em)
            do 20 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   20       continue
          endif
 
*---- Track through element.
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
            suml = suml + el
            call m66mpy(re, em, em)
            do 30 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   30       continue
          endif
 
*---- Misalignment at exit.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            call m66mpy(re, em, em)
            do 40 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   40       continue
          endif
 
*---- Print and save at exit.
          if (fprt) then
            call emevpr('EIGEN', icode, elmnam, ienum, iocc,  em, amu)
          endif
          if (fsav) then
            call emevsv(2, elmnam, ipos, em, amu)
          endif
 
*---- Entrance of line.
        else if (icode .eq. 2) then
 
*---- Output before entering.
          if (fprt) then
            call emevpr('EIGEN', icode, elmnam, 0, iocc,  em, amu)
          endif
          if (fsav) then
            call emevsv(2, elmnam, ipos, em, amu)
          endif
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            call m66mpy(re, em, em)
            do 50 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   50       continue
          endif
 
*---- Exit of line.
        else
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            call m66mpy(re, em, em)
            do 60 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   60       continue
          endif
 
*---- Output after exiting.
          if (fprt) then
            call emevpr('EIGEN', icode, elmnam, 0, iocc, em, amu)
          endif
          if (fsav) then
            call emevsv(2, elmnam, ipos, em, amu)
          endif
        endif
 
*---- Fill in SAVESIGA command, if any.
        if (jbit(iflag,msbet) .ne. 0) then
          call emce2i(em, ex, ey, et, sigma)
          call emssig(ipos, orbit, disp, sigma)
        endif
   90 continue
      call prline(iqpr2)
      qx = nsup * amu(1)
      qy = nsup * amu(2)
      if (stabt) then
        qs = nsup * abs(amu(3))
        write (iqpr2, 910) qx, qy, qs
      else
        write (iqpr2, 920) qx, qy
      endif
      call prline(iqpr2)
 
*---- Close lattice function table.
      if (ltwfun .ne. 0) call emevsv(3, savnam, 0, em, amu)
 
  910 format(' ',t61,'Q1 = ',f14.6,t87,'Q2 = ',f14.6,t113,'Q3 = ',f14.6)
  920 format(' ',t74,'Q1 = ',f14.6,t100,'Q2 = ',f14.6)
 
 9999 end
