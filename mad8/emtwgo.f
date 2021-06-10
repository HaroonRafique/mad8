      subroutine emtwgo(itype, savnam, betnam, iline)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TWISS3 command: Track Mais-Ripken lattice functions.               *
* Attributes:                                                          *
*   ITYPE(3)  (integer) Flag array: SAVE, BETA0, LINE.                 *
*   SAVNAM    (name)    SAVE option: Table name.                       *
*   BETNAM    (name)    Initial values for functions (ignored).        *
*   ILINE     (integer) Number for LINE attribute.                     *
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
      integer icode,ienum,iflag,iline,iocc,ipos,j,j1,j2,jbit,jbyt,k,k1,
     +k2
      double precision aival,amu,amuj,bmax,bx,dismax,disrms,el,em,gmax,
     +gx,one,orbmax,orbrms,pos,reval,sigma,tol,utwopi
      integer           itype(3)
      character*(mcnam) savnam, betnam
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
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0d0 / (2.0d0 * pi))
      parameter         (tol = 1.000001d0, one = 1.0d0)
 
      character*(mcnam) elmnam
      logical           fmap, fprt, fsav, m66sta
      dimension         em(6,6), reval(6), aival(6), sigma(6,6)
      dimension         bmax(3,3), gmax(3,3), amu(3)
      dimension         orbmax(6), orbrms(6), dismax(6), disrms(6)
 
*---- Get data for beam.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- LINE attribute.
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
        stabt = reval(5)**2 + aival(5)**2 .le. tol  .and.
     +          reval(6)**2 + aival(6)**2 .le. tol
      endif
      if (error) go to 9999
      stabx = reval(1)**2 + aival(1)**2 .le. tol  .and.
     +        reval(2)**2 + aival(2)**2 .le. tol
      staby = reval(3)**2 + aival(3)**2 .le. tol  .and.
     +        reval(4)**2 + aival(4)**2 .le. tol
 
*---- Maximum extents.
      do 30 j = 1, 3
        j1 = 2 * j -1
        j2 = 2 * j
        do 20 k = 1, 3
          k1 = 2 * k - 1
          k2 = 2 * k
          bmax(j,k) = em(j1,k1) * em(j1,k1) + em(j1,k2) * em(j1,k2)
          gmax(j,k) = em(j2,k1) * em(j2,k1) + em(j2,k2) * em(j2,k2)
   20   continue
        amu(j) = 0.0
   30 continue
      do 40 j = 1, 6
        orbmax(j) = abs(orbit(j))
        orbrms(j) = orbit(j)**2
        dismax(j) = abs(disp(j))
        disrms(j) = disp(j)**2
   40 continue
      pos   = one
 
*---- Create internal table for lattice functions.
      ltwfun = 0
      if (savnam .ne. ' ') call emtwsv(1, savnam, 0, em, amu)
      if (error) go to 9999
 
*---- Loop through element sequence.
      suml  = 0.0
      fsav = .false.
      call emtwpr('TWISS3', 0, ' ', 0, 0, em, amu)
      do 190 ipos = irg1, irg2
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
            call m66mpy(re, em, em)
            do 50 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   50       continue
          endif
 
*---- Track through element.
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
            if (.not. stabt) then
              call m66byv(re, disp, disp)
            endif
            suml = suml + el
            call m66mpy(re, em, em)
 
*---- Compute maximum extents and phases.
            do 70 j = 1, 3
              j1 = 2 * j -1
              j2 = 2 * j
              do 60 k = 1, 3
                k1 = 2 * k - 1
                k2 = 2 * k
                bx = em(j1,k1)*em(j1,k1) + em(j1,k2)*em(j1,k2)
                bmax(j,k) = max(bmax(j,k),bx)
                gx = em(j2,k1)*em(j2,k1) + em(j2,k2)*em(j2,k2)
                gmax(j,k) = max(gmax(j,k),gx)
   60         continue
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   70       continue
            do 80 j = 1, 6
              orbmax(j) = max(abs(orbit(j)),orbmax(j))
              orbrms(j) = orbrms(j) + orbit(j)**2
              dismax(j) = max(abs(disp(j)),dismax(j))
              disrms(j) = disrms(j) + disp(j)**2
   80       continue
            pos  = pos  + one
          endif
 
*---- Misalignment at exit.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, em, em)
            do 90 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
   90       continue
          endif
 
*---- Print and save at exit.
          if (fprt) then
            call emtwpr('TWISS3', icode, elmnam, ienum, iocc, em, amu)
          endif
          if (fsav) then
            call emtwsv(2, elmnam, ipos, em, amu)
          endif
 
*---- Entrance of line.
        else if (icode .eq. 2) then
 
*---- Output before entering.
          if (fprt) then
            call emtwpr('TWISS3', icode, elmnam, 0, iocc, em, amu)
          endif
          if (fsav) then
            call emtwsv(2, elmnam, ipos, em, amu)
          endif
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, em, em)
            do 100 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
  100       continue
          endif
 
*---- Exit of line.
        else
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, em, em)
            do 110 j = 1, 3
              amuj = atan2(em(2*j-1,2*j),em(2*j-1,2*j-1)) * utwopi
              amu(j) = amuj + anint(amu(j)-amuj)
  110       continue
          endif
 
*---- Output after exiting.
          if (fprt) then
            call emtwpr('TWISS3', icode, elmnam, 0, iocc, em, amu)
          endif
          if (fsav) then
            call emtwsv(2, elmnam, ipos, em, amu)
          endif
        endif
 
*---- Fill in SAVESIGA command, if any.
        if (jbit(iflag,msbet) .ne. 0) then
          call emce2i(em, ex, ey, et, sigma)
          call emssig(ipos, orbit, disp, sigma)
        endif
  190 continue
 
*---- Summary.
      call prline(iqpr2)
      qx = nsup * amu(1)
      qy = nsup * amu(2)
      if (stabt) then
        qs = nsup * abs(amu(3))
        do 210 j = 1, 6
          orbmax(j) = 1000.0 * orbmax(j)
          orbrms(j) = 1000.0 * sqrt(orbrms(j)/pos)
  210   continue
        write (iqpr2, 910)
     +    qx, (bmax(1,k), k = 1, 3), (gmax(1,k), k = 1, 3),
     +    qy, (bmax(2,k), k = 1, 3), (gmax(2,k), k = 1, 3),
     +    qs, (bmax(3,k), k = 1, 3), (gmax(3,k), k = 1, 3)
        call prline(iqpr2)
        write (iqpr2, 920) orbmax, orbrms
      else
        do 220 j = 1, 4
          orbmax(j) = 1000.0 * orbmax(j)
          orbrms(j) = 1000.0 * sqrt(orbrms(j)/pos)
          disrms(j) = sqrt(disrms(j)/pos)
  220   continue
        write (iqpr2, 930)
     +    qx, (bmax(1,k), k = 1, 2), (gmax(1,k), k = 1, 2),
     +    qy, (bmax(2,k), k = 1, 2), (gmax(2,k), k = 1, 2)
        call prline(iqpr2)
        write (iqpr2, 940)
     +    (dismax(j), j=1,4), (disrms(j), j=1,4),
     +    (orbmax(j), j=1,4), (orbrms(j), j=1,4)
      endif
      call prline(iqpr2)
 
*---- Drop LINE condition bank.
      if (ltwlin .ne. 0) call lndrop(ltwlin)
 
*---- Close lattice function table.
      if (ltwfun .ne. 0) call emtwsv(3, savnam, 0, em, amu)
 
  910 format(t31,'Q [1]',t44,'betx [m]',t60,'bety [m]',t76,'bett [m]',
     +       t90,'gamx [1/m]',t106,'gamy [1/m]',t122,'gamt [1/m]'/
     +       ' Mode 1',t20,7f16.6/' Mode 2',t20,7f16.6/
     +       ' Mode 3',t20,7f16.6)
  920 format(t46,'x [mm]',t59,'px [mrad]',t78,'y [mm]',t91,'py [mrad]',
     +       t110,'t [mm]',t123,'pt [mrad]'/
     +       ' orbit',t13,'(abs. max.)',t36,6f16.6/
     +       t13,'(r.m.s.)',t36,6f16.6)
  930 format(t31,'Q [1]',t44,'betx [m]',t60,'bety [m]',t76,
     +       'gamx [1/m]',t90,'gamy [1/m]'/
     +       ' Mode 1',t20,5f16.6/' Mode 2',t20,5f16.6)
  940 format(t47,'x [m]',t60,'px [rad]',t79,'y [m]',t92,'py [rad]'/
     +       ' dispersion',t13,'(abs. max.)',t36,4f16.6/
     +       t13,'(r.m.s.)',t36,4f16.6/
     +       t46,'x [mm]',t59,'px [mrad]',t78,'y [mm]',t91,'py [mrad]'/
     +       ' orbit',t13,'(abs. max.)',t36,4f16.6/
     +       t13,'(r.m.s.)',t36,4f16.6)
 
 9999 end
