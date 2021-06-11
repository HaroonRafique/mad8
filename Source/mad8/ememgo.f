      subroutine ememgo(deltap)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute emittances by A. Chao's method.                            *
* Input:                                                               *
*   DELTAP     (real)   Average energy error desired.                  *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer i,icode,ienum,iflag,iocc,ipos,j,j1,j2,jbyt,k,k1,k2
      double precision aival,bmax,bx,deltap,dismax,el,em,em2,gmax,gx,
     +orbit1,rd,reval,tol,twopi
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi, tol = 1.000001d0)
 
      dimension         em(6,6), rd(6,6), reval(6), aival(6)
      dimension         orbit1(6), bmax(3,3), gmax(3,3), dismax(4)
      dimension         em2(6,6)
      character*(mcnam) elmnam
      logical           m66sta, fmap
 
*---- SYMM is not allowed.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
      if (symm) then
        call aafail('EMEMIT', 1,
     +    '"SYMM" cannot be set for "EMIT" command.')
        go to 9999
      endif
 
*---- Adjust environment.
      call enfix
      if (error) go to 9999
      call enfreq(deltap)
 
*---- Compute one-turn map and find closed orbit.
      call tmturn(lcseq, deltas, error)
      if (error) go to 9999
      do 10 i = 1, 6
        orbit(i) = orbit0(i)
   10 continue
 
*---- Find eigenvectors at initial position.
      if (m66sta(rt)) then
        call laseig(rt, reval, aival, em)
        stabt = .false.
*        write (iqlog, '('' Static map, eigenvalues:'',(/1X,2E15.8))')
*     +    (reval(i), aival(i), i = 1, 4)
      else
        call ladeig(rt, reval, aival, em)
        stabt = reval(5)**2 + aival(5)**2 .le. tol  .and.
     +          reval(6)**2 + aival(6)**2 .le. tol
*        write (iqlog, '('' Static map, eigenvalues:'',(/1X,2E15.8))')
*     +    (reval(i), aival(i), i = 1, 6)
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
   30 continue
 
*---- Initialize damping calculation.
      if (frad .and. stabt) then
        call eminit
        call m66one(rd)
      endif
 
*---- Cumulated length.
      suml = 0.0
 
*---- Loop through element sequence.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        icode = jbyt(iflag, 1, mcode)
 
*---- Physical element.
        if (icode .eq. 1) then
 
*---- Misalignment at entrance.
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, em, em)
            if (frad .and. stabt) call m66mpy(re, rd, rd)
          endif
 
*---- Keep orbit at entrance.
          call ucopy(orbit, orbit1, 6*mwflt)
 
*---- Track through element.
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
 
*---- Advance dispersion.
            if (.not. stabt) then
              call m66byv(re, disp, disp)
              do 40 j = 1, 4
                dismax(j) = max(abs(disp(j)),dismax(j))
   40         continue
            endif
            suml = suml + el
 
*---- Radiation damping.
            call m66mpy(re, em, em2)
            if (frad .and. stabt) then
              call emdamp(em, em2, orbit1, orbit, re)
              call m66mpy(re, rd, rd)
            endif
            call ucopy(em2, em, 36*mwflt)
 
*---- Compute beta and gamma.
            do 60 j = 1, 3
              j1 = 2 * j -1
              j2 = 2 * j
              do 50 k = 1, 3
                k1 = 2 * k - 1
                k2 = 2 * k
                bx = em(j1,k1) * em(j1,k1) + em(j1,k2) * em(j1,k2)
                bmax(j,k) = max(bmax(j,k),bx)
                gx = em(j2,k1) * em(j2,k1) + em(j2,k2) * em(j2,k2)
                gmax(j,k) = max(gmax(j,k),gx)
   50         continue
   60       continue
          endif
 
*---- Misalignment at exit.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, em, em)
            if (frad .and. stabt) call m66mpy(re, rd, rd)
          endif
 
*---- Entrance of line.
        else if (icode .eq. 2) then
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, em, em)
            if (frad .and. stabt) call m66mpy(re, rd, rd)
          endif
 
*---- Exit of line.
        else
 
*---- Misalignment.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            if (.not. stabt) call m66byv(re, disp, disp)
            call m66mpy(re, em, em)
            if (frad .and. stabt) call m66mpy(re, rd, rd)
          endif
        endif
   90 continue
 
*---- Undamped tunes and beam extents.
      qx = nsup * atan2(aival(1), reval(1)) / twopi
      if (qx .lt. 0.0) qx = qx + nsup
      qy = nsup * atan2(aival(3), reval(3)) / twopi
      if (qy .lt. 0.0) qy = qy + nsup
      qs = nsup * atan2(aival(5), reval(5)) / twopi
      if (qs .lt. 0.0) qs = - qs
 
*---- Summary output.
      call emsumm('EMIT', rd, em, bmax, gmax, dismax)
 
 9999 end
