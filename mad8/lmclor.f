      subroutine lmclor(lseq, deltap, show, nord, eflag, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find closed orbit for a beam line sequence using Lie algebra.      *
* Input:                                                               *
*   LSEQ(1)   (pointer) Beam line sequence bank.                       *
*   DELTAP    (real)    Relative energy error.                         *
*   SHOW      (logical) Print flag.                                    *
*   NORD      (integer) Desired map order.                             *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
*   FP(*), FM(6,6)      Map about closed orbit.                        *
* Important common data:                                               *
*   ORBIT0(6) /OPTIC0/  Initial conditions for closed orbit, returned. *
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
      integer i,irank,itmax,itra,j,nord
      double precision cogood,costep,deltap,dmat,err,fm,fp,guess,one,
     +small,smat,toler,two,zero
      integer           lseq(1)
      logical           show, eflag
      dimension         fp(*), fm(6,6)
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
      double precision coest,cotol
 
*---- Estimate for closed orbit search.
      common /coesti/ coest(6), cotol
      save            /coesti/
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      double precision cohelp
 
*---- Communication area for TM module.
*     Positive sign means deflection to negative coordinates.
      common /tmcomm/   cohelp
      save              /tmcomm/
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      dimension         dmat(6,7), smat(4,5)
      logical           m66sta
      parameter         (itmax = 20)
      parameter         (zero = 0.0d0, one = 1.0d0, toler = 1.d-6)
      parameter         (two = 2.0d0, small = 0.01d0)
 
*---- Retrieve beam line description.
      call utbeam(lseq, irg1, irg2, symm, nsup, linnam, rngnam)
*---- Clear closed orbit displacement.
      cohelp = one
      costep = one
      cogood = zero
 
*---- Initialize guess.
      call ucopy(coest, orbit0, 6*mwflt)
 
*---- Store delta(p)/p for the static case.
   10 continue
      if (show) write (iqlog, 910) linnam, nord, deltap
  910 format(' '/' Searching for closed orbit for line: ',a,
     +       '   order =',i2,'.'/
     +       t11,'Delta(p)/p =',f12.6/
     +       ' Iteration',5x,'x',11x,'px',10x,'y',11x,'py',10x,'t',11x,
     +       'pt',9x,'error')
 
*---- Iteration loop.
      do 190 itra = 1, itmax
        call lalump(nord, lseq, fp, fm)
        err = zero
 
*---- Static case (constant p).
        if (m66sta(fm)) then
          do 120 i = 1, 4
            do 110 j = 1, 4
              smat(i,j) = fm(i,j)
  110       continue
            smat(i,i) = smat(i,i) - one
            smat(i,5) = orbit(i) - orbit0(i)
            err = max(abs(smat(i,5)), err)
  120     continue
          call solver(smat, 4, 1, irank)
          do 130 i = 1, 4
            orbit0(i) = orbit0(i) - smat(i,5)
  130     continue
 
*---- Dynamic case (variable p).
        else
          do 170 i = 1, 6
            do 160 j = 1, 6
              dmat(i,j) = fm(i,j)
  160       continue
            dmat(i,i) = dmat(i,i) - one
            dmat(i,7) = orbit(i) - orbit0(i)
            err = max(abs(dmat(i,7)), err)
  170     continue
 
          call solver(dmat, 6, 1, irank)
          do 180 i = 1, 6
            orbit0(i) = orbit0(i) - dmat(i,7)
  180     continue
        endif
 
*---- Message and convergence test.
        if (show) write (iqlog, 920) itra, orbit0, err
  920   format(' ',i9,6f12.6,1p,e16.6)
        if (err .lt. toler) go to 830
  190 continue
 
*---- No convergence.
      if (show) write (msg(1), 930) itmax
  930 format('Closed orbit did not converge in ',i5,' iterations.')
      costep = costep / two
      go to 850
 
*---- Converged with current factor.
  830 continue
      if (cohelp .ge. one) go to 890
      if (costep .le. small) go to 880
      if (show) write (msg(1), 950) cohelp
  950 format('Succeeded to find closed orbit for sextupoles reduced to',
     +       2p,f6.2,' percent.')
      call ucopy(orbit0, guess, 6*mwflt)
      cogood = cohelp
      if (itra .eq. 1) then
        costep = costep * two
      else if (itra .gt. 3) then
        costep = costep / two
      endif
      cohelp = min(cogood + costep, one)
      if (show) then
        write (msg(2), 960) cohelp
  960   format('Continuing with sextupoles increased to',
     +       2p,f6.2,' percent.')
        call aainfo('LMCLOR', 2, msg)
      endif
      go to 10
 
*---- Test for possible restart.
  850 continue
      if (costep .le. small) go to 880
      costep = costep / two
      cohelp = min(cogood + costep, one)
      if (show) then
        write (msg(2), 970) cohelp
  970   format('Restarting with sextupoles reduced to',
     +         2p,f6.2,' percent.')
        call aainfo('LMCLOR', 2, msg)
      endif
      go to 10
 
*---- Exit with best orbit so far.
  880 continue
      if (show) then
        msg(1) =
     +    'Unable to find closed orbit for full sextupole settings.'
        write (msg(2), 980) cogood
  980   format('The best orbit found had the sextupoles reduced to',
     +         2p,f7.2,' percent.')
        call aafail('LMCLOR', 2, msg)
      endif
      call ucopy(guess, orbit0, 6*mwflt)
      eflag = .true.
 
*---- Restore closed orbit help factor.
  890 cohelp = one
      if (optflg(20))  call ucopy(orbit0, coest, 6*mwflt)
 
      end
