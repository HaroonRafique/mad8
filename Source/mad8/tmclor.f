      subroutine tmclor(lseq, deltap, show, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find closed orbit for a beam line sequence.                        *
* Input:                                                               *
*   LSEQ(1)   (pointer) Beam line sequence bank.                       *
*   DELTAP    (real)    Relative energy error.                         *
*   SHOW      (logical) Print flag.                                    *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
* Important common data:                                               *
*   ORBIT0(6) /OPTIC0/  Initial conditions for closed orbit, returned. *
*   RT(6,6)   /MAPTRN/  Transfer matrix for (half) superperiod.        *
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
      integer i,irank,itmax,itra,k
      double precision a,as,b,bs,cogood,costep,deltap,det,err,guess,one,
     +small,two,zero
      integer           lseq(*)
      logical           show, eflag
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
      double precision coest,cotol
 
*---- Estimate for closed orbit search.
      common /coesti/ coest(6), cotol
      save            /coesti/
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
      double precision cohelp
 
*---- Communication area for TM module.
*     Positive sign means deflection to negative coordinates.
      common /tmcomm/   cohelp
      save              /tmcomm/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mmaxel,mwind,ndccnt,ndflag,nditer,ndocc,ndpos,ndtype,
     +nlpos
      double precision admatr,adorbt,adsuml,adtol,orbkpt,reforb,skpt
*--- common block for threader variables
      parameter (mwind = 500, mmaxel = 20000)
      common/thrcml/adthfl, adwofl, adcofl
      logical adthfl, adwofl, adcofl
      common/thrcmi/ndccnt, ndocc, nlpos, nditer,
     +ndpos(mwind), ndtype(mwind), ndflag(mmaxel)
      common/thrcmr/adtol(6), reforb(6), adsuml(mwind),
     +adorbt(6,mwind), admatr(6,6,mwind), orbkpt(6,mmaxel),
     +skpt(mmaxel)
 
      dimension         a(6,7), b(4,5), as(3,4), bs(2,3)
      equivalence       (a(1,1), b(1,1), as(1,1), bs(1,1))
      dimension         guess(6)
      logical           m66sta, symm
      character*(mcnam) linnam
      character*(mcrng) rngnam
      parameter         (itmax = 50)
      parameter         (one = 1.0d0, zero = 0.0d0, two = 2.0d0)
      parameter         (small = 0.01d0)
 
*---- Initialize.
      cohelp = one
      costep = one
      cogood = zero
      symm = iq(lseq(1)+msym) .ne. 0
      call uhtoc(q(lseq(1)+msbn), mcwrd, linnam, mcnam)
      call uhtoc(q(lseq(1)+msrn), mcwrd, rngnam, 40)
      if (show) then
        write (iqlog, 910) linnam, rngnam, deltap, symm
        write (iqlog, 920) 0, coest
      endif
  910 format(' '/' TMCLOR.  Searching for closed orbit, beam line: "',
     +       a,'", range: ',a/
     +       t11,'Delta(p)/p =',f12.6,', symmetry = ',l1/
     +       ' Iteration',5x,'x',11x,'px',10x,'y',11x,'py',10x,'t',11x,
     +       'deltap',6x,'error')
 
*---- Initialize guess.
      eflag = .false.
 
      call ucopy(coest, guess, 6*mwflt)
*---- Restart with best guess.
   10 continue
      call ucopy(guess, orbit0, 6*mwflt)
 
*---- Iteration for closed orbit.
      do 300 itra = 1, itmax
 
*---- Track orbit and transfer matrix.
        call tmfrst(lseq, eflag)
        if (eflag) go to 810
*---- Solve for dynamic case.
        if (docav  .and.  .not. m66sta(rt)) then
          err = 0.0
          if (symm) then
            call aafail('TMCLOR', 1,
     +      'SYMM not allowed with line containing RF cavities'
     +      // ' and/or synchrotron radiation.')
          else
            do 130 i = 1, 6
              do 120 k = 1, 6
                a(i,k) = rt(i,k)
  120         continue
              a(i,i) = a(i,i) - 1.0
              a(i,7) = orbit(i) - orbit0(i)
              err = max(abs(a(i,7)), err)
  130       continue
            call solver(a, 6, 1, irank)
            if (irank .lt. 6) go to 820
            do 140 i = 1, 6
              orbit0(i) = orbit0(i) - a(i,7)
  140       continue
          endif
 
*---- Solve for static case.
        else
          if (symm) then
            det = rt(2,1) * rt(4,3) - rt(2,3) * rt(4,1)
            orbit0(1) = orbit0(1) -
     +        (rt(4,3) * orbit(2) - rt(2,3) * orbit(4)) / det
            orbit0(3) = orbit0(3) -
     +        (rt(2,1) * orbit(4) - rt(4,1) * orbit(2)) / det
            err = max(abs(orbit(2)),abs(orbit(4)))
          else
            err = 0.0
            do 230 i = 1, 4
              do 220 k = 1, 4
                b(i,k) = rt(i,k)
  220         continue
              b(i,i) = b(i,i) - 1.0
              b(i,5) = orbit(i) - orbit0(i)
              err = max(abs(b(i,5)), err)
  230       continue
            call solver(b, 4, 1, irank)
            if (irank .lt. 4) go to 820
            do 240 i = 1, 4
              orbit0(i) = orbit0(i) - b(i,5)
  240       continue
          endif
        endif
 
*---- Message and convergence test.
        if (show) write (iqlog, 920) itra, orbit0, err
  920   format(' ',i9,1p,6e14.6,e16.6)
*        print *, ' '
*        print '(1p,6e14.6)', ((rt(k,i),i=1,6),k=1,6)
        if (err .lt. cotol) go to 830
  300 continue
 
*---- No convergence.
      write (msg(1), 930) itmax
  930 format('Closed orbit did not converge in ',i5,' iterations.')
      costep = costep / two
      go to 850
 
*---- Overflow problems.
  810 continue
      write (iqlog, 940) itra
  940 format(' An overflow occurred in iteration ',i2,'.')
      if (.not. adcofl)  goto 850
      if (adthfl .or. ndccnt .ne. 0)  then
        if (show)  then
          msg(1) =
     +    'Unable to find closed orbit with threader.'
          call aafail('TMCLOR', 1, msg)
        endif
        goto 981
      else
        adthfl = .true.
        if (show) write (iqlog, *) 'Switching Threader on.'
        goto 10
      endif
 
*---- Matrix inversion problems.
  820 continue
      msg(1) = 'Singular matrix occurred during closed orbit search.'
      go to 850
 
*---- Converged with current factor.
  830 continue
      if (cohelp .ge. one) go to 890
      if (costep .le. small) go to 880
      write (msg(1), 950) cohelp
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
        call aainfo('TMCLOR', 2, msg)
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
        call aainfo('TMCLOR', 2, msg)
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
        call aafail('TMCLOR', 2, msg)
      endif
  981 continue
      call ucopy(guess, orbit0, 6*mwflt)
      eflag = .true.
 
*---- Restore closed orbit help factor.
  890 cohelp = one
      if (optflg(20))  call ucopy(orbit0, coest, 6*mwflt)
*---- switch threader off
      adthfl = .false.
 
      end
