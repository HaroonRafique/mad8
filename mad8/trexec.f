      subroutine trexec(doelem, iorder, nturn, track, number, ntrk,
     +  zn, distvect)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track specified number of turns.                                   *
* Input:                                                               *
*   DOELEM    (subr)    Routine to track according to selected method. *
*   IORDER    (integer) Order for Lie algebraic tracking.              *
*   NTURN     (integer) Number of turns to be tracked.                 *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track.                       *
*   NTRK      (integer) Number of surviving tracks.                    *
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
      integer iorder,iturn,k,kp,kq,nline,nt,ntrk,nturn
      double precision distvect,doelem,templyap,ten6m,track,ttot,tturn,
     +wxy,zn,znt
      external          doelem
      dimension         track(6,*), zn(nturn,4), distvect(*)
      integer           number(*)
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
      integer iffreq,ipfreq,itrfil,npart,ntrack
 
*---- Common flags for tracking.
      common /trkchr/   trktitle
      character * 32    trktitle
      common /trkint/   ipfreq, iffreq, npart, ntrack, itrfil
      common /trktim/   time1, time2, dtime
      common /trkflg/   onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      save              /trkint/, /trktim/, /trkflg/
      real              time1, time2, dtime
      logical           onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      integer itrturns,ktrturns
      double precision chkbelow,deltax,dtune,dynapfrac,fracmin,smear,
     +trstep,tunx,tuny,wxmax,wxmin,wxstart,wxymax,wxymin,wxystart,wymax,
     +wymin,wystart,yapunov,zendyn,zstart
 
*---- Communication area for DYNAP command.
      common /trcdyn/   trknam(2)
      character*(mcnam) trknam
      common /trfdyn/   chkbelow, deltax, dtune, dynapfrac, fracmin,
     +                  smear, trstep, tunx, tuny,
     +                  wxmax, wxmin, wymax, wymin, wxymax, wxymin,
     +                  wxstart, wystart, wxystart, yapunov,
     +                  zstart(6), zendyn(6)
      common /tridyn/   itrturns, ktrturns
      common /trldyn/   fastune, lyapflag, orbflag
      logical           fastune, lyapflag, orbflag
      save              /trcdyn/, /trfdyn/, /tridyn/, /trldyn/
      integer ltrbuf,ltrcur,ltrfbf,ltrnoi,ltrnum,ltrobs,ltrrbf,ltrstt,
     +ltrtab,ltrtmp
 
*---- Links used for tracking.
      common /trlink/   ltrnum, ltrstt, ltrnoi, ltrfbf, ltrrbf, ltrobs,
     +                  ltrcur, ltrtmp, ltrtab, ltrbuf
      save              /trlink/
      double precision aival,eigen,reval
 
*---- Initial conditions for optical functions for tracking.
      common /troptc/   eigen(6,6), reval(6), aival(6)
      save              /troptc/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      dimension         znt(6)
      real              timeb, timee, timer
 
      parameter         (ten6m = 1.0d-6)
 
*---- Copy damping flags and possibly orbit.
      dorad  = frad
      dodamp = fdamp
      dorand = frand
      nt = 1
      if (dorad  .and.  .not. dodamp) then
        nt = 2
        call ucopy(orbit0, track(1,1), 6*mwflt)
      endif
 
*---- Save initial data in track table.
      if (ltrtab .ne. 0) then
        if (lyapflag) then
          call trtbsvly(ltrtab, 0, track(1,nt))
        else
          call trtbsv(ltrtab, 0, orbit0, track, number, ntrk)
        endif
      endif
 
*---- Output initial conditions.
      if (ipfreq .gt. 0) then
        call prhead('RUN', trktitle, deltas, 0, nline, 1)
        if (onepss) then
          write (iqpr2, 910)
          write (iqpr2, 935) 1, ener1
        else
          write (iqpr2, 920) ex, ey, et
          write (iqpr2, 930) 1
        endif
        call trptrn(track, number, ntrk)
      endif
 
*---- Open track file.
      if (iffreq .ne. 0) then
        if (itrfil .ne. 0) call trflsv(0, track, number, ntrk)
      endif
 
*---- Initialize data pointers.
      lsali = lq(lcseq-msali)
      lscom = lq(lcseq-mscom)
      lsdir = lq(lcseq-msdir)
      lsfld = lq(lcseq-msfld)
      lsflg = lq(lcseq-msflg)
      lcali = 0
      lccom = 0
      lcfld = 0
 
*---- Time required for one turn around machine.
      tturn = 0.0
      if (freq0 .ne. 0.0) tturn = ten6m / freq0
      ttot = 0.0
*---- Loop for turns.
      call timex(timeb)
      do 90 iturn = 1, nturn
 
*---- Apply noise perturbations.
        if (ltrnoi .ne. 0) then
          call trnset(ttot)
          call aapdrp
          ttot = ttot + tturn
        endif
 
*---- Track one turn.
        call trturn(doelem, iorder, iturn, track, number, ntrk)
        if (ntrk .eq. 0) go to 800
 
*---- DYNAP output.
        if (.not. fbelow) then
          if (fdynap) then
            do 10 kq = 1, 3, 2
              kp = kq + 1
              znt(kq) =
     +          eigen(2,kp) * (track(1,nt) - orbit0(1)) -
     +          eigen(1,kp) * (track(2,nt) - orbit0(2)) +
     +          eigen(4,kp) * (track(3,nt) - orbit0(3)) -
     +          eigen(3,kp) * (track(4,nt) - orbit0(4)) +
     +          eigen(6,kp) * (track(5,nt) - orbit0(5)) -
     +          eigen(5,kp) * (track(6,nt) - orbit0(6))
              znt(kp) =
     +          eigen(1,kq) * (track(2,nt) - orbit0(2)) -
     +          eigen(2,kq) * (track(1,nt) - orbit0(1)) +
     +          eigen(3,kq) * (track(4,nt) - orbit0(4)) -
     +          eigen(4,kq) * (track(3,nt) - orbit0(3)) +
     +          eigen(5,kq) * (track(6,nt) - orbit0(6)) -
     +          eigen(6,kq) * (track(5,nt) - orbit0(5))
   10       continue
 
*---- Update max and min betatron invariants;
*     convert to amplitudes (and phases: not computed).
            wx = znt(1)**2 + znt(2)**2
            wy = znt(3)**2 + znt(4)**2
            wxy = wx + wy
 
*---- Compare to and redefine WMIN and WMAX in TRDYNAP.
            if (wx.gt.wxmax) then
              wxmax = wx
            else if (wx.lt.wxmin) then
              wxmin = wx
            endif
            if (wy.gt.wymax) then
              wymax = wy
            else if (wy.lt.wymin) then
              wymin = wy
            endif
            if (wxy.gt.wxymax) then
              wxymax = wxy
            else if (wxy.lt.wxymin) then
              wxymin = wxy
            endif
 
*---- Save data in ZN(*,4) array for fast tune.
            if (fastune) then
              zn(iturn,1) = znt(1)
              zn(iturn,2) = znt(2)
              zn(iturn,3) = znt(3)
              zn(iturn,4) = znt(4)
            endif
 
*---- Save data in DISTVECT array for Lyapunov exponent.
            if (lyapflag) then
              templyap = 0.0
              do 20 k = 1, 6
                templyap = templyap +
     +            ((track(k,nt) - track(k,nt+1)) / deltax)**2
   20         continue
              distvect(iturn) = sqrt(templyap)
            endif
          endif
 
*---- Ordinary tracking output.
*     Save data in track table.
          if (ltrtab .ne. 0) then
            if (lyapflag) then
              call trtbsvly(ltrtab, iturn, track(1,nt))
            else
              call trtbsv(ltrtab, iturn, orbit0, track, number, ntrk)
            endif
          endif
 
*---- Save data in track file.
          if (iffreq .gt. 0) then
            if (mod(iturn, iffreq) .eq. 0 .or. iturn .eq. nturn) then
              call trflsv(iturn, track, number, ntrk)
            endif
          endif
 
*---- Turn print-out.
          if (ipfreq .gt. 0  .and.  iturn .lt. nturn) then
            if (mod(iturn, ipfreq) .eq. 0) then
              write (iqpr2, 940) iturn
              call trptrn(track, number, ntrk)
            endif
          endif
        endif
 
*---- Restart the closed orbit.
        if (dorad  .and.  .not. dodamp) then
          call ucopy(orbit0, track(1,1), 6*mwflt)
        endif
 
*---- Test if CPU time is sufficient to go on tracking.
        call timex(timee)
        call timel(timer)
        if (timer .lt. 1.5 * (timee - timeb) + 2.0) then
          write (msg, 950) iturn
          call aawarn('TREXEC', 1, msg)
          nturn = iturn
          go to 800
        endif
        timeb = timee
   90 continue
*---- Final print-out.
  800 continue
      ktrturns = iturn - 1
      if (ipfreq .gt. 0) then
        if (onepss) then
          write (iqpr2, 965) nturn, ener1
        else
          write (iqpr2, 960) nturn
        endif
        call trptrn(track, number, ntrk)
      endif
 
*---- Reset noise data.
      if (ltrnoi .ne. 0) call trnres
 
  910 format(' '/' Positions not normalized.')
  920 format(' '/' Positions normalized to EX =',1p,e16.8,
     +  '     EY =',e16.8,'     ET =',e16.8/
     +  ' (referred to coupled motion around closed orbit).')
  930 format(' '/' Initial particle positions before turn',i8)
  935 format(' '/' Initial particle positions before turn',i8,
     +  ' at energy: ', 1pg14.5)
  940 format(' '/' Particle positions after turn',i8)
  950 format('Tracking stopped due to time limit in turn ',i8,'.')
  960 format(' '/' Final particle positions after turn',i8)
  965 format(' '/' Final particle positions after turn',i8,
     +  ' at energy: ', 1pg14.5)
 
      end
