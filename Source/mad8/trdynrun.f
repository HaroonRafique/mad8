      subroutine trdynrun
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Tracking for dynamic aperture determination.                       *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer ileng,ilyap,imeth,ipx,ipy,istep,iswork,ix,iy,izn,k,
     +maxmet,nbelow,nt,ntt,nturn,nturnhalf
      double precision eigen,fitlyap,frac,one,track,tuneabt,
     +tuneabt2,tunx1,tunx2,tuny1,tuny2
 
      parameter           (maxmet = 3, one = 1.0d0)
 
      external            ttelem, lmelem
      character*(mcnam)   dmeth(maxmet)
      dimension           track(6,3)
      integer             number(3)
 
      data dmeth        / 'TRANSPORT', 'LIE3    ', 'LIE4    ' /
 
*---- Tracking method.
      call utlook(trknam(1), dmeth, maxmet, imeth)
      if (imeth .eq. 0) then
        call utleng(trknam(1), ileng)
        msg(1) = 'Unknown method "' // trknam(1)(1:ileng) //
     +    '" --- "TRANSPORT" assumed.'
        call aawarn('TRRUN', 1, msg)
        imeth = 1
      endif
 
*---- Allocate working space.
      iswork = iwork
      if (fastune) then
        izn = iwork
        iwork = iwork + 4*itrturns
      else
        izn = 0
      endif
      if (lyapflag) then
        ilyap = iwork
        iwork = iwork + itrturns
      else
        ilyap = 0
      endif
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Set number of turns and flags.
      fdynap = .true.
      nturn  = itrturns
      itrfil = 0
      ipfreq = 0
      ipfreq = 0
 
*---- Number of steps for stability check below dynamic aperture
      nbelow = int(log(one - chkbelow) / log(one - trstep))
      dynapfrac = one
 
*---- Define initial conditions for one particle.
      call aagetp('XSTART',  zstart(1))
      call aagetp('PXSTART', zstart(2))
      call aagetp('YSTART',  zstart(3))
      call aagetp('PYSTART', zstart(4))
      call aagetp('TSTART',  zstart(5))
      call aagetp('PTSTART', zstart(6))
*--- store bb_kicks
      if (.not. bborbit) call bbstore()
*---- Start loop until survival is longer than NTURN.
  100 continue
 
*---- Set up optional closed orbit and main track.
        nt = 1
        if (dorad  .and.  .not. dodamp) then
          nt = 2
          number(1) = 0
        endif
        do k = 1, 4
          track(k,1)  = orbit0(k)
          track(k,nt) = orbit0(k) + dynapfrac * zstart(k)
        enddo
        do k = 5, 6
          track(k,1)  = orbit0(k)
          track(k,nt) = orbit0(k) + zstart(k)
        enddo
        number(nt) = 1
 
*---- Set up optional Lyapunov companion.
        ntt = nt
        if (lyapflag) then
          ntt = nt + 1
          do 120 k = 1, 6
            track(k,ntt) = track(k,nt)
  120     continue
          track(1,ntt) = track(1,nt) + deltax
          number(ntt) = 2
        endif
 
*---- Initialize max and min betatron invariants.
        wxmax = 0
        wymax = 0
        wxymax = 0
        call wmaxmin(track(1,nt))
        wxmin = wxmax
        wymin = wymax
        wxymin = wxymax
 
*---- Track according to selected method.
        ntrack = ntt
 
*---- Open main track table.
      if (trknam(2) .ne. ' ') then
        call trtbop(orbit0, eigen, trknam(2), nturn, ntrack)
      endif
 
        fbelow = .false.
        if (imeth .eq. 1) then
          call trexec(ttelem, 3, nturn, track, number, ntrack,
     +      dq(izn+1), dq(ilyap+1))
        else if (imeth .eq. 2) then
          call trexec(lmelem, 3, nturn, track, number, ntrack,
     +      dq(izn+1), dq(ilyap+1))
        else
          call trexec(lmelem, 4, nturn, track, number, ntrack,
     +      dq(izn+1), dq(ilyap+1))
        endif
 
*---- If particle is lost and DYNAPFRAC > FRACMIN then
*     reduce betatron matrix TRACK by 1 - TRSTEP and loop.
      if (ntrack .eq. 0  .and.  dynapfrac .gt. fracmin) then
        dynapfrac = dynapfrac * (one - trstep)
        go to 100
      endif
 
*---- Compute SMEAR and save ZENDYN.
      smear = 2.0 * (wxymax - wxymin) / (wxymax + wxymin)
      do 130 k = 1, 6
        zendyn(k) = track(k,nt) - orbit0(k)
  130 continue
 
*---- Check stability below dynamic aperture,
*     if particle survived NTURN turns.
      if (ktrturns .eq. nturn  .and.  nbelow .ne. 0  .and.
     +    dynapfrac .gt. fracmin) then
        fbelow = .true.
        frac = dynapfrac
 
*---- Start loop to check stability in NBELOW steps below DYNAP.
*     Number of tracks is NT
*     (optional closed orbit + main track)
        do 300 istep = 1, nbelow
          ntrack = nt
          frac = frac * (one - trstep)
          do 210 k = 1, 6
            track(k,1)  = orbit0(k)
            track(k,nt) = orbit0(k) + frac * zstart(k)
  210     continue
 
*---- Track according to selected method, all output suppressed.
          if (imeth .eq. 1) then
            call trexec(ttelem, 3, nturn, track, number, ntrack,
     +        dq(izn+1), dq(ilyap+1))
          else if (imeth .eq. 2) then
            call trexec(lmelem, 3, nturn, track, number, ntrack,
     +        dq(izn+1), dq(ilyap+1))
          else
            call trexec(lmelem, 4, nturn, track, number, ntrack,
     +        dq(izn+1), dq(ilyap+1))
          endif
 
*---- If particle is unstable, restart search loop at next lower step.
          if (ntrack .eq. 0) then
            dynapfrac = frac * (one - trstep)
            go to 100
          endif
  300   continue
      endif
 
*---- Fast tune calculation by interpolated FFT.
      if (fastune) then
        ix  = izn + 1
        ipx = ix  + itrturns
        iy  = ipx + itrturns
        ipy = iy  + itrturns
        if (nturn .le. 64) then
          tunx = tuneabt(dq(ix), dq(ipx), ktrturns)
          tuny = tuneabt(dq(iy), dq(ipy), ktrturns)
        else
          tunx = tuneabt2(dq(ix), dq(ipx), ktrturns)
          tuny = tuneabt2(dq(iy), dq(ipy), ktrturns)
        endif
 
*---- Fast tune variation over half the number of turns.
        nturnhalf = ktrturns / 2
        if (nturnhalf .le. 64) then
          tunx1 = tuneabt(dq(ix), dq(ipx), nturnhalf)
          tuny1 = tuneabt(dq(iy), dq(ipy), nturnhalf)
          tunx2 = tuneabt(dq(ix +nturnhalf),
     +                    dq(ipx+nturnhalf), nturnhalf)
          tuny2 = tuneabt(dq(iy +nturnhalf),
     +                    dq(ipy+nturnhalf), nturnhalf)
        else
          tunx1 = tuneabt2(dq(ix), dq(ipx), nturnhalf)
          tuny1 = tuneabt2(dq(iy), dq(ipy), nturnhalf)
          tunx2 = tuneabt2(dq(ix +nturnhalf),
     +                     dq(ipx+nturnhalf), nturnhalf)
          tuny2 = tuneabt2(dq(iy +nturnhalf),
     +                     dq(ipy+nturnhalf), nturnhalf)
        endif
        dtune = sqrt((tunx2 - tunx1)**2 + (tuny2 - tuny1)**2)
      endif
 
*---- Lyapunov exponent calculation over second half of tracking.
      if (lyapflag) then
        yapunov = fitlyap(dq(ilyap+1), ktrturns)
      endif
 
*---- Close tracking tables.
      if (ltrtab .ne. 0) call trtbcl(ltrtab)
 
*---- Release working store.
      iwork = iswork
      fdynap = .false.
 
      end
