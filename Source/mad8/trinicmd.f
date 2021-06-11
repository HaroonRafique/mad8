      subroutine trinicmd
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Define initial conditions for a single particle;                   *
*   DYNAPSTART command.                                                *
* Attributes:                                                          *
*   X, PX, Y, PY, T, DELTAP, FX, PHIX, FY, PHIY, FT, PHIT, TURNS,      *
*   STEP, CHKBELOW, FRACMIN, FASTUNE, LYAPUNOV, DAMP, QUANTUM,         *
*   CONTINUE, METHOD, TABLE, ORBIT.                                    *
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
      integer k,kp,kq,mtrack
      double precision phi,track,trturns,twopi,z,zn
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
      parameter         (mtrack = 50)
 
      dimension         track(12), zn(6), z(6)
      integer           itype(23)
      logical           zgiv, zngiv, flcont
 
*---- Copy data to local storage.
      call uzero(track(1), 1, 12 * mwflt)
      call utgflt(lccmd, 1, 12, track)
      call utgtyp(lccmd, itype)
      trturns  = 64
      trstep   = 0.05d0
      chkbelow = 0.0d0
      fracmin  = 0.1d0
      fastune  = .false.
      lyapflag = .false.
      deltax   = 1.0d-7
      fdamp    = .false.
      frand    = .false.
      flcont   = .false.
      orbflag  = .true.
      call utgflt(lccmd, 13, 13, trturns)
      itrturns = int(trturns)
      call utgflt(lccmd, 14, 14, trstep)
      call utgflt(lccmd, 15, 15, chkbelow)
      call utgflt(lccmd, 16, 16, fracmin)
      call utglog(lccmd, 17, 17, fastune)
      call utgflt(lccmd, 18, 18, deltax)
      if (itype(18) .ne. 0) lyapflag = .true.
      call utglog(lccmd, 19, 19, fdamp)
      call utglog(lccmd, 20, 20, frand)
      call utglog(lccmd, 21, 21, flcont)
      call utglog(lccmd, 24, 24, orbflag)
 
*---- Tracking method and table name.
      trknam(1) = 'TRANSPORT'
      trknam(2) = ' '
      call utgnam(lccmd, 22, 23, trknam)
 
*---- Initialise orbit, emittances and eigenvectors.
      call trbegn(.false., orbflag)
      if (error) return
 
*---- Normalized coordinates.
      do 10 kq = 1, 5, 2
        kp = kq + 1
        phi = twopi * track(kq+7)
        zn(kq) =   track(kq+6) * cos(phi)
        zn(kp) = - track(kq+6) * sin(phi)
   10 continue
 
*---- Transform to unnormalized coordinates and refer to closed orbit.
      zgiv = .false.
      zngiv = .false.
      do 20 k = 1, 6
        if (itype(k) .ne. 0) zgiv = .true.
        if (itype(k+6) .ne. 0) zngiv = .true.
        zstart(k) = track(k)
     +       + sqrt(ex) * (eigen(k,1) * zn(1) + eigen(k,2) * zn(2))
     +       + sqrt(ey) * (eigen(k,3) * zn(3) + eigen(k,4) * zn(4))
     +       + sqrt(et) * (eigen(k,5) * zn(5) + eigen(k,6) * zn(6))
   20 continue
 
*---- Warn user about possible data conflict.
      if (zgiv .and. zngiv) then
        msg(1) = 'Absolute and normalized coordinates given,'
        msg(2) = 'Superposition used.'
        call rdwarn('START', 2, msg)
      endif
 
*---- Set initial betatron invariants.
      if (flcont) then
        do 30 k = 1, 6
          zstart(k) =  zstart(k) + zendyn(k)
   30   continue
      endif
      do 40 k = 1, 6
        z(k) = orbit0(k) + zstart(k)
   40 continue
      wxmax = 0d0
      wymax = 0d0
      wxymax = 0d0
      call wmaxmin(z)
      wxstart = wxmax
      wystart = wymax
      wxystart = wxymax
      wxmin = wxmax
      wymin = wymax
      wxymin = wxymax
 
      write (iqpr2, 930) zstart(1), zstart(2),
     +                   zstart(3), zstart(4),
     +                   zstart(5), zstart(6),
     +                   itrturns,  fracmin,  trstep, chkbelow
      write (iqpr2, 940) fastune, lyapflag, fdamp, frand, flcont
      write (iqpr2, 950) trknam(1), trknam(2)
 
*---- Make parameters accessible to VALUE and PUSH.
      call aasetp('XSTART',    zstart(1))
      call aasetp('PXSTART',   zstart(2))
      call aasetp('YSTART',    zstart(3))
      call aasetp('PYSTART',   zstart(4))
      call aasetp('TSTART',    zstart(5))
      call aasetp('PTSTART',   zstart(6))
      call aasetp('TURNS',     itrturns*1.d0)
      call aasetp('STEP',      trstep)
      call aasetp('CHKBELOW',  chkbelow)
      call aasetp('FRACMIN',   fracmin)
      call aasetp('WXSTART',   wxstart)
      call aasetp('WYSTART',   wystart)
      call aasetp('WXYSTART',  wxystart)
      call aasetp('D0',        deltax)
 
*---- Activate start flag.
      fstart = .true.
 
  930 format(' '/' Initial conditions w.r.t. closed orbit',
     +  ' for dynamic aperture search'/
     +  ' XSTART =',t10,f14.12,t36,'PXSTART =',f14.12/
     +  ' YSTART =',t10,f14.12,t36,'PYSTART =',f14.12/
     +  ' TSTART =',t10,f14.12,t36,'PTSTART =',f14.12/
     +  ' Tracking over:',t17,'TURNS =',i7,t36,'FRACMIN =',f14.12/
     +  ' Fractional step for downward search:',t39,'STEP =',f14.12/
     +  ' Stability analysis below dynap:',t35,'CHKBELOW =',f14.12)
  940 format(' FASTUNE = ',l1, ', LYAPUNOV = ',l1,', DAMPING = ',l1,
     +  ', QUANTUM = ',l1,', CONTINUE = ',l1)
  950 format(' METHOD = ',a,', TABLE = ',a)
 
      end
