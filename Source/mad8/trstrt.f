      subroutine trstrt
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Define initial conditions for a particle; START command.           *
* Attributes:                                                          *
*   X, PX, Y, PY, DT, DE, FX, PHIX, FY, PHIY, FT, PHIT.                *
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
      integer ibias,k,kp,kq,mtrack,nd
      double precision phi,track,twopi,z,zn
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
      parameter         (mtrack = 50)
 
      dimension         track(12), z(6), zn(6)
      integer           itype(12)
      logical           zgiv, zngiv
 
*---- Copy data to local storage.
      call uzero(track(1), 1, 12 * mwflt)
      call utgflt(lccmd, 1, 12, track)
      call utgtyp(lccmd, itype)
 
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
        z(k) = orbit0(k) + track(k)
     +       + sqrt(ex) * (eigen(k,1) * zn(1) + eigen(k,2) * zn(2))
     +       + sqrt(ey) * (eigen(k,3) * zn(3) + eigen(k,4) * zn(4))
     +       + sqrt(et) * (eigen(k,5) * zn(5) + eigen(k,6) * zn(6))
   20 continue
 
*---- Warn user about possible data conflict.
      if (zgiv .and. zngiv) then
        msg(1) = 'Absolute and normalized coordinates given,'
        msg(2) = 'Superposition used.'
        call rdwarn('TRSTRT', 2, msg)
      endif
 
*---- Build banks to store track.
      nd = 6 * mtrack * mwflt
      if (ltrstt .eq. 0) then
        call mzbook(2, ltrstt, ltrstt, 1, 'TRAK', 0, 0, nd, mreal, 0)
        call mzbook(2, ltrnum, ltrnum, 1, 'TNUM', 0, 0, mtrack, 2, 0)
      else if (ntrack .ge. iq(ltrnum-1)) then
        call mzpush(0, ltrstt, 0, nd, 'I')
        call mzpush(0, ltrnum, 0, mtrack, 'I')
      endif
      ibias = 6 * mwflt * ntrack
      npart = npart + 1
      ntrack = ntrack + 1
      call ucopy(z, q(ltrstt+ibias+1), 6 * mwflt)
      iq(ltrnum+ntrack) = npart
 
      end
