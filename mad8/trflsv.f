      subroutine trflsv(iturn, track, number, ntrk)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Write particle positions for current turn to track file.           *
* Input:                                                               *
*   ITURN     (integer) Turn number.                                   *
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
      integer i,ierr,itrack,itrk,iturn,ni,nr,ntrk
      double precision track
      dimension         track(6,*)
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
 
      integer           ihead(6)
 
*---- Skip hidden track (closed orbit).
      if (number(1) .eq. 0) then
        itrk = 2
        ihead(6) = ntrk - 1
      else
        itrk = 1
        ihead(6) = ntrk
      endif
 
*---- Pack integer data to buffer bank.
      ihead(5) = iturn
      ni = 0
      do 10 i = itrk, ntrk
        ni = ni + 1
        iq(ltrrbf+ni) = number(i)
   10 continue
      nr = ni
 
*---- Pack real data to buffer bank.
      do 20 itrack = itrk, ntrk
        q(ltrrbf+nr+1) = track(1,itrack)
        q(ltrrbf+nr+2) = track(2,itrack)
        q(ltrrbf+nr+3) = track(3,itrack)
        q(ltrrbf+nr+4) = track(4,itrack)
        q(ltrrbf+nr+5) = track(5,itrack)
        q(ltrrbf+nr+6) = track(6,itrack)
        q(ltrrbf+nr+7) = 0.0
        q(ltrrbf+nr+8) = en0
        nr = nr + 8
   20 continue
      call ctoibm(q(ltrrbf+ni+1), nr - ni, 3)
 
*---- Write buffer in EPIO format.
      call epoutl(itrfil,3,6,ihead,nr,q(ltrrbf+1),q(ltrfbf+1),ierr)
      if (ierr .ne. 0) then
        write (msg, 910) iturn
  910   format('Cannot write record for turn number ',i8,'.')
        call aafail('TRFLSV', 1, msg)
        iffreq = 0
      endif
 
      end
