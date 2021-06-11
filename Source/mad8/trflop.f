      subroutine trflop(cmmnt, ntrk, nturn)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Write header for track file.                                       *
* Input:                                                               *
*   CMMNT     (char*80) Comment line.                                  *
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
      integer i,icode,ierr,j,mblock,mctxt,mtrfil,mwdat,mwdum,mwtxt,nd,
     +ni,nr,ntrk,nturn
      double precision twopi
      character*(80)    cmmnt
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
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
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
 
      parameter         (mtrfil = 71)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
      parameter         (mblock = 32000)
      parameter         (mctxt = 200, mwtxt = mctxt / 4)
      parameter         (mwdat =  60, mwdum = 50)
 
      character         cprog*8, text*(mctxt)
      integer           ihead(6), itext(mwtxt), jtext(mwtxt)
 
      data cprog        / 'MAD     ' /
      data icode        / 7 /
 
*---- Set up track file: Set block size, unit is 32-bit word (4 bytes).
      itrfil = mtrfil
      nd = mblock / 2
      call epsetw(itrfil, 1, nd, ierr)
      call epsetw(itrfil, 3, 32, ierr)
 
*---- Lift file and record buffers.
      nd = mblock / mcwrd
      call mzbook(2, ltrfbf, ltrfbf, 1, 'FBUF', 0, 0, nd, 0, -1)
      nd = max(9 * ntrk, mwtxt + mwdat + mwdum)
      call mzbook(2, ltrrbf, ltrrbf, 1, 'RBUF', 0, 0, nd, 0, -1)
 
*---- Pack text information.
      text =  ctitle // cmmnt // cdate // ctime // cprog
      call uctoh(text, itext, mcwrd, mctxt)
      call stoasc(itext, 1, jtext, 1, mctxt)
      call blo32w(jtext, 1, iq(ltrrbf+1), 1, mwtxt)
 
*---- Pack integer data to buffer bank.
      iq(ltrrbf+mwtxt+1) = 1
      iq(ltrrbf+mwtxt+2) = ntrk
      iq(ltrrbf+mwtxt+3) = ntrk
      iq(ltrrbf+mwtxt+4) = icode
      iq(ltrrbf+mwtxt+5) = nturn
      ni = mwtxt + 5
 
*---- Pack real data to buffer bank.
      q(ltrrbf+mwtxt+6) = qx
      q(ltrrbf+mwtxt+7) = qy
      q(ltrrbf+mwtxt+8) = qs
      nr = mwtxt + 8
      do 20 i = 1, 6
        do 10 j = 1, 6
          q(ltrrbf+nr+j) = eigen(i,j)
   10   continue
        q(ltrrbf+nr+7) = orbit0(i)
        q(ltrrbf+nr+8) = disp0(i)
        nr = nr + 8
   20 continue
 
*---- Pack program-dependent data to buffer bank.
      do 30 i = 1, mwdum
        nr = nr + 1
        q(ltrrbf+nr) = 0.0
   30 continue
      call ctoibm(q(ltrrbf+ni+1), nr - ni, 3)
 
*---- Write buffer in EPIO format.
      ihead(5) = 0
      ihead(6) = 0
      call epoutl(itrfil,3,6,ihead,nr,q(ltrrbf+1),q(ltrfbf+1),ierr)
      if (ierr .ne. 0) then
        call aafail('TRFLOP', 1, 'Cannot write file header.')
        iffreq = 0
      endif
 
      end
