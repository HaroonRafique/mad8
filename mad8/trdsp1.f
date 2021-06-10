      subroutine trdsp1(ipos, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Displace tracks at entry due to misalignment.                      *
* Input:                                                               *
*   IPOS      (integer) Position in beam line (unused).                *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   KTRACK    (integer) number of surviving tracks.                    *
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
      integer ipos,itrack,ktrack
      double precision beti,d,one,phi,psi,pt1,px1,py1,s2,s2bg,the,track,
     +two,w,w33i,x1,y1
      dimension         track(6,*)
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
 
      dimension         d(6), w(3,3)
      parameter         (two = 2.0d0, one = 1.0d0)
 
      call ucopy(q(lcali+1), d, 6*mwflt)
 
*---- Build rotation matrix and compute additional drift length.
      the = d(5)
      phi = d(4)
      psi = d(6)
      call sumtrx(the, phi, psi, w)
      beti = one / betas
      w33i = one / w(3,3)
      s2   = (w(1,3) * d(1) + w(2,3) * d(2) + w(3,3) * d(3)) * w33i
      s2bg = s2 / (betas*gammas)**2
 
*---- Loop for all particles.
      do 90 itrack = 1, ktrack
        px1 = track(2,itrack)
        py1 = track(4,itrack)
        pt1 = track(6,itrack)
        track(2,itrack) = w(1,1) * px1 + w(2,1) * py1 +
     +    w(3,1) * (one + pt1 * beti)
        track(4,itrack) = w(1,2) * px1 + w(2,2) * py1 +
     +    w(3,2) * (one + pt1 * beti)
 
        x1  = track(1,itrack) + s2 * px1 - d(1)
        y1  = track(3,itrack) + s2 * py1 - d(2)
        track(1,itrack) = (w(2,2) * x1 - w(1,2) * y1) * w33i
        track(3,itrack) = (w(1,1) * y1 - w(2,1) * x1) * w33i
        track(5,itrack) = track(5,itrack) - s2bg * pt1 +
     +    ((w(1,3) * x1 + w(2,3) * y1) / w(3,3) - d(3)) * beti
   90 continue
 
      end
