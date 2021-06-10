      subroutine tmali1(ipos, fsec, orb1, orb2, rm, tm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for orbit displacement at entry of an element.       *
* Input:                                                               *
*   IPOS      (integer) Position in beam line (not used).              *
*   FSEC      (logical) If true, return second order terms.            *
*   ORB1(6)   (real)    Orbit before misalignment.                     *
* Output:                                                              *
*   ORB2(6)   (real)    Orbit after misalignment.                      *
*   RM(6,6)   (real)    First order transfer matrix w.r.t. orbit.      *
*   TM(6,6,6) (real)    Second order terms.                            *
* Important common data:                                               *
*   LCALI     /REFER/   Current misalignment pointer.                  *
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
      integer ipos
      double precision d,ds,dx,dy,orb1,orb2,orbt,phi,psi,rm,s2,the,tm,w
      logical           fsec
      dimension         orb1(6), orb2(6), rm(6,6), tm(6,6,6)
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
 
      dimension         d(6), w(3,3), orbt(6)
 
*---- Build rotation matrix and compute additional drift length.
      call ucopy(q(lcali+1), d, 6*mwflt)
      dx  = d(1)
      dy  = d(2)
      ds  = d(3)
      the = d(5)
      phi = d(4)
      psi = d(6)
      call sumtrx(the, phi, psi, w)
      s2 = (w(1,3) * dx + w(2,3) * dy + w(3,3) * ds) / w(3,3)
 
*---- F2 terms (transfer matrix).
      call m66one(rm)
      rm(2,2) = w(1,1)
      rm(2,4) = w(2,1)
      rm(2,6) = w(3,1) / betas
      rm(4,2) = w(1,2)
      rm(4,4) = w(2,2)
      rm(4,6) = w(3,2) / betas
 
      rm(1,1) =   w(2,2) / w(3,3)
      rm(1,2) = rm(1,1) * s2
      rm(1,3) = - w(1,2) / w(3,3)
      rm(1,4) = rm(1,3) * s2
      rm(3,1) = - w(2,1) / w(3,3)
      rm(3,2) = rm(3,1) * s2
      rm(3,3) =   w(1,1) / w(3,3)
      rm(3,4) = rm(3,3) * s2
      rm(5,1) = w(1,3) / (w(3,3) * betas)
      rm(5,2) = rm(5,1) * s2
      rm(5,3) = w(2,3) / (w(3,3) * betas)
      rm(5,4) = rm(5,3) * s2
      rm(5,6) = - s2 / (betas * gammas)**2
 
*---- Second-order effects are ignored (coefficients all of order 1).
      if (fsec) then
        call uzero(tm, 1, 216*mwflt)
      endif
 
*---- Track orbit.
      call m66byv(rm, orb1, orbt)
      orb2(1) = orbt(1) - (w(2,2) * dx - w(1,2) * dy) / w(3,3)
      orb2(2) = orbt(2) + w(3,1)
      orb2(3) = orbt(3) - (w(1,1) * dy - w(2,1) * dx) / w(3,3)
      orb2(4) = orbt(4) + w(3,2)
      orb2(5) = orbt(5) - s2 / betas
      orb2(6) = orbt(6)
 
      end
