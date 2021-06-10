      subroutine trdsp2(ipos, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Displace tracks at exit of an element due to misalignment.         *
* Input:                                                               *
*   IPOS      (integer) Position in beam line.                         *
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
      double precision arc,beti,d,el,one,phi,psi,pt1,px1,py1,s2,s2bg,
     +the,track,two,ve,w,w33i,we,x1,y1
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
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
 
      dimension         d(6), ve(3), w(3,3), we(3,3)
      parameter         (one = 1.0d0, two = 2.0d0)
 
*---- Misalignment componentss.
      call ucopy(q(lcali+1), d, 6 * mwflt)
      the = d(5)
      phi = d(4)
      psi = d(6)
      call sumtrx(the, phi, psi, w)
      beti = one / betas
 
*---- Reference change from entrance to exit.
      if (iq(lcelm+mbpr) .eq. mplin) then
        call suline(ipos, el, arc, ve, we)
      else
        call suelem(el, arc, ve, we)
      endif
 
*---- Displacements at exit w.r.t. entrance system.
      d(1) = d(1) + w(1,1)*ve(1) + w(1,2)*ve(2) + w(1,3)*ve(3) - ve(1)
      d(2) = d(2) + w(2,1)*ve(1) + w(2,2)*ve(2) + w(2,3)*ve(3) - ve(2)
      d(3) = d(3) + w(3,1)*ve(1) + w(3,2)*ve(2) + w(3,3)*ve(3) - ve(3)
 
*---- Convert all references to exit and compute additional drift.
      call sutran(w, d, we)
      w33i = one / w(3,3)
      s2   = (w(1,3) * d(1) + w(2,3) * d(2) + w(3,3) * d(3)) * w33i
      s2bg = s2 / (betas*gammas)**2
 
*---- Loop for all particles.
      do 90 itrack = 1, ktrack
        pt1 = track(6,itrack)
        px1 = track(2,itrack) - w(3,1) * (one + pt1 * beti)
        py1 = track(4,itrack) - w(3,2) * (one + pt1 * beti)
        track(2,itrack) = (w(2,2) * px1 - w(2,1) * py1) * w33i
        track(4,itrack) = (w(1,1) * py1 - w(1,2) * px1) * w33i
 
        x1  = w(1,1) * track(1,itrack) + w(1,2) * track(3,itrack)
        y1  = w(2,1) * track(1,itrack) + w(2,2) * track(3,itrack)
        track(1,itrack) = x1 - s2 * track(2,itrack) + d(1)
        track(3,itrack) = y1 - s2 * track(4,itrack) + d(2)
        track(5,itrack) = track(5,itrack) + s2bg * pt1 -
     +    ((w(1,3) * x1 + w(2,3) * y1) / w(3,3) - d(3)) * beti
   90 continue
 
      end
