      subroutine lmdsp2(ipos, nord, d, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a misalignment at exit.                      *
* Input:                                                               *
*   IPOS      (integer) Position in beam line.                         *
*   NORD      (integer) Order desired.                                 *
*   D(6)      (real)    Components of misalignment.                    *
* Output:                                                              *
*   FP, FM    (map)     Map representing displacement.                 *
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
      integer ipos,nord
      double precision arc,d,el,fm,fp,half,phi,psi,r,s2,the,two,ve,w,we
      dimension         d(6), fp(*), fm(6,6)
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      dimension         r(3), ve(3), w(3,3), we(3,3)
      parameter         (two = 2.0d0, half = 0.5d0)
 
*---- Rotation w.r.t. entrance system.
      the = d(5)
      phi = d(4)
      psi = d(6)
      call sumtrx(the, phi, psi, w)
 
*---- Translation from entrance to exit w.r.t. entrance system.
      if (iq(lcelm+mbpr) .eq. mplin) then
        call suline(ipos, el, arc, ve, we)
      else
        call suelem(el, arc, ve, we)
      endif
      r(1) = d(1)+w(1,1)*ve(1)+w(1,2)*ve(2)+w(1,3)*ve(3)-ve(1)
      r(2) = d(2)+w(2,1)*ve(1)+w(2,2)*ve(2)+w(2,3)*ve(3)-ve(2)
      r(3) = d(3)+w(3,1)*ve(1)+w(3,2)*ve(2)+w(3,3)*ve(3)-ve(3)
 
*---- Convert all references to exit.
      call sutran(w, r, we)
 
*---- Build additional drift.
      s2 = - (w(1,3) * r(1) + w(2,3) * r(2) + w(3,3) * r(3)) / w(3,3)
 
*---- Rotation:
      call lmone(nord, fp(1), fm)
 
*---- F1 terms (kicks).
      fp(1) = - w(3,1)
      fp(2) = - (w(2,2) * r(1) - w(1,2) * r(2)) / w(3,3)
      fp(3) = - w(3,2)
      fp(4) = - (w(1,1) * r(2) - w(2,1) * r(1)) / w(3,3)
      fp(5) = 0.0
      fp(6) = s2 / betas
 
*---- F2 terms (transfer matrix).
      fm(1,1) = w(1,1)
      fm(3,1) = w(2,1)
      fm(5,1) = w(3,1) / betas
      fm(1,3) = w(1,2)
      fm(3,3) = w(2,2)
      fm(5,3) = w(3,2) / betas
 
      fm(2,2) =   w(2,2) / w(3,3)
      fm(1,2) = fm(2,2) * s2
      fm(4,2) = - w(1,2) / w(3,3)
      fm(3,2) = fm(4,2) * s2
      fm(2,4) = - w(2,1) / w(3,3)
      fm(1,4) = fm(2,4) * s2
      fm(4,4) =   w(1,1) / w(3,3)
      fm(3,4) = fm(4,4) * s2
      fm(2,6) = w(1,3) / (w(3,3) * betas)
      fm(1,6) = fm(2,6) * s2
      fm(4,6) = w(2,3) / (w(3,3) * betas)
      fm(3,6) = fm(4,6) * s2
      fm(5,6) = - s2 / (betas * gammas)**2
 
*---- F3 and F4 terms are ignored.
 
      end
