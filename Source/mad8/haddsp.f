      subroutine haddsp(iprint, dxp1, dxpp1, dxp2, dxpp2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*             d(D)            d**2(D)                                  *
*   Compute ---------- and ------------- over the full HARMON table.   *
*            d(delta)       d(delta)**2                                *
* Input:                                                               *
*   IPRINT    (integer) Print flag.                                    *
* Output:                                                              *
*   DXP1      (real)    d(Dx)/d(delta) at i.p.                         *
*   DXPP1     (real)    d**2(Dx)/d(delta**2) at i.p.                   *
*   DXP2      (real)    d(Dx)/d(delta) at s.p.                         *
*   DXPP2     (real)    d**2(Dx)/d(delta**2) at s.p.                   *
*   DXPMX     (real)    d(Dx)/d(delta) (maximum).                      *
*   DXPPMX    (real)    d**2(Dx)/d(delta**2) (maximum).                *
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
      double precision amux1,amux2,amuy1,amuy2,ax1,ax2,ay1,ay2,bx1,bx2,
     +by1,by2,ct1,ct2,delta1,delta2,dpx1,dpx2,dpy1,dpy2,dx1,dx2,dy1,dy2,
     +px1,px2,py1,py2,s1,s2,x1,x2,y1,y2
 
*---- Buffer for "long" HARMON table: Values at both ends of an element.
      common /halbuf/   bx1, ax1, amux1, by1, ay1, amuy1,
     +                  x1, px1, y1, py1, ct1, delta1,
     +                  dx1, dpx1, dy1, dpy1, s1,
     +                  bx2, ax2, amux2, by2, ay2, amuy2,
     +                  x2, px2, y2, py2, ct2, delta2,
     +                  dx2, dpx2, dy2, dpy2, s2
      save   /halbuf/
      integer lhalbf,lhaltb,lhasbf,lhastb,mlcol,mscol
 
*---- Link area for HARMON module.
      parameter         (mlcol = 17, mscol = 12)
      common /halink/   lhaltb, lhalbf, lhastb, lhasbf
      save              /halink/
      double precision amuxb,amuyb,bxb,bxbp,byb,bybp,dxb,dxbp,dxbpp,dyb,
     +dybp,dybpp
 
*---- Buffer for "short" HARMON table: Values averaged over an element.
      common /hasbuf/   bxb, dxb, amuxb, byb, dyb, amuyb,
     +                  bxbp, dxbp, dxbpp, bybp, dybp, dybpp
      save   /hasbuf/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer ideriv,ienum,iflag,iocc,ipos,ipos1,iprint
      double precision angle,cmux,cqxn,cqxnh,csum,csumh,dxp1,dxp2,dxpmx,
     +dxpp1,dxpp2,dxppmx,fact,half,qxnh,sixth,sk1l,sk2l,sk3l,sk4l,smux,
     +sqxn,sqxnh,ssum,ssumh,term1,term2,two
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (two = 2.d0, half = .5d0, sixth = 1.d0/6.d0)
      character         elmnam*(mcnam)
 
*---- Fetch values at ends of system.
      ipos1 = max(irg1-1,1)
      call tbset(lhaltb, ipos1, 1, lhalbf)
      call ucopy(q(lhalbf+1), bx1, iq(lhalbf-1))
      call tbset(lhaltb, irg2, 1, lhalbf)
      call ucopy(q(lhalbf+1), bx2, iq(lhalbf-1))
 
*---- Initialize.
      qxnh = pi * qx / nsup
      cqxnh = cos(qxnh)
      sqxnh = sin(qxnh)
      cqxn = cqxnh * cqxnh - sqxnh * sqxnh
      sqxn = two * cqxnh * sqxnh
      dxpmx = 0.0
      dxppmx = 0.0
 
*---- Loop for first and second derivatives.
      do 100 ideriv = 1, 2
 
*---- Accumulate integrals over (half) superperiod.
        csum = 0.0
        ssum = 0.0
        do 40 ipos = irg1, irg2
          call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
          if (iq(lcelm+mbpr) .eq. mpelm) then
            call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
            if (sk1l .ne. 0.0  .or.  sk2l .ne. 0.0  .or.
     +          sk3l .ne. 0.0) then
              call tbset(lhastb, ipos, 1, lhasbf)
              call ucopy(q(lhasbf+1), bxb, iq(lhasbf-1))
              if (ideriv .eq. 1) then
                fact = (sk1l - half * sk2l * dxb) * dxb * sqrt(bxb)
              else
                fact = ((sk1l - sk2l * dxb) * dxbp -
     +            sixth * sk3l * dxb**3) * sqrt(bxb)
              endif
              csum = csum + fact * cos(amuxb)
              ssum = ssum + fact * sin(amuxb)
            endif
          endif
   40   continue
 
*---- Symmetric lattice.
        if (symm) then
          term1 = (csum * cqxnh + ssum * sqxnh) * sqrt(bx1) / sqxnh
          term2 = csum * sqrt(bx2) / sqxnh
          if (ideriv .eq. 1) then
            dxp1 = term1 - dx1
            dxp2 = term2 - dx2
          else
            dxpp1 = term1 - dxp1
            dxpp2 = term2 - dxp2
          endif
 
*---- Asymmetric lattice.
        else
          term1 = (csum * cqxnh + ssum * sqxnh) * sqrt(bx1) /
     +            (two * sqxnh)
          if (ideriv .eq. 1) then
            dxp1 = term1 - dx1
            dxp2 = dxp1
          else
            dxpp1 = term1 - dxp1
            dxpp2 = dxpp1
          endif
        endif
 
*---- Step around the ring.
        csumh = csum
        ssumh = ssum
 
        do 90 ipos = irg1, irg2
          call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
          if (iq(lcelm+mbpr) .eq. mpelm) then
            call tbset(lhastb, ipos, 2, lhasbf)
            call ucopy(q(lhasbf+1), bxb, iq(lhasbf-1))
 
*---- Advance phase four accumulated sum.
            cmux = cos(amuxb)
            smux = sin(amuxb)
            term1 = (cqxnh * cmux - sqxnh * smux) * csum
     +            + (sqxnh * cmux + cqxnh * smux) * ssum
            if (symm) then
              term1 = (cqxnh * cmux + sqxnh * smux) * csumh
     +              + (sqxnh * cmux - cqxnh * smux) * ssumh + term1
            endif
 
*---- Store values for this element.
            term1 = term1 * sqrt(bxb) / (two * sqxnh)
            if (ideriv .eq. 1) then
              dxbp = term1 - dxb
              if (abs(dxbp) .gt. dxpmx) dxpmx = abs(dxbp)
            else
              dxbpp = term1 - dxbp
              if (abs(dxbpp) .gt. dxppmx) dxppmx = abs(dxbpp)
            endif
            call ucopy(bxb, q(lhasbf+1), iq(lhasbf-1))
 
*---- Advance over this element.
            call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
            if (ideriv .eq. 1) then
              fact = (sk1l - half * sk2l * dxb) * dxb * sqrt(bxb)
            else
              fact = ((sk1l - sk2l * dxb) * dxbp -
     +          sixth * sk3l * dxb**3) * sqrt(bxb)
            endif
            csum = csum + fact * (cmux*cqxn - smux*sqxn - cmux)
            ssum = ssum + fact * (smux*cqxn + cmux*sqxn - smux)
          endif
   90   continue
  100 continue
 
*---- Optional print.
      if (iprint .gt. 0) then
        write (iqpr2, 910)
        write (iqpr2, 920) dx1, dxp1, dxpp1
        if (symm) write (iqpr2, 930) dx2, dxp2, dxpp2
        write (iqpr2, 940) dxpmx, dxppmx
      endif
 
  910 format(' '/' Horizontal dispersion:',
     +       t42,'first',t57,'second',t74,'third')
  920 format(' at interaction point',t31,1p,3e16.6)
  930 format(' at symmetry point',t31,1p,3e16.6)
  940 format(' maximum',t47,1p,2e16.6/' ')
 
      end
