      subroutine hadtun(xix2, xiy2, xix3, xiy3)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Tune shift versus momentum (second and third derivatives).         *
*   This routine integrates the former routines CURVE, DOUBLE, TRIPLE. *
* Output:                                                              *
*   XIX2(2)   (real)    d**2(Qx)/d(delta)**2 contributions.            *
*   XIY2(2)   (real)    d**2(Qy)/d(delta)**2 contributions.            *
*   XIX3(3)   (real)    d**3(Qx)/d(delta)**3 contributions.            *
*   XIY3(3)   (real)    d**3(Qy)/d(delta)**3 contributions.            *
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
      integer ienum,iflag,iocc,ipos,irg
      double precision amux,amuy,angle,cqxn,cqyn,cx,cy,f11x,f11y,f1cx,
     +f1cy,f1sx,f1sy,f1x1,f1x2,f1y1,f1y2,fc1x,fc1y,fccx,fccy,fcsx,fcsy,
     +fcx1,fcx2,fcy1,fcy2,fs1x,fs1y,fscx,fscy,fssx,fssy,fsx1,fsx2,fsy1,
     +fsy2,half,pgamx1,pgamx2,pgamx3,pgamy1,pgamy2,pgamy3,pi02,pi04,
     +pi08,pi16,qxn,qyn,sdb2x,sdb2y,sdb3x,sdb3y,sixth,sk1l,sk2l,sk3l,
     +sk4l,sqxn,sqyn,ssg2x,ssg2y,ssg3x,ssg3y,str3x,str3y,sx,sy,two,xix2,
     +xix3,xiy2,xiy3
      dimension         xix2(2), xiy2(2), xix3(3), xiy3(3)
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
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (two = 2.0d0, half = .5d0, sixth = 1.d0/6.d0)
      parameter         (pi02 = 2.0d0 * pi, pi04 =  4.0d0 * pi)
      parameter         (pi08 = 8.0d0 * pi, pi16 = 16.0d0 * pi)
 
      character*(mcnam) elmnam
      logical           mirror
 
*---- Cosines and sines of (2*pi*Q/Nsup).
      qxn = pi02 * qx / nsup
      qyn = pi02 * qy / nsup
      cqxn = cos(qxn)
      sqxn = sin(qxn)
      cqyn = cos(qyn)
      sqyn = sin(qyn)
 
*---- Clear single integrals.
      ssg2x = 0.0
      ssg3x = 0.0
      ssg2y = 0.0
      ssg3y = 0.0
 
*---- Clear single sums.
      fcx1 = 0.0
      fsx1 = 0.0
      f1x1 = 0.0
      fcx2 = 0.0
      fsx2 = 0.0
      f1x2 = 0.0
      fcy1 = 0.0
      fsy1 = 0.0
      f1y1 = 0.0
      fcy2 = 0.0
      fsy2 = 0.0
      f1y2 = 0.0
 
*---- Clear double integrals.
      sdb3x = 0.0
      sdb3y = 0.0
 
*---- Clear double sums.
      fccx = 0.
      fcsx = 0.
      fc1x = 0.
      fscx = 0.
      fssx = 0.
      fs1x = 0.
      f1cx = 0.
      f1sx = 0.
      f11x = 0.
      fccy = 0.
      fcsy = 0.
      fc1y = 0.
      fscy = 0.
      fssy = 0.
      fs1y = 0.
      f1cy = 0.
      f1sy = 0.
      f11y = 0.
 
*---- Clear triple integrals.
      str3x = 0.
      str3y = 0.
 
      mirror = .false.
  110 continue
        do 190 irg = irg1, irg2
          if (mirror) then
            ipos = irg2 + irg1 - irg
          else
            ipos = irg
          endif
          call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
          if (iq(lcelm+mbpr) .eq. mpelm) then
            call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
            if (sk1l .ne. 0.0  .or.  sk2l .ne. 0.0  .or.
     +          sk3l .ne. 0.0  .or.  sk4l .ne. 0.0) then
              call tbset(lhastb, ipos, 1, lhasbf)
              call ucopy(q(lhasbf+1), bxb, iq(lhasbf-1))
 
*---- Strength coefficients.
              pgamx1 = bxb * (sk1l - sk2l * dxb)
              pgamy1 = byb * (sk1l - sk2l * dxb)
              pgamx2 = - pgamx1 - bxb * (sk2l*dxbp + half*sk3l*dxb**2)
              pgamy2 = - pgamy1 - byb * (sk2l*dxbp + half*sk3l*dxb**2)
              pgamx3 = - pgamx2 -
     +          bxb * (sk2l*dxbpp + sk3l*dxb*dxbp + sixth*sk4l*dxb**3)
              pgamy3 = - pgamy2 -
     +          byb * (sk2l*dxbpp + sk3l*dxb*dxbp + sixth*sk4l*dxb**3)
 
*---- Phase coefficients.
              if (mirror) then
                amux = two * (qxn - amuxb)
                amuy = two * (qyn - amuyb)
              else
                amux = two * amuxb
                amuy = two * amuyb
              endif
              cx = cos(amux)
              sx = sin(amux)
              cy = cos(amuy)
              sy = sin(amuy)
 
*---- Single integrals.
              ssg2x = ssg2x + pgamx2
              ssg2y = ssg2y + pgamy2
              ssg3x = ssg3x + pgamx3
              ssg3y = ssg3y + pgamy3
 
*---- Single sums.
              fcx1 = fcx1 + pgamx1 * cx
              fsx1 = fsx1 + pgamx1 * sx
              f1x1 = f1x1 + pgamx1
              fcx2 = fcx2 + pgamx2 * cx
              fsx2 = fsx2 + pgamx2 * sx
              f1x2 = f1x2 + pgamx2
 
              fcy1 = fcy1 + pgamy1 * cy
              fsy1 = fsy1 + pgamy1 * sy
              f1y1 = f1y1 + pgamy1
              fcy2 = fcy2 + pgamy2 * cy
              fsy2 = fsy2 + pgamy2 * sy
              f1y2 = f1y2 + pgamy2
 
*---- Double integrals.
              sdb3x = sdb3x +
     +                pgamx2 * (fcx1 * (cx * cqxn + sx * sqxn) +
     +                          fsx1 * (sx * cqxn - cx * sqxn) -
     +                          f1x1 * cqxn) +
     +                pgamx1 * (fcx2 * (cx * cqxn + sx * sqxn) +
     +                          fsx2 * (sx * cqxn - cx * sqxn) -
     +                          f1x2 * cqxn)
 
              sdb3y = sdb3y +
     +                pgamy2 * (fcy1 * (cy * cqyn + sy * sqyn) +
     +                          fsy1 * (sy * cqyn - cy * sqyn) -
     +                          f1y1 * cqyn) +
     +                pgamy1 * (fcy2 * (cy * cqyn + sy * sqyn) +
     +                          fsy2 * (sy * cqyn - cy * sqyn) -
     +                          f1y2 * cqyn)
 
*---- Double sums.
              fccx = fccx + pgamx1 * cx * fcx1
              fcsx = fcsx + pgamx1 * cx * fsx1
              fc1x = fc1x + pgamx1 * cx * f1x1
              fscx = fscx + pgamx1 * sx * fcx1
              fssx = fssx + pgamx1 * sx * fsx1
              fs1x = fs1x + pgamx1 * sx * f1x1
              f1cx = f1cx + pgamx1 * fcx1
              f1sx = f1sx + pgamx1 * fsx1
              f11x = f11x + pgamx1 * f1x1
 
              fccy = fccy + pgamy1 * cy * fcy1
              fcsy = fcsy + pgamy1 * cy * fsy1
              fc1y = fc1y + pgamy1 * cy * f1y1
              fscy = fscy + pgamy1 * sy * fcy1
              fssy = fssy + pgamy1 * sy * fsy1
              fs1y = fs1y + pgamy1 * sy * f1y1
              f1cy = f1cy + pgamy1 * fcy1
              f1sy = f1sy + pgamy1 * fsy1
              f11y = f11y + pgamy1 * f1y1
 
*---- Triple integrals.
              str3x = str3x + pgamx1 *
     +          (sqxn * (fccx + fssx) + cqxn * (fcsx - fscx) +
     +          (sqxn * cx - cqxn * sx) * (fc1x - f1cx) +
     +          (cqxn * cx + sqxn * sx) * (fs1x - f1sx) - sqxn * f11x)
 
              str3y = str3y + pgamy1 *
     +          (sqyn * (fccy + fssy) + cqyn * (fcsy - fscy) +
     +          (sqyn * cy - cqyn * sy) * (fc1y - f1cy) +
     +          (cqyn * cy + sqyn * sy) * (fs1y - f1sy) - sqyn * f11y)
            endif
          endif
  190   continue
        mirror = .not. mirror
      if (symm .and. mirror) go to 110
 
*---- Apply constant factors.
      sdb2x = cqxn * (fccx + fssx - f11x) + sqxn * (fscx - fcsx)
      sdb2y = cqyn * (fccy + fssy - f11y) + sqyn * (fscy - fcsy)
 
      xix2(1) = - nsup * ssg2x / pi04
      xix2(2) = - nsup * sdb2x / (pi08 * sqxn)
      xix3(1) = - nsup * ssg3x / pi04
      xix3(2) = - nsup * sdb3x / (pi08 * sqxn)
      xix3(3) = - nsup * str3x / (pi16 * sqxn)
 
      xiy2(1) =   nsup * ssg2y / pi04
      xiy2(2) = - nsup * sdb2y / (pi08 * sqyn)
      xiy3(1) =   nsup * ssg3y / pi04
      xiy3(2) = - nsup * sdb3y / (pi08 * sqyn)
      xiy3(3) =   nsup * str3y / (pi16 * sqyn)
 
      end
