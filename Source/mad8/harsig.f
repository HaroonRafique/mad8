      subroutine harsig(acode, n1, n2, np, sum, rsum)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Integrate resonance functions along all specified sextupoles.      *
* Input:                                                               *
*   ACODE     (logical) True: Include dispersion.                      *
*   CLASS     (char)    Name of sextupole class.                       *
*   N1,N2,NP  (integer  Resonance integers:                            *
*                       N1 * QX + N2 * QY = NP.                        *
* Output:                                                              *
*   SUM(2)    (real)    Cosine and sine terms of resonance integral.   *
*   RSUM      (real)    Random terms of resonance integral.            *
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
      integer ienum,iflag,iocc,ipos,n1,n2,na1,na2,np
      double precision a1,a2,angle,ans1,ans2,b1,b2,el,fb1,fb2,fn1,fn2,
     +fnp,fs,rsum,sk1l,sk2l,sk3l,sk4l,sum,th1,th2,twelve,two,twopi
      logical           acode
      dimension         sum(2)
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
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
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
      parameter         (two = 2.0d0, twelve = 12.0, twopi = two * pi)
 
      character*(mcnam) elmnam
 
*---- Loop for all elements.
      sum(1) = 0.0
      sum(2) = 0.0
      rsum = 0.0
      fn1 = n1
      fn2 = n2
      fnp = np
      na1 = abs(n1)
      na2 = abs(n2)
 
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        if (iq(lcelm+mbpr) .eq. mpelm) then
 
*---- Find sextupole strength.
          call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
          if (sk2l .ne. 0.0) then
 
*---- Fetch lattice functions at both ends.
            call tbset(lhaltb, ipos - 1, 1, lhalbf)
            call ucopy(q(lhalbf+1), bx1, iq(lhalbf-1))
            call tbset(lhaltb, ipos, 1, lhalbf)
            call ucopy(q(lhalbf+1), bx2, iq(lhalbf-1))
 
*---- If DISP has not been specified, change dispersion to 1.
            if (.not. acode) then
              dx1 = 1.0
              dx2 = 1.0
              dpx1 = 0.0
              dpx2 = 0.0
            endif
 
*---- Integrate over long element.
            el = s2 - s1
            fb1 = sk2l * sqrt(bx1)**na1 * sqrt(by1)**na2
            fb2 = sk2l * sqrt(bx2)**na1 * sqrt(by2)**na2
            fs = twopi * (fn1*qx + fn2*qy - fnp) / circ
            th1 = fn1*amux1 + fn2*amuy1 - fs*s1
            th2 = fn1*amux2 + fn2*amuy2 - fs*s2
            a1 = dx1 * (fn1/bx1 + fn2/by1 - fs) * el / twelve
            a2 = dx2 * (fn1/bx2 + fn2/by2 - fs) * el / twelve
            b1 = dx1/two + (el/twelve) *
     +           (dpx1 - dx1 * (fn1*ax1/bx1 + fn2*ay1/by1))
            b2 = dx2/two - (el/twelve) *
     +           (dpx2 - dx2 * (fn1*ax2/bx2 + fn2*ay2/by2))
            ans1 = fb1 * (b1*cos(th1) - a1*sin(th1))
     +           + fb2 * (b2*cos(th2) + a2*sin(th2))
            ans2 = fb1 * (b1*sin(th1) + a1*cos(th1))
     +           + fb2 * (b2*sin(th2) - a2*cos(th2))
 
*---- Sum up.
            sum(1) = sum(1) + ans1
            sum(2) = sum(2) + ans2
            rsum = rsum + ans1**2 + ans2**2
          endif
        endif
   90 continue
 
*---- Apply constant coefficients.
      if (symm) then
        sum(1) = nsup * sum(1) / pi
        sum(2) = 0.0
        rsum = sqrt(two * nsup * rsum) / pi
      else
        sum(1) = nsup * sum(1) / twopi
        sum(2) = nsup * sum(2) / twopi
        rsum = sqrt(nsup * rsum) / pi
      endif
 
      end