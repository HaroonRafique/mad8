      subroutine hareso(iprint, dh1, dv1, dh2, dv2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Calculation of 1/3rd integer resonances due to sextupoles.         *
*   Penalty of the configuration is expressed as distortion of         *
*   The 4-dimensional phase space.                                     *
*   A = - n1 / sin(2*pi*Q*N) * integral over one superperiod of        *
*       G * beta**(k/2) * K2 * complex exponent of                     *
*       N*Q * (pi + phi - psi) * ds.                                   *
*   Horizontal distortion is:                                          *
*   (emittance)**(k2/2) / eh * A.                                      *
*   Vertical distortion is                                             *
*   (emittance)**(k2/2) / ev * A.                                      *
*     N1 = phase integer, horz or vert.                                *
*     N  = ditto          combined.                                    *
*     K  = amplitude integer, combined.                                *
* Input:                                                               *
*   IPRINT    (integer) Print flag.                                    *
* Output:                                                              *
*   DH1       (real)    Horizontal resonances at i.p.                  *
*   DV1       (real)    Vertical resonances   at i.p.                  *
*   DH2       (real)    Horizontal resonances at s.p.                  *
*   DV2       (real)    Vertical resonances   at s.p.                  *
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
      double precision ensige,ensigx,ensigy
 
*---- Communication area for HARMON module.
      common /harchr/   lngnam, shtnam
      common /harflt/   ensigx, ensigy, ensige
      save              /harchr/, /harflt/
      character*(mcnam) lngnam, shtnam
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
      integer i,ienum,iflag,iocc,ipos,iprint,nres
      double precision aa,angle,c2x,c2y,cb,cg,cmux,cmuy,dh1,dh2,disth1,
     +disth2,distv1,distv2,dv1,dv2,h,pbet,qxn,qy2n,s2x,s2y,sb,sg,sk1l,
     +sk2l,sk3l,sk4l,smux,smuy,src,srs,sum1,sum2,twopi,v,w1
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
 
*---- Local variables.
      parameter         (nres = 5)
      dimension         src(nres), srs(nres), pbet(nres),
     +                  cb(nres), sb(nres), aa(nres),
     +                  cg(nres), sg(nres), sum1(5), sum2(5),
     +                  disth1(nres), distv1(nres),
     +                  disth2(nres), distv2(nres)
      character*(mcnam) elmnam
*     SRC, SRS          Accumulation of cos & sin components of sums.
*     PBET              K2 * beta**(k/2).
*     CB, SB            Sin & cos of (n*pi*Q).
*     AA                1 / (2 * sin(n*pi*Q)).
*     CG, SG            cos and sin(n*Q*psi).
*     SUM1, SUM2        Summation at I.P. and symmmetry point.
*     DISTH1, DISTV1    Distortion horz/vert at I.P.
*     DISTH2, DISTV2    Ditto at symmetry point.
 
*---- Clear the sums.
      do 10 i = 1, nres
        src(i) = 0.0
        srs(i) = 0.0
   10 continue
 
*---- Accumulate the integrals.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        if (iq(lcelm+mbpr) .eq. mpelm) then
          call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
          if (sk2l .ne. 0.0) then
            call tbset(lhastb, ipos, 1, lhasbf)
            call ucopy(q(lhasbf+1), bxb, iq(lhasbf-1))
            pbet(1) = sk2l * sqrt(bxb) * bxb
            pbet(2) = pbet(1)
            pbet(3) = sk2l * sqrt(bxb) * byb
            pbet(4) = pbet(3)
            pbet(5) = pbet(3)
            cmux = cos(amuxb)
            smux = sin(amuxb)
            cmuy = cos(amuyb)
            smuy = sin(amuyb)
            c2x = cmux * cmux - smux * smux
            s2x = smux * cmux + cmux * smux
            c2y = cmuy * cmuy - smuy * smuy
            s2y = smuy * cmuy + cmuy * smuy
            cg(1) = cmux * c2x - smux * s2x
            cg(2) = cmux
            cg(3) = cmux * c2y - smux * s2y
            cg(4) = cmux * c2y + smux * s2y
            cg(5) = cg(2)
            sg(1) = smux * c2x + cmux * s2x
            sg(2) = smux
            sg(3) = smux * c2y + cmux * s2y
            sg(4) = smux * c2y - cmux * s2y
            sg(5) = sg(2)
            do 20 i = 1, nres
              src(i) = src(i) + pbet(i) * cg(i)
              srs(i) = srs(i) + pbet(i) * sg(i)
   20       continue
          endif
        endif
   90 continue
 
*---- Set up (n*pi*Q).
      qxn = pi * qx / nsup
      qy2n = twopi * qy / nsup
      aa(1) = 3.0 * qxn
      aa(2) = qxn
      aa(3) = qxn + qy2n
      aa(4) = qxn - qy2n
      aa(5) = qxn
      if (symm) then
        w1 = - 1.0
      else
        w1 = - 0.5
      endif
      do 110 i = 1, nres
        cb(i) = cos(aa(i))
        sb(i) = sin(aa(i))
        aa(i) = w1 / sb(i)
  110 continue
      aa(1) = aa(1) / (4.0 * 6.0)
      aa(2) = aa(2) / (4.0 * 2.0)
      aa(3) = aa(3) / (4.0 * 2.0)
      aa(4) = aa(4) / (4.0 * 2.0)
      aa(5) = aa(5) / 4.0
 
*---- Get the functions by using the sums.
      do 120 i = 1, nres
        sum1(i) = (src(i) * cb(i) + srs(i) * sb(i)) * aa(i)
        if (symm) then
          sum2(i) = src(i) * aa(i)
        else
          sum2(i) = sum1(i)
        endif
  120 continue
 
*---- Actual distortion.
      h = sqrt(ex) * ensigx
      v = ey * ensigy**2 / h
      disth1(1) = h * sum1(1) * 3.0
      disth1(2) = h * sum1(2)
      disth1(3) = v * sum1(3)
      disth1(4) = v * sum1(4)
      disth1(5) = v * sum1(5)
      distv1(1) = 0.0
      distv1(2) = 0.0
      distv1(3) = h * sum1(3) * 2.0
      distv1(4) = h * sum1(4) * 2.0
      distv1(5) = 0.0
      disth2(1) = h * sum2(1) * 3.0
      disth2(2) = h * sum2(2)
      disth2(3) = v * sum2(3)
      disth2(4) = v * sum2(4)
      disth2(5) = v * sum2(5)
      distv2(1) = 0.0
      distv2(2) = 0.0
      distv2(3) = h * sum2(3) * 2.0
      distv2(4) = h * sum2(4) * 2.0
      distv2(5) = 0.0
 
*---- Sum up.
      dh1 = sqrt(disth1(1)**2 + disth1(2)**2 + disth1(3)**2 +
     +           disth1(4)**2 + disth1(5)**2)
      dh2 = sqrt(disth2(1)**2 + disth2(2)**2 + disth2(3)**2 +
     +           disth2(4)**2 + disth2(5)**2)
      dv1 = sqrt(distv1(1)**2 + distv1(2)**2 + distv1(3)**2 +
     +           distv1(4)**2 + distv1(5)**2)
      dv2 = sqrt(distv2(1)**2 + distv2(2)**2 + distv2(3)**2 +
     +           distv2(4)**2 + distv2(5)**2)
 
*---- Output.
      if (iprint .gt. 0) then
        write (iqpr2, 910)
        write (iqpr2, 930)
        write (iqpr2, 940) (sum1(i), i = 1, nres)
        write (iqpr2, 950) (disth1(i), i = 1, nres)
        write (iqpr2, 960) (distv1(i), i = 1, nres)
        if (symm) then
          write (iqpr2, 920)
          write (iqpr2, 930)
          write (iqpr2, 940) (sum2(i), i = 1, nres)
          write (iqpr2, 950) (disth2(i), i = 1, nres)
          write (iqpr2, 960) (distv2(i), i = 1, nres)
        endif
      endif
 
  910 format(' '/' '/' Resonance coefficients at interaction point:')
  920 format(' '/' Resonance coefficients at symmetry point:')
  930 format(' resonances',t38,'3000',12x,'2100',12x,'1020',12x,
     +                         '1002',12x,'1011')
  940 format(' coefficients',t26,1p,5e16.6)
  950 format(' horizontal  ',t26,1p,5e16.6)
  960 format(' vertical    ',t26,1p,5e16.6)
 
      end
