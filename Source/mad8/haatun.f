      subroutine haatun(iprint, dqxdex, dqydey, dqydex)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute tune shift with amplitude.                                 *
* Input:                                                               *
*   IPRINT    (integer) Print flag.                                    *
* Output:                                                              *
*   DQXDEX    (real)    d(Qx)/d(Ex).                                   *
*   DQYDEY    (real)    d(Qy)/d(Ey).                                   *
*   DQYDEX    (real)    d(Qy)/d(Ex).                                   *
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
      integer ienum,iflag,iocc,ipos,iprint,irg
      double precision amux,amuy,angle,c10,c1m2,c1p2,c30,cg01,cg02,cg10,
     +cg1m2,cg1p2,cg20,cg30,dqxdex,dqydex,dqydey,f1a,f2a,f3a,f4a,f5a,
     +f6a,fc1,fc2,fc3,fc4,fc5,fs1,fs2,fs3,fs4,fs5,pgamh,pgamv,q02,q10,
     +q20,q30,s10,s1m2,s1p2,s30,sg01,sg02,sg10,sg1m2,sg1p2,sg20,sg30,
     +sk1l,sk2l,sk3l,sk4l,temp,twopi,v0120,v1002,v1011,v1020,v2100,
     +v21111,v3000,x1,x2,x3
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
      character*(mcnam) elmnam
      logical           mirror
 
      q10 = pi * qx / nsup
      q20 = 2.0 * q10
      q30 = 3.0 * q10
      q02 = twopi * qy / nsup
      c10 = cos(q10)
      s10 = sin(q10)
      c30 = cos(q30)
      s30 = sin(q30)
      c1p2 = cos(q10+q02)
      s1p2 = sin(q10+q02)
      c1m2 = cos(q10-q02)
      s1m2 = sin(q10-q02)
 
*---- Clear single sums.
      fc1 = 0.0
      fc2 = 0.0
      fc3 = 0.0
      fc4 = 0.0
      fc5 = 0.0
      fs1 = 0.0
      fs2 = 0.0
      fs3 = 0.0
      fs4 = 0.0
      fs5 = 0.0
 
*---- Clear double sums.
      f1a = 0.0
      f2a = 0.0
      f3a = 0.0
      f4a = 0.0
      f5a = 0.0
      f6a = 0.0
 
      mirror = .false.
   10 continue
        do 90 irg = irg1, irg2
          if (mirror) then
            ipos = irg2 + irg1 - irg
          else
            ipos = irg
          endif
          call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
          if (iq(lcelm+mbpr) .eq. mpelm) then
            call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
            if (sk2l .ne. 0.0) then
              call tbset(lhastb, ipos, 1, lhasbf)
              call ucopy(q(lhasbf+1), bxb, iq(lhasbf-1))
              temp  = sqrt(bxb) * sk2l
              pgamh = temp * bxb
              pgamv = temp * byb
              if (mirror) then
                amux = q20 - amuxb
                amuy = q02 - amuyb
              else
                amux = amuxb
                amuy = amuyb
              endif
 
              cg10 = cos(amux)
              sg10 = sin(amux)
              cg01 = cos(amuy)
              sg01 = sin(amuy)
              cg20 = cg10 * cg10 - sg10 * sg10
              sg20 = 2.0 * cg10 * sg10
              cg02 = cg01 * cg01 - sg01 * sg01
              sg02 = 2.0 * cg01 * sg01
              cg30 = cg10 * cg20 - sg10 * sg20
              sg30 = sg10 * cg20 + cg10 * sg20
              cg1p2 = cg10 * cg02 - sg10 * sg02
              sg1p2 = sg10 * cg02 + cg10 * sg02
              cg1m2 = cg10 * cg02 + sg10 * sg02
              sg1m2 = sg10 * cg02 - cg10 * sg02
 
*---- Accumulate single sums.
              fc1 = fc1 + pgamh * cg30
              fs1 = fs1 + pgamh * sg30
              fc2 = fc2 + pgamh * cg10
              fs2 = fs2 + pgamh * sg10
              fc3 = fc3 + pgamv * cg1p2
              fs3 = fs3 + pgamv * sg1p2
              fc4 = fc4 + pgamv * cg1m2
              fs4 = fs4 + pgamv * sg1m2
              fc5 = fc5 + pgamv * cg10
              fs5 = fs5 + pgamv * sg10
 
*---- Accumulate double sums.
              f1a = f1a + pgamh * ((c30 * cg30 + s30 * sg30) * fc1 -
     +                             (s30 * cg30 - c30 * sg30) * fs1 -
     +                             c30 * pgamh / 2.0)
              f2a = f2a + pgamh * ((c10 * cg10 + s10 * sg10) * fc2 -
     +                             (s10 * cg10 - c10 * sg10) * fs2 -
     +                             c10 * pgamh / 2.0)
              f3a = f3a + pgamv * ((c1p2 * cg1p2 + s1p2 * sg1p2) * fc3 -
     +                             (s1p2 * cg1p2 - c1p2 * sg1p2) * fs3 -
     +                             c1p2 * pgamv / 2.0)
              f4a = f4a + pgamv * ((c1m2 * cg1m2 + s1m2 * sg1m2) * fc4 -
     +                             (s1m2 * cg1m2 - c1m2 * sg1m2) * fs4 -
     +                             c1m2 * pgamv / 2.0)
              f5a = f5a + pgamv * ((c10 * cg10 + s10 * sg10) * fc5 -
     +                             (s10 * cg10 - c10 * sg10) * fs5 -
     +                             c10 * pgamv / 2.0)
              f6a = f6a + pgamv * ((c10 * cg10 + s10 * sg10) * fc2 -
     +                             (s10 * cg10 - c10 * sg10) * fs2)
     +                  + pgamh * ((c10 * cg10 + s10 * sg10) * fc5 -
     +                             (s10 * cg10 - c10 * sg10) * fs5 -
     +                             c10 * pgamv)
            endif
          endif
   90   continue
        mirror = .not. mirror
      if (symm .and. mirror) go to 10
 
      x1 = nsup / (2304.0 * pi)
      x2 = 9.0 * x1
      x3 = 4.0 * x2
      f1a = f1a * x1 / s30
      f2a = f2a * x2 / s10
      f3a = f3a * x2 / s1p2
      f4a = f4a * x2 / s1m2
      f5a = f5a * x3 / s10
      f6a = f6a * x3 / s10
      dqxdex = 4.0 * (- 9.0 * f1a - 3.0 * f2a)
      dqydey = 4.0 * (- f3a - f4a - f5a)
      dqydex = 2.0 * (- 4.0 * f3a + 4.0 * f4a + f6a)
      if (iprint .gt. 0) then
        write (iqpr2, 910)
        if (iprint .gt. 1) then
          v3000 = - 36.0 * f1a
          v2100 = - 12.0 * f2a
          v1020 = -  4.0 * f3a
          v1002 = -  4.0 * f4a
          v1011 = -  4.0 * f5a
          v0120 =    4.0 * f4a
          v21111 = f6a
          write (iqpr2, 920) v3000,  v1020,  v1020,
     +                       v2100,  v1002,  v0120,
     +                               v1011,  v21111,
     +                       dqxdex, dqydey, dqydex
        else
          write (iqpr2, 930) dqxdex, dqydey, dqydex
        endif
      endif
 
  910 format(' '/' Tune shift with amplitude:'/
     +       6x,'d(Qx)/d(Ex)',5x,'d(Qy)/(dEy)',5x,'d(Qy)/d(Ex)')
  920 format(' ',1p,3e16.6,' (V3000, V1020, V1020)'/
     +       ' ',1p,3e16.6,' (V2100, V1002, V0120)'/
     +       17x,1p,2e16.6,' (V1011, V2100-01111)'/
     +       ' ',1p,3e16.6,' (total)'/' ')
  930 format(' ',1p,3e16.6/' ')
 
      end
