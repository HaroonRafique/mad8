      subroutine ha4sum(jp, kp, lp, mp, jpp, kpp, lpp, mpp, ip, sm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Evaluate quarter integer resonances for use in HA4ANA.             *
* Input:                                                               *
*   JP,KP,LP,MP         Indices for first factor.                      *
*   JPP,KPP,LPP,MPP     Indices for second factor.                     *
*   IP                  Value of p in n1*Qx + n2*Qy = p.               *
* Output:                                                              *
*   SM(2)     (real)    Computed resonance integrals.                  *
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
      integer ienum,iflag,iocc,ip,ipos,jp,jpp,kp,kpp,lp,lpp,mp,mpp,n1p,
     +n1pp,n2p,n2pp,na1p,na1pp,na2p,na2pp,np,npp
      double precision a1,a2,amuxb,amuyb,angle,b1,b2,betap,betapp,ccp,
     +ccpp,cospiq,el,factor,one,phip,phipp,scp,scpp,sinpiq,sk1l,sk2l,
     +sk3l,sk4l,sm,ssp,sspp,sum1,sum11,sum12,sum2,sum21,sum22,sup,tcp,
     +tcpp,thetan,tsp,tspp,tune,twelve,two,twopi
      dimension         sm(2)
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
      parameter         (one = 1.0d0, two = 2.0d0, twelve = 12.0d0)
      parameter         (twopi = two * pi)
 
      character*(mcnam) elmnam
 
      na1p   = jp + kp
      na2p   = lp + mp
      np     = na1p + na2p
      na1pp  = jpp + kpp
      na2pp  = lpp + mpp
      npp    = na1pp + na2pp
      n1p    = jp - kp
      n2p    = lp - mp
      n1pp   = jpp - kpp
      n2pp   = lpp - mpp
      sup    = nsup
      ccp    = sup * (- one) ** ((na2p + 1) / 2) / (two**np * pi *
     +         factor(jp) * factor(kp) * factor(lp) * factor(mp))
      ccpp   = sup * (- one) ** ((na2pp + 1) / 2) / (two**npp * pi *
     +         factor(jpp) * factor(kpp) * factor(lpp) * factor(mpp))
 
      tune = (n1p * qx + n2p * qy) / sup
      cospiq = cos(pi * tune)
      sinpiq = sin(pi * tune)
 
*---- Clear simple sums.
      scp = 0.0
      ssp = 0.0
      scpp = 0.0
      sspp = 0.0
 
*---- Clear double sums.
      sum1 = 0.0
      sum2 = 0.0
      sum11 = 0.0
      sum12 = 0.0
      sum21 = 0.0
      sum22 = 0.0
 
*---- Loop for all elements.
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
 
*---- Averaged arc length.
            el = s2 - s1
            thetan = sup * pi * (s2 + s1) / circ
 
            b1 = sqrt(bx1)**na1p * sqrt(by1)**na2p
            b2 = sqrt(bx2)**na1p * sqrt(by2)**na2p
            a1 = na1p * ax1 / bx1 + na2p * ay1 / by1
            a2 = na1p * ax2 / bx2 + na2p * ay2 / by2
            betap = sk2l * ((b2 + b1) / two + el * (a2 - a1) / twelve)
 
            b1 = sqrt(bx1)**na1pp * sqrt(by1)**na2pp
            b2 = sqrt(bx2)**na1pp * sqrt(by2)**na2pp
            a1 = na1pp * ax1 / bx1 + na2pp * ay1 / by1
            a2 = na1pp * ax2 / bx2 + na2pp * ay2 / by2
            betapp = sk2l * ((b2 + b1) / two + el * (a2 - a1) / twelve)
 
*---- Averaged phase functions.
            amuxb = (amux2 + amux1)/two - el*(one/bx2 - one/bx1)/twelve
            amuyb = (amuy2 + amuy1)/two - el*(one/by2 - one/by1)/twelve
            phip = n1p * (amuxb - qx * thetan / sup)
     +           + n2p * (amuyb - qy * thetan / sup)
     +           + tune * thetan
            phipp = n1pp * (amuxb - qx * thetan / sup)
     +            + n2pp * (amuyb - qy * thetan / sup)
     +            + (ip - tune) * thetan
 
*---- Terms to be integrated.
            tcp = betap * cos(phip)
            tsp = betap * sin(phip)
            tcpp = betapp * cos(phipp)
            tspp = betapp * sin(phipp)
 
*---- Machine is symmetric.
            if (symm) then
              sum1 = sum1 + tcp  * (tcpp + two * scpp)
     +                    + tcpp * (tcp  + two * scp)
              sum2 = sum2 + tsp  * (tcpp + two * scpp)
     +                    - tspp * (tcp  + two * scp)
 
*---- Machine is asymmetric.
            else
              sum1 = sum1 + (tcp * tcpp - tsp * tspp)
              sum2 = sum2 + (tsp * tcpp + tcp * tspp)
              sum11 = sum11 + (tcp * scpp - tsp * sspp)
              sum12 = sum12 + (tcpp * scp - tspp * ssp)
              sum21 = sum21 + (tsp * scpp + tcp * sspp)
              sum22 = sum22 + (tspp * scp + tcpp * ssp)
            endif
 
*---- Accumulate partial sums for next pass (sum from 1 to i-1).
            ssp  = ssp  + tsp
            sspp = sspp + tspp
            scp  = scp  + tcp
            scpp = scpp + tcpp
          endif
        endif
   90 continue
 
      if (symm) then
        sm(1) = (twopi / (sup * sinpiq)) * ccp * ccpp *
     +          (cospiq * sum1 + sinpiq * sum2)
        sm(2) = 0.0
      else
        sm(1) = (pi / (sup * sinpiq)) * ccp * ccpp *
     +    (cospiq * (sum1 + sum11 + sum12) + sinpiq * (sum21 - sum22))
        sm(2) = (pi / (sup * sinpiq)) * ccp * ccpp *
     +    (cospiq * (sum2 + sum22 + sum21) + sinpiq * (sum12 - sum11))
      endif
 
      end
