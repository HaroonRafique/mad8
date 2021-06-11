      subroutine hadbet(iprint, dbx1, dby1, dbx2, dby2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*            1       d(beta)                                           *
*  Compute ------ * ---------- over the full HARMON table.             *
*           beta     d(delta)                                          *
* Input:                                                               *
*   IPRINT    (integer) Print flag.                                    *
* Output:                                                              *
*   DBX1      (real)    d(betax)/d(delta) at i.p.                      *
*   DBY1      (real)    d(betay)/d(delta) at i.p.                      *
*   DBX2      (real)    d(betax)/d(delta) at s.p.                      *
*   DBY2      (real)    d(betay)/d(delta) at s.p.                      *
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
      integer ienum,iflag,iocc,ipos,ipos1,iprint
      double precision angle,c2qxn,c2qyn,cmux,cmuy,cqxn,cqyn,csumx,
     +csumxh,csumy,csumyh,dbx1,dbx2,dbxmx,dby1,dby2,dbymx,factx,facty,
     +qxn,qyn,s2qxn,s2qyn,sk1l,sk2l,sk3l,sk4l,smux,smuy,sqxn,sqyn,ssumx,
     +ssumxh,ssumy,ssumyh,termx,termy,two,twopi
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (two = 2.0d0, twopi = two * pi)
      character         elmnam*(mcnam)
 
*---- Fetch values at ends of system.
      ipos1 = max(irg1-1,1)
      call tbset(lhaltb, ipos1, 1, lhalbf)
      call ucopy(q(lhalbf+1), bx1, iq(lhalbf-1))
      call tbset(lhaltb, irg2, 1, lhalbf)
      call ucopy(q(lhalbf+1), bx2, iq(lhalbf-1))
 
*---- Initialize.
      qxn = twopi * qx / nsup
      qyn = twopi * qy / nsup
      cqxn = cos(qxn)
      cqyn = cos(qyn)
      sqxn = sin(qxn)
      sqyn = sin(qyn)
      c2qxn = cqxn * cqxn - sqxn * sqxn
      c2qyn = cqyn * cqyn - sqyn * sqyn
      s2qxn = two * cqxn * sqxn
      s2qyn = two * cqyn * sqyn
 
*---- Accumulate integrals over (half) superperiod.
      csumx = 0.0
      csumy = 0.0
      ssumx = 0.0
      ssumy = 0.0
 
      do 40 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        if (iq(lcelm+mbpr) .eq. mpelm) then
          call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
          if (sk1l .ne. 0.0  .or.  sk2l .ne. 0.0) then
            call tbset(lhastb, ipos, 1, lhasbf)
            call ucopy(q(lhasbf+1), bxb, iq(lhasbf-1))
            factx = bxb * (sk1l - sk2l * dxb)
            facty = byb * (sk1l - sk2l * dxb)
            csumx = csumx + factx * cos(two*amuxb)
            csumy = csumy + facty * cos(two*amuyb)
            ssumx = ssumx + factx * sin(two*amuxb)
            ssumy = ssumy + facty * sin(two*amuyb)
          endif
        endif
   40 continue
 
*---- Symmetric lattice.
      if (symm) then
        dbx1 =   (csumx * cqxn + ssumx * sqxn) / sqxn
        dby1 = - (csumy * cqyn + ssumy * sqyn) / sqyn
        dbx2 =   csumx / sqxn
        dby2 = - csumy / sqyn
 
*---- Asymmetric lattice.
      else
        dbx1 =   (csumx * cqxn + ssumx * sqxn) / (2.0 * sqxn)
        dby1 = - (csumy * cqyn + ssumy * sqyn) / (2.0 * sqyn)
        dbx2 = dbx1
        dby2 = dby1
      endif
 
      dbxmx = 0.0
      dbymx = 0.0
 
*---- Step around the ring.
      if (iprint .gt. 1) then
        csumxh = csumx
        csumyh = csumy
        ssumxh = ssumx
        ssumyh = ssumy
 
        do 90 ipos = irg1, irg2
          call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
          if (iq(lcelm+mbpr) .eq. mpelm) then
            call tbset(lhastb, ipos, 2, lhasbf)
            call ucopy(q(lhasbf+1), bxb, iq(lhasbf-1))
 
*---- Advance phase for accumulated sum.
            cmux = cos(two*amuxb)
            cmuy = cos(two*amuyb)
            smux = sin(two*amuxb)
            smuy = sin(two*amuyb)
            termx = (cqxn * cmux - sqxn * smux) * csumx
     +            + (sqxn * cmux + cqxn * smux) * ssumx
            termy = (cqyn * cmuy - sqyn * smuy) * csumy
     +            + (sqyn * cmuy + cqyn * smuy) * ssumy
            if (symm) then
              termx = (cqxn * cmux + sqxn * smux) * csumxh
     +              + (sqxn * cmux - cqxn * smux) * ssumxh + termx
              termy = (cqyn * cmuy + sqyn * smuy) * csumyh
     +              + (sqyn * cmuy - cqyn * smuy) * ssumyh + termy
            endif
 
*---- Store values for this element.
            bxbp =   termx / (two * sqxn)
            bybp = - termy / (two * sqyn)
            if (abs(bxbp) .gt. dbxmx) dbxmx = abs(bxbp)
            if (abs(bybp) .gt. dbymx) dbymx = abs(bybp)
            call ucopy(bxb, q(lhasbf+1), iq(lhasbf-1))
 
*---- Advance over this element.
            call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
            factx = bxb * (sk1l - sk2l * dxb)
            facty = byb * (sk1l - sk2l * dxb)
            csumx = csumx + factx * (cmux*c2qxn - smux*s2qxn - cmux)
            csumy = csumy + facty * (cmuy*c2qyn - smuy*s2qyn - cmuy)
            ssumx = ssumx + factx * (smux*c2qxn + cmux*s2qxn - smux)
            ssumy = ssumy + facty * (smuy*c2qyn + cmuy*s2qyn - smuy)
          endif
   90   continue
      endif
 
*---- Optional print.
      if (iprint .gt. 0) then
        write (iqpr2, 910)
        write (iqpr2, 920) dbx1, dby1
        if (symm) write (iqpr2, 930) dbx2, dby2
        if (iprint .gt. 1) then
          write (iqpr2, 940) dbxmx, dbymx
        else
          write (iqpr2, 950)
        endif
      endif
 
  910 format(' '/' d(beta)/d(delta) / beta:',
     +       t37,'horizontal',t55,'vertical')
  920 format(' at interaction point',t31,1p,2e16.6)
  930 format(' at symmetry point',t31,1p,2e16.6)
  940 format(' maximum',t31,1p,2e16.6/' ')
  950 format(' ')
 
      end
