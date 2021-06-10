      subroutine hachcl(xix, xiy)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Calculate chromaticities.                                          *
* Output:                                                              *
*   XIX       (real)    Horizontal chromaticity.                       *
*   XIY       (real)    Vertical chromaticity.                         *
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer meangb,meangg,meangr,mechg,mee1b,mee1g,mee2b,mee2g,meflde,
     +mefrqc,megapb,megapg,meh1b,meh1g,meh2b,meh2g,mehrmc,meintb,meintg,
     +mek1b,mek1g,mek1q,mek2b,mek2s,mek3b,mek3o,mekick,meklm,meksg,
     +mekss,melagc,melen,mesigx,mesigy,metltb,metlte,metltg,metltm,
     +metlto,metltq,metlts,metyp,mevltc,mexcol,mexma,meycol,meyma
      integer meintbx,meintgx,meapro,mek0lm,met0m,mek1lm,met1m,
     +mek2lm,met2m,mek3lm,met3m,meaprm,meapss,melosc,meaprc,mee0l,
     +medel,mephil,mefrql,melosl,mevoll,melagl,meaprl
 
*---- Bias for element attribute values.
*     These statements MUST be consistent with the command dictionary.
*     Routines using this group must also include BANKHEAD and CMDGROUP.
*     Common to all elements: TYPE and L attributes.
      parameter    (metyp  = mbat   + mcval, melen  = metyp  + mcsiz)
*     Common to RBEND and SBEND.
      parameter    (meangb = melen  + mcsiz, mek1b  = meangb + mcsiz,
     +              mee1b  = mek1b  + mcsiz, mee2b  = mee1b  + mcsiz,
     +              metltb = mee2b  + mcsiz, mek2b  = metltb + mcsiz,
     +              meh1b  = mek2b  + mcsiz, meh2b  = meh1b  + mcsiz,
     +              megapb = meh2b  + mcsiz, meintb = megapb + mcsiz)
      parameter (meintbx = meintb + mcsiz, mek3b  = meintbx + mcsiz)
*     QUADRUPO.
      parameter    (mek1q  = melen  + mcsiz, metltq = mek1q  + mcsiz)
      integer meaprq
      parameter    (meaprq = metltq + mcsiz)
*     SEXTUPOL.
      parameter    (mek2s  = melen  + mcsiz, metlts = mek2s  + mcsiz)
      integer meaprs
      parameter    (meaprs = metlts + mcsiz)
*     OCTUPOLE.
      parameter    (mek3o  = melen  + mcsiz, metlto = mek3o  + mcsiz)
      parameter    (meapro = metlto + mcsiz)
*     MULTIPOL.
      parameter    (mek0lm = melen  + mcsiz, met0m  = mek0lm + mcsiz,
     +              mek1lm = met0m  + mcsiz, met1m  = mek1lm + mcsiz,
     +              mek2lm = met1m  + mcsiz, met2m  = mek2lm + mcsiz,
     +              mek3lm = met2m  + mcsiz, met3m  = mek3lm + mcsiz,
     +              meaprm = melen  + 21*mcsiz)
*     MULTIPOL.
      parameter    (meklm  = melen  + mcsiz, metltm = meklm  + mcsiz)
*     SOLENOID.
      parameter    (mekss  = melen  + mcsiz, meapss = mekss  + mcsiz)
*     RFCAVITY.
      parameter    (mevltc = melen  + mcsiz, melagc = mevltc + mcsiz,
     +              mefrqc = melagc + mcsiz, mehrmc = mefrqc + mcsiz)
      parameter    (melosc = mehrmc + 5*mcsiz,
     +              meaprc = melosc + 3*mcsiz)
*     ELSEPARA.
      parameter    (meflde = melen  + mcsiz, metlte = meflde + mcsiz)
*     Common to SROT and YROT.
      parameter    (meangr = melen  + mcsiz)
*     Common to KICK, HKICK, and VKICK.
      parameter    (mekick = melen  + mcsiz)
*     Common to ECOLLIMA and RCOLLIMA.
      parameter    (mexcol = melen  + mcsiz, meycol = mexcol + mcsiz)
*     BEAMBEAM.
      parameter    (mesigx = melen  + mcsiz, mesigy = mesigx + mcsiz,
     +              mexma  = mesigy + mcsiz, meyma  = mexma  + mcsiz,
     +              mechg  = meyma  + mcsiz)
*     GBEND.
      parameter    (meangg = melen  + mcsiz, mek1g  = meangg + mcsiz,
     +              mee1g  = mek1g  + mcsiz, mee2g  = mee1g  + mcsiz,
     +              metltg = mee2g  + mcsiz, meksg  = metltg + mcsiz,
     +              meh1g  = meksg  + mcsiz, meh2g  = meh1g  + mcsiz,
     +              megapg = meh2g  + mcsiz, meintg = megapg + mcsiz)
*     lcavity.
      parameter    (mee0l  = melen  + mcsiz, medel  = mee0l  + mcsiz,
     +              mephil = medel  + mcsiz, mefrql = mephil + mcsiz,
     +              melosl = mefrql + mcsiz, mevoll = melosl + mcsiz,
     +              melagl = mevoll + mcsiz, meaprl = melagl + mcsiz)
      parameter (meintgx = meintg + mcsiz)
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
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
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
      integer ienum,iflag,iocc,ipos,isp
      double precision angle,bgx,bgy,dint,dxx,dyy,el,fact,fourpi,gx1,
     +gx2,gy1,h,sk1l,sk2l,sk3l,sk4l,sqx,sqy,ss,twopi,xix,xiy
      double precision an2
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (fourpi = 4.0 * pi, twopi = 2.0 * pi)
      character*(mcnam) elmnam
 
      xix = 0.0
      xiy = 0.0
 
*---- Deal with all elements.
      do 90 ipos = irg1 + 1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
 
*---- Physical element?
        if (iq(lcelm+mbpr) .eq. mpelm) then
          isp = iq(lcelm+mbsp)
 
*---- Fetch lattice functions at both ends.
          call tbset(lhaltb, ipos - 1, 1, lhalbf)
          call ucopy(q(lhalbf+1), bx1, iq(lhalbf-1))
          call tbset(lhaltb, ipos, 1, lhalbf)
          call ucopy(q(lhalbf+1), bx2, iq(lhalbf-1))
          gx1 = (1.0 + ax1**2) / bx1
          gx2 = (1.0 + ax2**2) / bx2
          gy1 = (1.0 + ay1**2) / by1
 
*---- Fetch field strength coefficients.
          call hastrg(angle, sk1l, sk2l, sk3l, sk4l)
 
*---- Quadrupole.
          el = s2 - s1
          if (isp .eq. 5) then
            xix = xix - ((el * gx1 + sk1l * bx1) + (ax2 - ax1)) / 2.0
            xiy = xiy - ((el * gy1 - sk1l * by1) + (ay2 - ay1)) / 2.0
 
*---- Sextupole.
          else if (isp .eq. 6) then
            dxx = + (1.0/2.0) * (bx1*dx1 + bx2*dx2 -
     +      (el/6.0) * (bx2*dpx2 - bx1*dpx1 - 2.0*(ax2*dx2 - ax1*dx1)))
            dyy = - (1.0/2.0) * (by1*dx1 + by2*dx2 -
     +      (el/6.0) * (by2*dpx2 - by1*dpx1 - 2.0*(ay2*dx2 - ay1*dx1)))
            xix = xix + dxx * sk2l
            xiy = xiy + dyy * sk2l
            dxx = dxx * el
            dyy = dyy * el
 
*---- Multipole.
          else if (isp .eq. 8) then
            dxx = + bx1 * dx1
            dyy = - by1 * dx1
            xix = xix - bx1 * sk1l + dxx * sk2l
            xiy = xiy + by1 * sk1l + dyy * sk2l
 
*---- Dipole.
          else if (isp .eq. 2  .or.  isp .eq. 3) then
*--- HG001026: arc length to rectangular bend
            if (isp .eq. 2)  then
              an2 = angle / 2.d0
              if (an2 .ne. 0.d0 .and. rbarc)  el = el * an2 / sin(an2)
            endif
            h   = angle / el
            sqx = + sk1l / el + h ** 2
            sqy = - sk1l / el
            ss  = sk2l / el
            bgx = bx1 * sqx + gx1
            bgy = by1 * sqy + gy1
            dxx = + (el/2.0) * (bx1*dx1 + bx2*dx2 -
     +      (el/6.0) * (bx2*dpx2 - bx1*dpx1 - 2.0*(ax2*dx2 - ax1*dx1)))
            dyy = - (el/2.0) * (by1*dx1 + by2*dx2 -
     +      (el/6.0) * (by2*dpx2 - by1*dpx1 - 2.0*(ay2*dx2 - ay1*dx1)))
            dint = (el/2.0) * (dx2 + dx1 - (el/6.0) * (dpx2 - dpx1))
            xix = xix
     +          - (el * bgx + ax2 - ax1) / 2.0
     +          + (ss + h * sqx - 2.0 * h**3) * dxx
     +          - h*el * (ax2*dpx2 + ax1*dpx1 - (el/6.0) *
     +            (h * (ax2 - ax1) - dpx2*gx2 + dpx1*gx1 +
     +            sqx*(dpx2*bx2 - dpx2*bx1 - dx2*ax2 + dx1*ax1)))
     +          + h * bgx * dint
            xiy = xiy
     +          - (el * bgy + ay2 - ay1) / 2.0
     +          + ss * dyy
     +          - h * (dpx2 * by2 - dpx1 * by1)
     +          + h * bgy * dint
          endif
        endif
   90 continue
 
*---- Deal with symmetry and global factors.
      if (symm) then
        fact = nsup / twopi
      else
        fact = nsup / fourpi
      endif
      xix = fact * xix
      xiy = fact * xiy
 
      end
