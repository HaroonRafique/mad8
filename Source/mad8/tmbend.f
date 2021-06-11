      subroutine tmbend(fsec, ftrk, orbit, fmap, el, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for bending magnets of all types.                    *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   FTRK      (logical) If true, track orbit.                          *
* Input/output:                                                        *
*   ORBIT(6)  (real)    Closed orbit.                                  *
* Output:                                                              *
*   FMAP      (logical) If true, element has a map.                    *
*   EL        (real)    Element length.                                *
*   EK(6)     (real)    Kick due to element.                           *
*   RE(6,6)   (real)    Transfer matrix.                               *
*   TE(6,6,6) (real)    Second-order terms.                            *
* Important common data:                                               *
*   LCELM     /REFER/   Current element bank.                          *
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
      integer isp,nd,i,j
      double precision an,corr,ct,dh,e1,e2,ek,ek0,el,field,fint,h,h1,h2,
     +hgap,hx,hy,one,orbit,pt,re,rfac,rw,sk1,sk2,sks,st,te,three,tilt,
     +tw,two,x,y
      logical           fsec, ftrk, fmap
      dimension         orbit(6), ek(6), re(6,6), te(6,6,6)
      double precision an2
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
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
      double precision cohelp
 
*---- Communication area for TM module.
*     Positive sign means deflection to negative coordinates.
      common /tmcomm/   cohelp
      save              /tmcomm/
*---- Synchrotron integrals, etc.
      common /synch/    synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision  synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision fintx
      logical bvflag
 
      parameter         (one = 1.0d0, two = 2.0d0, three = 3.0d0)
 
      dimension         field(2,0:2)
      dimension         ek0(6), rw(6,6), tw(6,6,6)
 
      bvflag = .false.
*---- Test for non-zero length.
      call ucopy(q(lcelm+melen),  el,   mwflt)
      fmap = el .ne. 0.0
      if (fmap) then
        isp = iq(lcelm+mbsp)
 
*---- RBEND or SBEND.
        if (isp .ne. 26) then
          call ucopy(q(lcelm+meangb), an, mwflt)
          call ucopy(q(lcelm+metltb), tilt, mwflt)
          call ucopy(q(lcelm+mek1b), sk1, mwflt)
          call ucopy(q(lcelm+mee1b), e1, mwflt)
          call ucopy(q(lcelm+mee2b), e2, mwflt)
          call ucopy(q(lcelm+mek2b), sk2, mwflt)
          call ucopy(q(lcelm+meh1b), h1, mwflt)
          call ucopy(q(lcelm+meh2b), h2, mwflt)
          call ucopy(q(lcelm+megapb), hgap, mwflt)
          call ucopy(q(lcelm+meintb), fint, mwflt)
          call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
          call ucopy(q(lcelm+meintbx), fintx, mwflt)
          sks = 0.0
 
*---- GBEND.
        else
          call ucopy(q(lcelm+meangg), an, mwflt)
          call ucopy(q(lcelm+metltg), tilt, mwflt)
          call ucopy(q(lcelm+mek1g), sk1, mwflt)
          call ucopy(q(lcelm+mee1g), e1, mwflt)
          call ucopy(q(lcelm+mee2g), e2, mwflt)
          call ucopy(q(lcelm+meksg), sks, mwflt)
          call ucopy(q(lcelm+meh1g), h1, mwflt)
          call ucopy(q(lcelm+meh2g), h2, mwflt)
          call ucopy(q(lcelm+megapg), hgap, mwflt)
          call ucopy(q(lcelm+meintg), fint, mwflt)
          call ucopy(q(lcelm+meintgx), fintx, mwflt)
          call ucopy(q(lcelm+meintgx+3*mcsiz), bvflag, 1)
          sk2 = 0.0
        endif
*--- HG000915 use bv flag to possibly invert angle
        if (bvflag) an = beambv * an
        if (isp .eq. 2) then
*--- HG001026: arc length to rectangular bend
          an2 = an / 2.d0
          if (an2 .ne. 0.d0 .and. rbarc)  el = el * an2 / sin(an2)
          e1 = e1 + an2
          e2 = e2 + an2
        endif
        h = an / el
 
*---- Fetch field errors and change coefficients using DELTAS.
        if (lcfld .ne. 0) then
          nd = min(6*mwflt,iq(lcfld-1))
          call uzero(field, 1, 6*mwflt)
          call ucopy(q(lcfld+1), field, nd)
*--- HG000915 use bv flag to possibly invert angle
          if (bvflag) field(1,0) = beambv * field(1,0)
          dh = (- h * deltas + field(1,0) / el) / (one + deltas)
          sk1 = (sk1 + field(1,1) / el) / (one + deltas)
          sk2 = (sk2 + field(1,2) / el) / (one + deltas) * cohelp
          sks = (sks + field(2,1) / el) / (one + deltas) * cohelp
        else
          dh = - h * deltas / (one + deltas)
          sk1 = sk1 / (one + deltas)
          sk2 = sk2 / (one + deltas) * cohelp
          sks = sks / (one + deltas) * cohelp
        endif
*--- apply inversion and scaling
        sk1 = sk1 * elmfact(1)
        sks = sks * elmfact(1)
        sk2 = sk2 * elmfact(2)
 
*---- Half radiation effects at entrance.
        if (ftrk .and. dorad) then
          ct = cos(tilt)
          st = sin(tilt)
          x =   orbit(1) * ct + orbit(3) * st
          y = - orbit(1) * st + orbit(3) * ct
          hx = h + dh + sk1*(x - h*y**2/two) + sks*y +
     +         sk2*(x**2 - y**2)/two
          hy = sks * x - sk1*y - sk2*x*y
          rfac = (arad * gammas**3 * el / three)
     +         * (hx**2 + hy**2) * (one + h*x) * (one - tan(e1)*x)
          pt = orbit(6)
          orbit(2) = orbit(2) - rfac * (one + pt) * orbit(2)
          orbit(4) = orbit(4) - rfac * (one + pt) * orbit(4)
          orbit(6) = orbit(6) - rfac * (one + pt) ** 2
        endif
 
*---- Body of the dipole.
        if (isp .ne. 26) then
          call tmsect(.true., el, h, dh, sk1, sk2, ek, re, te)
        else
          call tmgsec(.true., el, h, dh, sk1, sks, ek, re, te)
        endif
 
*---- Fringe fields.
        corr = (h + h) * hgap * fint
        call tmfrng(.true., h, sk1, e1, h1, +one, corr, ek0, rw, tw)
        call tmcat1(.true., ek, re, te, ek0, rw, tw, ek, re, te)
*---- Tor: use FINTX if set
        if (fintx .gt. 0) then
          corr = (h + h) * hgap * fintx
        else
          corr = (h + h) * hgap * fint
        endif
        call tmfrng(.true., h, sk1, e2, h2, -one, corr, ek0, rw, tw)
        call tmcat1(.true., ek0, rw, tw, ek, re, te, ek, re, te)
 
*---- Apply tilt.
        if (tilt .ne. 0.0) then
          call tmtilt(.true., tilt, ek, re, te)
          cplxy = .true.
        endif
 
*---- Track orbit.
        if (ftrk) then
          call tmtrak(ek, re, te, orbit, orbit)
 
*---- Half radiation effects at exit.
          if (dorad) then
            x =   orbit(1) * ct + orbit(3) * st
            y = - orbit(1) * st + orbit(3) * ct
            hx = h + dh + sk1*(x - h*y**2/two) + sks*y +
     +           sk2*(x**2 - y**2)/two
            hy = sks * x - sk1*y - sk2*x*y
            rfac = (arad * gammas**3 * el / three)
     +           * (hx**2 + hy**2) * (one + h*x) * (one - tan(e2)*x)
            pt = orbit(6)
            orbit(2) = orbit(2) - rfac * (one + pt) * orbit(2)
            orbit(4) = orbit(4) - rfac * (one + pt) * orbit(4)
            orbit(6) = orbit(6) - rfac * (one + pt) ** 2
          endif
        endif
 
*---- Identity map.
      else
        call uzero(ek, 1, 6*mwflt)
        call m66one(re)
        if (fsec) call uzero(te, 1, 216*mwflt)
      endif
 
*---- Tor: set parameters for sychrotron integral calculations
      rhoinv = h
      blen = el
      end
