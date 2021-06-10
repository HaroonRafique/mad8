      subroutine ttbend(el, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track a set of particles through a bending magnet.                 *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   KTRACK    (integer) number of surviving tracks.                    *
* Output:                                                              *
*   EL        (real)    Length of quadrupole.                          *
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
      integer isp,itrack,nd,ktrack
      double precision an,const,ct,e1,e2,el,elrad,field,h,hx,hy,one,pt,
     +px,py,rfac,rpt,rpx,rpy,sk1,sk2,sks,st,three,tilt,track,two,x,y
      dimension         track(6,ktrack)
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
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
 
      parameter         (one = 1.0d0, two = 2.0d0, three = 3.0d0)
      logical fmap
      dimension         field(2,0:2)
      logical bvflag
 
      bvflag = .false.
*---- Test for non-zero length.
      call ucopy(q(lcelm+melen), el, mwflt)
 
      if (el .ne. 0.0) then
        isp = iq(lcelm+mbsp)
 
*---- RBEND or SBEND.
        if (isp .ne. 26) then
          call ucopy(q(lcelm+meangb), an, mwflt)
          call ucopy(q(lcelm+metltb), tilt, mwflt)
          call ucopy(q(lcelm+mek1b), sk1, mwflt)
          call ucopy(q(lcelm+mek2b), sk2, mwflt)
          call ucopy(q(lcelm+mee1b), e1, mwflt)
          call ucopy(q(lcelm+mee2b), e2, mwflt)
          call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
          sks = 0.0
*--- HG000915 use bv flag to possibly invert angle
          if (bvflag) an = beambv * an
          if (isp .eq. 2) then
*--- HG001026: arc length to rectangular bend
            an2 = an / 2.d0
            if (an2 .ne. 0.d0 .and. rbarc)  el = el * an2 / sin(an2)
            e1 = e1 + an2
            e2 = e2 + an2
          endif
 
*---- GBEND.
        else
          call ucopy(q(lcelm+meangg), an, mwflt)
          call ucopy(q(lcelm+metltg), tilt, mwflt)
          call ucopy(q(lcelm+mek1g), sk1, mwflt)
          call ucopy(q(lcelm+meksg), sks, mwflt)
          call ucopy(q(lcelm+mee1g), e1, mwflt)
          call ucopy(q(lcelm+mee2g), e2, mwflt)
          call ucopy(q(lcelm+meintgx+3*mcsiz), bvflag, 1)
          sk2 = 0.0
*--- HG000915 use bv flag to possibly invert angle
          if (bvflag) an = beambv * an
        endif
 
        h = an / el
 
*---- Fetch field errors, if any.
        if (lcfld .ne. 0) then
          nd = min(6*mwflt,iq(lcfld-1))
          call uzero(field, 1, 6*mwflt)
          call ucopy(q(lcfld+1), field, nd)
          h = h + (field(1,0) / el) / (one + deltas)
          sk1 = (sk1 + field(1,1) / el) / (one + deltas)
          sk2 = (sk2 + field(1,2) / el) / (one + deltas)
          sks = (sks + field(2,1) / el) / (one + deltas)
        else
          sk1 = sk1 / (one + deltas)
          sk2 = sk2 / (one + deltas)
          sks = sks / (one + deltas)
        endif
*--- apply inversion and scaling
        sk1 = sk1 * elmfact(1)
        sks = sks * elmfact(1)
        sk2 = sk2 * elmfact(2)
 
*---- Half radiation effects at entrance.
        if (dorad) then
          ct = cos(tilt)
          st = sin(tilt)
          const = arad * gammas**3 / three
 
*---- Full damping, optionally with random effects.
          if (dodamp) then
            do 10 itrack = 1, ktrack
              x =   track(1,itrack) * ct + track(3,itrack) * st
              y = - track(1,itrack) * st + track(3,itrack) * ct
              hx = h + sk1*(x - h*y**2/two) + sks*y +
     +             sk2*(x**2 - y**2)/two
              hy = sks*x - sk1*y - sk2*x*y
              px = track(2,itrack)
              py = track(4,itrack)
              pt = track(6,itrack)
              elrad = el * (one + h*x) * (one - tan(e1)*x)
 
              if (dorand) then
                call trphot(elrad, sqrt(hx**2+hy**2), rfac)
              else
                rfac = const * (hx**2 + hy**2) * elrad
              endif
 
              track(2,itrack) = px - rfac * (one + pt) * px
              track(4,itrack) = py - rfac * (one + pt) * py
              track(6,itrack) = pt - rfac * (one + pt) ** 2
   10       continue
 
*---- Energy loss like on closed orbit.
          else
            x =   track(1,1) * ct + track(3,1) * st
            y = - track(1,1) * st + track(3,1) * ct
            hx = h + sk1*(x - h*y**2/two) + sks*y +
     +           sk2*(x**2 - y**2)/two
            hy = sks*y - sk1*y - sk2*x*y
            elrad = el * (one + h*x) * (one - tan(e1)*x)
            rfac = const * elrad * (hx**2 + hy**2)
            rpx = rfac * (one + track(6,1)) * track(2,1)
            rpy = rfac * (one + track(6,1)) * track(4,1)
            rpt = rfac * (one + track(6,1)) ** 2
 
            do 20 itrack = 1, ktrack
              track(2,itrack) = track(2,itrack) - rpx
              track(4,itrack) = track(4,itrack) - rpy
              track(6,itrack) = track(6,itrack) - rpt
   20       continue
          endif
        endif
 
*---- Get map of dipole and track.
        call tmbend(.true., .false., orbit, fmap, el, ek, re, te)
        call tttrak(ek, re, te, track, ktrack)
 
*---- Half radiation effects at exit.
        if (dorad) then
 
*---- Full damping, optionally with random effects.
          if (dodamp) then
            do 30 itrack = 1, ktrack
              ct = cos(tilt)
              st = sin(tilt)
              x =   track(1,itrack) * ct + track(3,itrack) * st
              y = - track(1,itrack) * st + track(3,itrack) * ct
              hx = h + sk1*(x - h*y**2/two) + sks*y +
     +             sk2*(x**2 - y**2)/two
              hy = sks*x - sk1*y - sk2*x*y
              px = track(2,itrack)
              py = track(4,itrack)
              pt = track(6,itrack)
              elrad = el * (one + h*x) * (one - tan(e2)*x)
 
              if (dorand) then
                call trphot(elrad, sqrt(hx**2+hy**2), rfac)
              else
                rfac = const * (hx**2 + hy**2) * elrad
              endif
 
              track(2,itrack) = px - rfac * (one + pt) * px
              track(4,itrack) = py - rfac * (one + pt) * py
              track(6,itrack) = pt - rfac * (one + pt) ** 2
   30       continue
 
*---- Energy loss like on closed orbit.
          else
            x =   track(1,1) * ct + track(3,1) * st
            y = - track(1,1) * st + track(3,1) * ct
            hx = h + sk1*(x - h*y**2/two) + sks*y +
     +           sk2*(x**2 - y**2)/two
            hy = sks*x - sk1*y - sk2*x*y
            elrad = el * (one + h*x) * (one - tan(e2)*x)
            rfac = const * elrad * (hx**2 + hy**2)
            rpx = rfac * (one + track(6,1)) * track(2,1)
            rpy = rfac * (one + track(6,1)) * track(4,1)
            rpt = rfac * (one + track(6,1)) ** 2
 
            do 40 itrack = 1, ktrack
              track(2,itrack) = track(2,itrack) - rpx
              track(4,itrack) = track(4,itrack) - rpy
              track(6,itrack) = track(6,itrack) - rpt
   40       continue
          endif
        endif
      endif
 
      end
