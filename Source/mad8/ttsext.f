      subroutine ttsext(el, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Track a set of trajectories through a sextupole.                  *
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
      integer itrack,ktrack
      double precision bi2gi2,const,curv,curv2,d,dsk2,dt,el,el1,el2,el3,
     +el4,four,one,pt,px,py,rfac,rpt,rpx,rpy,sk2,three,tilt,track,
     +twelve,two,x,y,zero
      dimension         track(6,*)
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
 
      parameter         (one    = 1.0d0, two    = 2.0d0)
      parameter         (three  = 3.0d0, four   = 4.0d0)
      parameter         (zero   = 0.0d0, twelve = 12.0d0)
 
*---- Fetch data from pool.
      call ucopy(q(lcelm+melen), el, mwflt)
 
*---- Field strength.
      call ucopy(q(lcelm+mek2s), sk2, mwflt)
      call ucopy(q(lcelm+metlts), tilt, mwflt)
 
*---- Field error.
      if (lcfld .ne. 0) then
        call ucopy(q(lcfld+4*mwflt+1), dsk2, mwflt)
        sk2 = (sk2 + dsk2 / el) / (one + deltas)
      else
        sk2 = sk2 / (one + deltas)
      endif
      sk2 = sk2 * elkfact(6)
 
*---- Rotation at entrance.
      if (tilt .ne. zero) call tttilt(tilt, track, ktrack)
 
*---- Half radiation effects at entrance.
      if (dorad) then
        const = arad * gammas**3 / three
 
*---- Full damping.
        if (dodamp) then
          do 10 itrack = 1, ktrack
            curv = (sk2/two) * (track(1,itrack)**2 + track(3,itrack)**2)
 
            if (dorand) then
              call trphot(el, curv, rfac)
            else
              rfac = const * curv**2 * el
            endif
 
            px = track(2,itrack)
            py = track(4,itrack)
            pt = track(6,itrack)
            track(2,itrack) = px - rfac * (one + pt) * px
            track(4,itrack) = py - rfac * (one + pt) * py
            track(6,itrack) = pt - rfac * (one + pt) ** 2
   10     continue
 
*---- Energy loss like on closed orbit.
        else
          curv2 = (sk2/two)**2 * (track(1,1)**2 + track(3,1)**2)**2
          rfac = const * curv2 * el
          rpx = rfac * (one + track(6,1)) * track(2,1)
          rpy = rfac * (one + track(6,1)) * track(4,1)
          rpt = rfac * (one + track(6,1)) ** 2
 
          do 20 itrack = 1, ktrack
            track(2,itrack) = track(2,itrack) - rpx
            track(4,itrack) = track(4,itrack) - rpy
            track(6,itrack) = track(6,itrack) - rpt
   20     continue
        endif
      endif
 
*---- Track trough magnet.
      el1 = sk2 * el
      el2 = el1 * el / two
      el3 = el2 * el / three
      el4 = el3 * el / four
      bi2gi2 = one / (betas * gammas) ** 2
      do 90 itrack = 1, ktrack
        x  = track(1,itrack)
        px = track(2,itrack)
        y  = track(3,itrack)
        py = track(4,itrack)
        dt = track(5,itrack)
        pt = track(6,itrack)
        d = el * (1.0 - pt / betas)
        track(1,itrack) = x + px * d
     +                - el2 * (x**2 - y**2) / two
     +                - el3 * (x*px - y*py)
     +                - el4 * (px**2 - py**2)
        track(2,itrack) = px
     +                - el1 * (x**2 - y**2) / two
     +                - el2 * (x*px - y*py)
     +                - el3 * (px**2 - py**2)
        track(3,itrack) = y + py * d
     +                + el2 * x * y
     +                + el3 * (x*py + y*px)
     +                + two * el4 * px * py
        track(4,itrack) = py
     +                + el1 * x * y
     +                + el2 * (x*py + y*px)
     +                + two * el3 * px * py
        track(5,itrack) = dt + bi2gi2*pt*d -
     +    el/betas*(px**2 + py**2 + bi2gi2*pt**2)/two + el*pt*dtbyds
   90 continue
 
*---- Half radiation effects at exit.
      if (dorad) then
 
*---- Full damping.
        if (dodamp) then
          do 30 itrack = 1, ktrack
            curv = (sk2/two) * (track(1,itrack)**2 + track(3,itrack)**2)
 
            if (dorand) then
              call trphot(el, curv, rfac)
            else
              rfac = const * curv**2 * el
            endif
 
            px = track(2,itrack)
            py = track(4,itrack)
            pt = track(6,itrack)
            track(2,itrack) = px - rfac * (one + pt) * px
            track(4,itrack) = py - rfac * (one + pt) * py
            track(6,itrack) = pt - rfac * (one + pt) ** 2
   30     continue
 
*---- Energy loss like on closed orbit.
        else
          curv2 = (sk2/two)**2 * (track(1,1)**2 + track(3,1)**2)**2
          rfac = const * curv2 * el
          rpx = rfac * (one + track(6,1)) * track(2,1)
          rpy = rfac * (one + track(6,1)) * track(4,1)
          rpt = rfac * (one + track(6,1)) ** 2
 
          do 40 itrack = 1, ktrack
            track(2,itrack) = track(2,itrack) - rpx
            track(4,itrack) = track(4,itrack) - rpy
            track(6,itrack) = track(6,itrack) - rpt
   40     continue
        endif
      endif
 
*---- Rotation at exit.
      if (tilt .ne. 0.0) call tttilt(- tilt, track, ktrack)
 
      end