      subroutine ttquad(el, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track a set of particles through a quadrupole.                     *
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
      double precision const,curv,curv2,cx,cy,dsk1,el,four,one,pt0,px0,
     +px1,py0,py1,qk,qkl,qkl2,rfac,rpt,rpx,rpy,sk1,sx,sy,t0,t1,three,
     +tilt,track,two,x0,x1,y0,y1,zero
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
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
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
 
      parameter         (zero   = 0.0d0, one    = 1.0d0)
      parameter         (two    = 2.0d0, three  = 3.0d0)
      parameter         (four   = 4.0d0)
 
*---- Fetch data from pool.
      call ucopy(q(lcelm+melen), el, mwflt)
      if (el .ne. zero) then
 
*---- Field.
        call ucopy(q(lcelm+mek1q), sk1, mwflt)
        if (lcfld .ne. 0) then
          call ucopy(q(lcfld+2*mwflt+1), dsk1, mwflt)
          sk1 = (sk1 + dsk1 / el) / (one + deltas)
        else
          sk1 = sk1 / (one + deltas)
        endif
 
*---- Tilt at entrance.
        call ucopy(q(lcelm+metltq), tilt, mwflt)
        if (tilt .ne. 0.0) call tttilt(tilt, track, ktrack)
 
*---- Half radiation effect at exit.
        if (dorad) then
          const = arad * gammas**3 / three
 
*---- Full damping.
          if (dodamp) then
            do 10 itrack = 1, ktrack
              curv = sk1 * sqrt(track(1,itrack)**2 + track(3,itrack)**2)
 
              if (dorand) then
                call trphot(el, curv, rfac)
              else
                rfac = const * curv**2 * el
              endif
 
              track(2,itrack) = track(2,itrack) -
     +          rfac * (one + track(6,itrack)) * track(2,itrack)
              track(4,itrack) = track(4,itrack) -
     +          rfac * (one + track(6,itrack)) * track(4,itrack)
              track(6,itrack) = track(6,itrack) -
     +          rfac * (one + track(6,itrack))**2
   10       continue
 
*---- Energy loss like on closed orbit.
          else
            curv2 = sk1**2 * (track(1,1)**2 + track(3,1)**2)
            rfac = const * curv2 * el
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
 
*---- Compute transfer map.
        qk = sqrt(abs(sk1))
        qkl = qk * el
 
        if (abs(qkl) .lt. 1.0e-3) then
          qkl2 = sk1 * el**2
          cx = (1.0 - qkl2 / 2.0)
          sx = (1.0 - qkl2 / 6.0) * el
          cy = (1.0 + qkl2 / 2.0)
          sy = (1.0 + qkl2 / 6.0) * el
        else if (sk1 .gt. 0.0) then
          cx = cos(qkl)
          sx = sin(qkl) / qk
          cy = cosh(qkl)
          sy = sinh(qkl) / qk
        else
          cx = cosh(qkl)
          sx = sinh(qkl) / qk
          cy = cos(qkl)
          sy = sin(qkl) / qk
        endif
 
*---- Track through magnet.
        do 30 itrack = 1, ktrack
          x0  = track(1,itrack)
          px0 = track(2,itrack)
          y0  = track(3,itrack)
          py0 = track(4,itrack)
          t0  = track(5,itrack)
          pt0 = track(6,itrack)
 
*---- Linear part of map.
          x1  =          cx * x0 + sx * px0
          px1 =  - sk1 * sx * x0 + cx * px0
          y1  =          cy * y0 + sy * py0
          py1 =  + sk1 * sy * y0 + cy * py0
 
          t1 = t0
     +       + pt0*el/(betas*gammas)**2*(one-(three/two)*pt0/betas)
     +       + el*pt0*dtbyds
 
*---- Quadratic part of map.
          track(1,itrack) = x1 - (el*px1 + sx*px0)*(pt0/(two*betas))
          track(3,itrack) = y1 - (el*py1 + sy*py0)*(pt0/(two*betas))
          track(2,itrack) = px1 + sk1*(el*x1 - sx*x0)*(pt0/(two*betas))
          track(4,itrack) = py1 - sk1*(el*y1 - sy*y0)*(pt0/(two*betas))
          track(5,itrack) = t1 + (x0*px0 - x1*px1 + y0*py0 - y1*py1 -
     +      el*(px0**2 + py0**2 + sk1*(x0**2 - y0**2))) / (four*betas)
   30   continue
 
*---- Full damping.
        if (dorad) then
 
*---- Full damping.
          if (dodamp) then
            do 40 itrack = 1, ktrack
              curv = sk1 * sqrt(track(1,itrack)**2 + track(3,itrack)**2)
 
              if (dorand) then
                call trphot(el, curv, rfac)
              else
                rfac = const * curv**2 * el
              endif
 
              track(2,itrack) = track(2,itrack) -
     +          rfac * (one + track(6,itrack)) * track(2,itrack)
              track(4,itrack) = track(4,itrack) -
     +          rfac * (one + track(6,itrack)) * track(4,itrack)
              track(6,itrack) = track(6,itrack) -
     +          rfac * (one + track(6,itrack))**2
   40       continue
 
*---- Energy loss like on closed orbit.
          else
            curv2 = sk1**2 * (track(1,1)**2 + track(3,1)**2)
            rfac = const * curv2 * el
            rpx = rfac * (one + track(6,1)) * track(2,1)
            rpy = rfac * (one + track(6,1)) * track(4,1)
            rpt = rfac * (one + track(6,1)) ** 2
 
            do 50 itrack = 1, ktrack
              track(2,itrack) = track(2,itrack) - rpx
              track(4,itrack) = track(4,itrack) - rpy
              track(6,itrack) = track(6,itrack) - rpt
   50       continue
          endif
        endif
 
*---- Tilt at exit.
        if (tilt .ne. 0.0) call tttilt(- tilt, track, ktrack)
      endif
 
      end
