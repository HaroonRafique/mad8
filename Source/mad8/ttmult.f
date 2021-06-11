      subroutine ttmult(track, ktrack)
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Track particle through a general thin multipole.                  *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   KTRACK    (integer) Number of surviving tracks.                    *
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
      integer id,ilink,iord,itrk,jtrk,lmap,ltrk,mbatch,nd,ndord,neord,
     +nord,ktrack,i,j
      double precision ang,beti,beti2,const,curv,dbi,dbr,dipi,dipr,dx,
     +dxt,dy,dyt,elrad,ez,fact,fz,half,one,ordinv,pt,px,py,rfac,
     +rpt1,rpt2,rpx1,rpx2,rpy1,rpy2,three,track,two,val
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
      double precision cohelp
 
*---- Communication area for TM module.
*     Positive sign means deflection to negative coordinates.
      common /tmcomm/   cohelp
      save              /tmcomm/
      double precision amu0,asube,asubp,clight,elamda,emass,eps0,erad,
     +falfa,hbar,plamda,pmass,qelect,mumass
 
*---- Universal physical constants.
*     Velocity of light [m/s]:
      parameter         (clight = 2.997 924 58 d+08)
*     Permeability of vacuum [V*s/A*m]:
      parameter         (amu0   = 1.256 637 061d-06)
*     Permittivity of vaccum [A*S/V*m]:
      parameter         (eps0   = 8.854 187 817d-12)
*     Reduced Plack's constant [GeV*s]:
      parameter         (hbar   = 6.58211889d-25)
 
*---- Electromagnetic constants.
*     Elementary charge [A*s]:
      parameter         (qelect = 1.602176462d-19)
*     Fine structure constant [1]:
      parameter         (falfa  = 7.297 353 08 d-03)
 
*---- Electron.
*     Rest mass [GeV]:
      parameter         (emass  = 0.510998902d-3)
*     Classical radius [m]:
      parameter         (erad   = 2.817940285d-15)
*     Reduced Compton wavelength [m]:
      parameter         (elamda = 3.861 593 23 d-13)
*     Magnetic moment anomaly [1]:
      parameter         (asube  = 1.159 652 193d-03)
 
*---- Proton.
*     Rest mass [GeV]:
      parameter         (pmass  = 0.938271998d+00)
*     Reduced Compton wavelength [m]:
      parameter         (plamda = 2.103 089 37 d-16)
*     Magnetic moment anomaly [1]:
      parameter         (asubp  = 1.792 847 386d+00)
 
*---- Muon.
*     Rest mass [GeV]:
      parameter         (mumass  = 0.1056583568d+00)
      integer maxmul
 
*---- Maximum order of multipoles.
      parameter         (maxmul = 20)
      parameter         (one = 1.0d0, half = 0.5d0)
      parameter         (two = 2.0d0, three = 3.0d0)
 
      dimension         fz(2,0:maxmul), ez(2,0:maxmul), ordinv(maxmul)
      logical           first
      save              first, ordinv
 
      integer bvpos
      parameter         (bvpos = 24)
      logical bvflag
      parameter         (mbatch = 640)
      dimension         dxt(mbatch), dyt(mbatch)
      integer           ival(2), iang(2), iel(2)
      equivalence       (val,   ival(1)), (ang,  iang(1))
      equivalence       (elrad, iel(1))
 
      data first        / .true. /
 
*---- Clear field array.
      do 10 iord = 0, maxmul
        fz(1,iord) = 0.0
        fz(2,iord) = 0.0
   10 continue
 
*---- Precompute reciprocals of orders.
      if (first) then
        do 20 iord = 1, maxmul
          ordinv(iord) = one / float(iord)
   20   continue
        first = .false.
      endif
 
*---- Build and store multipole transfer map.
      ilink = iq(lcelm+mbat) + mbemap
      lmap = lq(lcelm-ilink)
      if (lmap .eq. 0) then
        ndord = min((iq(lcelm+mbat) - 4) / 2, maxmul)
        id = mbat + 2 * mcsiz
        nord = 0
        do 30 iord = 0, ndord
          ival(1) = iq(lcelm+id+mcval)
          ival(2) = iq(lcelm+id+mcval+1)
          iang(1) = iq(lcelm+id+mcsiz+mcval)
          iang(2) = iq(lcelm+id+mcsiz+mcval+1)
          ang = ang * float(iord+1)
          fz(1,iord) = val * cos(ang)
          fz(2,iord) = val * sin(ang)
          if (fz(1,iord).ne.0.0 .or. fz(2,iord).ne.0.0) nord = iord
          id = id + 2 * mcsiz
   30   continue
        call utglog(lcelm, bvpos, bvpos, bvflag)
*--- HG000915 use bv flag to possibly invert angle
        if (bvflag) then
          fz(1,0) = beambv * fz(1,0)
          fz(2,0) = beambv * fz(2,0)
        endif
*--- apply inversion and scaling
        do i = 1, nord
          do j = 1, 2
            fz(j,i) = fz(j,i) * elmfact(i)
          enddo
        enddo
*---- Store generated field coefficients.
        nd = 2 * mwflt * (nord + 1)
        call mzbook(2, lmap, lcelm, -ilink, 'MMAP', 0, 0, nd, mreal, 0)
        iq(lmap-5) = nord
        call ucopy(fz, q(lmap+1), nd)
 
*---- Fetch precomputed multipole transfer map.
      else
        nord = iq(lmap-5)
        nd = 2 * mwflt * (nord + 1)
        call ucopy(q(lmap+1), fz, nd)
      endif
 
*---- Nominal dipole strength.
      fact = one / (one + deltas)
      dipr = fz(1,0) * fact
      dipi = fz(2,0) * fact
 
*---- Extract field errors and add them to nominal strengths.
      if (lcfld .ne. 0) then
        neord = min(iq(lcfld-1) / (2 * mwflt) - 1, maxmul)
        nd = 2 * mwflt * (neord + 1)
        call ucopy(q(lcfld+1), ez, nd)
 
*---- Dipole error.
        dbr = (ez(1,0) - fz(1,0) * deltas) * fact
        dbi = (ez(2,0) - fz(2,0) * deltas) * fact
 
*---- Quadrupole and higher terms.
        do 40 iord = 1, neord
          fz(1,iord) = fz(1,iord) + ez(1,iord)
          fz(2,iord) = fz(2,iord) + ez(2,iord)
          if ((fz(1,iord).ne.0.0 .or. fz(2,iord).ne.0.0)  .and.
     +        iord .gt. nord) nord = iord
   40   continue
      else
        dbr = - fz(1,0) * deltas * fact
        dbi = - fz(2,0) * deltas * fact
      endif
 
*---- Fetch multipole length, used for radiation.
      iel(1) = iq(lcelm+melen)
      iel(2) = iq(lcelm+melen+1)
 
*---- Precompute some constants.
      beti = one / betas
      beti2 = half * beti
 
*==== Start tracking; Loop over batches of MBATCH particles each.
      do 200 itrk = 1, ktrack, mbatch
        ltrk = min(itrk+mbatch-1,ktrack)
 
*---- Pure dipole: no higher terms.
        if (nord .eq. 0) then
          do 50 jtrk = 1, ltrk - itrk + 1
            dxt(jtrk) = 0.0
            dyt(jtrk) = 0.0
   50     continue
 
*---- Accumulate multipole kick from highest multipole to quadrupole.
        else
          do 60 jtrk = itrk, ltrk
            dxt(jtrk-itrk+1) =
     +        fz(1,nord)*track(1,jtrk) - fz(2,nord)*track(3,jtrk)
            dyt(jtrk-itrk+1) =
     +        fz(1,nord)*track(3,jtrk) + fz(2,nord)*track(1,jtrk)
   60     continue
 
          do 80 iord = nord - 1, 1, -1
            do 70 jtrk = itrk, ltrk
              dx = dxt(jtrk-itrk+1)*ordinv(iord+1) + fz(1,iord)
              dy = dyt(jtrk-itrk+1)*ordinv(iord+1) + fz(2,iord)
              dxt(jtrk-itrk+1) = dx*track(1,jtrk) - dy*track(3,jtrk)
              dyt(jtrk-itrk+1) = dx*track(3,jtrk) + dy*track(1,jtrk)
   70       continue
   80     continue
          do 85 jtrk = itrk, ltrk
            dxt(jtrk-itrk+1) = fact * dxt(jtrk-itrk+1)
            dyt(jtrk-itrk+1) = fact * dyt(jtrk-itrk+1)
   85     continue
        endif
 
*---- Radiation loss at entrance.
        if (dorad .and. elrad .ne. 0.0) then
          const = arad * gammas**3 / three
 
*---- Full damping.
          if (dodamp) then
            do 110 jtrk = itrk, ltrk
              curv = sqrt((dipr + dxt(jtrk-itrk+1))**2 +
     +                    (dipi + dyt(jtrk-itrk+1))**2) / elrad
 
              if (dorand) then
                call trphot(elrad, curv, rfac)
              else
                rfac = const * curv**2 * elrad
              endif
 
              px = track(2,jtrk)
              py = track(4,jtrk)
              pt = track(6,jtrk)
              track(2,jtrk) = px - rfac * (one + pt) * px
              track(4,jtrk) = py - rfac * (one + pt) * py
              track(6,jtrk) = pt - rfac * (one + pt) ** 2
  110       continue
 
*---- Energy loss like for closed orbit.
          else
 
*---- Store energy loss on closed orbit.
            if (itrk .eq. 1) then
              rfac = const * ((dipr + dxt(1))**2 + (dipi + dyt(1))**2)
              rpx1 = rfac * (one + track(6,1)) * track(2,1)
              rpy1 = rfac * (one + track(6,1)) * track(4,1)
              rpt1 = rfac * (one + track(6,1)) ** 2
            endif
 
            do 120 jtrk = itrk, ltrk
              track(2,jtrk) = track(2,jtrk) - rpx1
              track(4,jtrk) = track(4,jtrk) - rpy1
              track(6,jtrk) = track(6,jtrk) - rpt1
  120       continue
          endif
        endif
 
*---- Apply multipole effect including dipole.
        do 140 jtrk = itrk, ltrk
          track(2,jtrk) = track(2,jtrk) -
     +      (dbr + dxt(jtrk-itrk+1) - track(6,jtrk)*beti*dipr)
          track(4,jtrk) = track(4,jtrk) +
     +      (dbi + dyt(jtrk-itrk+1) - track(6,jtrk)*beti*dipi)
          track(5,jtrk) = track(5,jtrk)
     +      - (dipr*track(1,jtrk) - dipi*track(3,jtrk)) * beti
  140   continue
 
*---- Radiation loss at exit.
        if (dorad .and. elrad .ne. 0.0) then
 
*---- Full damping.
          if (dodamp) then
            do 150 jtrk = itrk, ltrk
              curv = sqrt((dipr + dxt(jtrk-itrk+1))**2 +
     +                    (dipi + dyt(jtrk-itrk+1))**2) / elrad
 
              if (dorand) then
                call trphot(elrad, curv, rfac)
              else
                rfac = const * curv**2 * elrad
              endif
 
              px = track(2,jtrk)
              py = track(4,jtrk)
              pt = track(6,jtrk)
              track(2,jtrk) = px - rfac * (one + pt) * px
              track(4,jtrk) = py - rfac * (one + pt) * py
              track(6,jtrk) = pt - rfac * (one + pt) ** 2
  150       continue
 
*---- Energy loss like for closed orbit.
          else
 
*---- Store energy loss on closed orbit.
            if (itrk .eq. 1) then
              rfac = const * ((dipr + dxt(1))**2 + (dipi + dyt(1))**2)
              rpx2 = rfac * (one + track(6,1)) * track(2,1)
              rpy2 = rfac * (one + track(6,1)) * track(4,1)
              rpt2 = rfac * (one + track(6,1)) ** 2
            endif
 
            do 160 jtrk = itrk, ltrk
              track(2,jtrk) = track(2,jtrk) - rpx2
              track(4,jtrk) = track(4,jtrk) - rpy2
              track(6,jtrk) = track(6,jtrk) - rpt2
  160       continue
          endif
        endif
  200 continue
 
      end
