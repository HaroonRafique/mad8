      subroutine bmgelm(lelm, temp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return element parameters for BEAMPARAM                            *
* Input:                                                               *
*   LELM(1)   (pointer) Current element bank.                          *
* Output:                                                              *
*   TEMP      (real)    Element parameters
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
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      integer mgcmd,micmd,mlcmd,mncmd,mnmbmi,mnmcav,mpbuck,mpclor,
     +mpcoup,mpdelq,mpevar,mpexda,mpi4i2,mpintr,mpkhm,mpmidc,mpnint,
     +mprang,mpsing,mpsynr,mptauq,mptous,mpxbsz,mpybsz,mpytol,mrcmd,
     +msbmpm,msbmrs
      double precision eight,fifty,five,four,half,one,p15d5,p16d0,p17d1,
     +p1d2,p1d3,p1d6,p1d9,p1dm15,p1dm2,p1dm3,p1dm4,p1dm6,p1dm8,p1dm9,
     +p23d0,p25d0,p2dm1,p32d0,p3d6,p55d0,p5dm3,p6d2,p6dm2,p8d2,pfacnb,
     +pfacnq,pfsig,rtodeg,seven,six,sixty,ten,three,twenty,two,twopi,
     +twothd,zero
      parameter      (zero   = 0.0d0,        one    = 1.0d0,
     +                two    = 2.0d0,        three  = 3.0d0,
     +                four   = 4.0d0,        five   = 5.0d0,
     +                six    = 6.0d0,        seven  = 7.0d0,
     +                eight  = 8.0d0,        ten    = 10.0d0,
     +                p16d0  = 16.0d0,       twenty = 20.0d0,
     +                p23d0  = 23.0d0,       p25d0  = 25.0d0,
     +                p32d0  = 32.0d0,       fifty  = 50.0d0,
     +                p55d0  = 55.0d0,       sixty  = 60.0d0,
     +                p1d2   = 1.0d2,        p17d1  = 17.0d1,
     +                p6d2   = 6.0d2,        p8d2   = 8.0d2,
     +                p1d3   = 1.0d3,        p1d6   = 1.0d6,
     +                p3d6   = 3.0d6,        p15d5  = 15.0d5,
     +                p1d9   = 1.0d9,        half   = 0.5d0,
     +                p1dm15 = 1d-15,        p1dm9  = 1.0d-9,
     +                p1dm8  = 1.0d-8,       p1dm6  = 1.0d-6,
     +                p1dm4  = 1.0d-4,       p1dm3  = 1.0d-3,
     +                p1dm2  = 1.0d-2,       p5dm3  = 5.0d-3,
     +                p6dm2  = 6.0d-2,       p2dm1  = 0.2d0    )
 
      parameter      (pfacnb = 0.40404d0,    pfacnq = 0.31859d0,
     +                pfsig  = 0.804d0                         )
 
      parameter      (twopi  = two * pi,     rtodeg = 180.0d0 / pi,
     +                twothd = two / three                     )
 
      parameter      (msbmpm = 2,            msbmrs = 16,
     +                mnmbmi = 80,           mnmcav = 9        )
 
      parameter      (micmd = 1,             mrcmd = micmd + 10,
     +                mlcmd = mrcmd + 6,     mncmd = mlcmd,
     +                mgcmd = mncmd + 2                        )
 
      parameter      (mpnint = 1                               )
 
      parameter      (mpdelq = 1,            mptauq = 2,
     +                mpbuck = 3,            mpcoup = 4,
     +                mpi4i2 = 5,            mpexda = 6,
     +                mpxbsz = 7,            mpybsz = 8,
     +                mpkhm  = 9,            mpytol = 10       )
 
      parameter      (mpsynr = 1,            mpclor = 2,
     +                mptous = 3,            mpsing = 4,
     +                mpevar = 5,            mpmidc = 6        )
 
      parameter      (mpintr = 1,            mprang = 2        )
 
      integer isp
      double precision temp,xkick,ykick
      integer           lelm(*)
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
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
 
      dimension         temp(9)
 
      temp(1) = zero
      temp(2) = zero
      temp(3) = zero
      temp(4) = zero
      temp(5) = zero
      temp(6) = zero
      temp(7) = zero
      temp(8) = zero
      temp(9) = zero
 
*---- Select element type.
        isp = iq(lelm(1)+mbsp)
        go to ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +         110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +         210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     +         310, 310, 310, 310, 310, 310, 310, 310, 310, 310), isp
 
*---- Drift.
   10   continue
*---- Monitor.
  170   continue
  180   continue
  190   continue
 
*---- Collimator.
  200   continue
  210   continue
 
*---- Beam instrument.
  240   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
        go to 500
 
*---- Rectangular or sector bend.
   20   continue
   30   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+meangb), temp(2), mwflt)
          call ucopy(q(lelm(1)+mek1b), temp(3), mwflt)
          call ucopy(q(lelm(1)+mek2b), temp(4), mwflt)
          call ucopy(q(lelm(1)+metltb), temp(5), mwflt)
          call ucopy(q(lelm(1)+mee1b), temp(6), mwflt)
          call ucopy(q(lelm(1)+mee2b), temp(7), mwflt)
          call ucopy(q(lelm(1)+meh1b), temp(8), mwflt)
          call ucopy(q(lelm(1)+meh2b), temp(9), mwflt)
        go to 500
 
*---- Quadrupole.
   50   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+mek1q), temp(3), mwflt)
          call ucopy(q(lelm(1)+metltq), temp(5), mwflt)
        go to 500
 
*---- Sextupole.
   60   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+mek2s), temp(4), mwflt)
          call ucopy(q(lelm(1)+metlts), temp(5), mwflt)
        go to 500
 
*---- Octupole.
   70   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+metlto), temp(5), mwflt)
          call ucopy(q(lelm(1)+mek3o), temp(6), mwflt)
        go to 500
 
*---- Multipole.
   80   continue
          call ucopy(q(lelm(1)+meklm),         temp(2), mwflt)
          call ucopy(q(lelm(1)+meklm+2*mcsiz), temp(3), mwflt)
          call ucopy(q(lelm(1)+meklm+4*mcsiz), temp(4), mwflt)
          call ucopy(q(lelm(1)+meklm+6*mcsiz), temp(6), mwflt)
          call ucopy(q(lelm(1)+metltm),         temp(5), mwflt)
          call ucopy(q(lelm(1)+metltm+2*mcsiz), temp(7), mwflt)
          call ucopy(q(lelm(1)+metltm+4*mcsiz), temp(8), mwflt)
          call ucopy(q(lelm(1)+metltm+6*mcsiz), temp(9), mwflt)
        go to 500
 
*---- Solenoid.
   90   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+mekss), temp(6), mwflt)
        go to 500
 
*---- RF cavity.
  100   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+mefrqc), temp(6), mwflt)
          call ucopy(q(lelm(1)+mevltc), temp(7), mwflt)
          call ucopy(q(lelm(1)+melagc), temp(8), mwflt)
        go to 500
 
*---- Electrostatic separator.
  110   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+metlte), temp(5), mwflt)
          call ucopy(q(lelm(1)+meflde), temp(6), mwflt)
        go to 500
 
*---- Coordinate rotations.
  120   continue
  130   continue
          call ucopy(q(lelm(1)+meangr), temp(6), mwflt)
        go to 500
 
*---- Orbit correctors.
  140   continue
  150   continue
  160   continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
 
*---- Original setting.
          if (isp .eq. 14) then
            call ucopy(q(lcelm+mekick), xkick, mwflt)
            ykick = 0.0
          else if (isp .eq. 16) then
            xkick = 0.0
            call ucopy(q(lcelm+mekick), ykick, mwflt)
          else
            call ucopy(q(lcelm+mekick), xkick, mwflt)
            call ucopy(q(lcelm+mekick+mcsiz), ykick, mwflt)
          endif
 
*---- Correction from C.O. correction algorithm.
          if (lccom .ne. 0) then
            call ucopy(q(lccom+1), xcm, 2*mwflt)
          else
            xcm = 0.0
            ycm = 0.0
          endif
 
*---- Store sum of original setting and correction.
          temp(5) = xcm + xkick
          temp(6) = ycm + ykick
        go to 500
 
*---- General bend (dipole, quadrupole, and skew quadrupole).
  260 continue
          call ucopy(q(lelm(1)+melen), temp(1), mwflt)
          call ucopy(q(lelm(1)+meangg), temp(2), mwflt)
          call ucopy(q(lelm(1)+mek1g), temp(3), mwflt)
          call ucopy(q(lelm(1)+metltg), temp(5), mwflt)
        go to 500
 
*---- Other elements.
   40   continue
  220   continue
  230   continue
  250   continue
        goto 500
  270   continue
*---- L-cavity.
        call ucopy(q(lelm(1)+melen), temp(1), mwflt)
        call ucopy(q(lelm(1)+melen+4*mcsiz), temp(6), mwflt)
        call ucopy(q(lelm(1)+melen+2*mcsiz), temp(7), mwflt)
        call ucopy(q(lelm(1)+melen+3*mcsiz), temp(8), mwflt)
        go to 500
 
  280   continue
  290   continue
  300   continue
  310   continue
  500   continue
 
      end
