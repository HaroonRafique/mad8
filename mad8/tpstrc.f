      subroutine tpstrc(elmnam, idisk, nord)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Write element parameters on disk for STRUCTURE command.            *
* Input:                                                               *
*   LCELM     /REFER/   Current element bank.                          *
*   ELMNAM    (char)    Name associated with current element.          *
*   IDISK     (integer) Logical unit number.                           *
*   NORD      (integer) Maximum multipole order.                       *
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
      integer i,idisk,iord,isp,kord,lkey,na,nd,ne,nord
      double precision ang,dsk1,dsk2,dsk3,ferror,field,sk0l,temp,
     +tilt0,val,xcm,xkick,ycm,ykick,zero
      character*(*)     elmnam
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
 
      integer maxmul
 
*---- Maximum order of multipoles.
      parameter         (maxmul = 20)
      parameter         (zero = 0.0d0)
 
      character*(mcnam) key, name, type
      dimension         temp(2*maxmul+2)
      dimension         field(2,0:maxmul), ferror(2,0:maxmul)
      integer bvpos
      parameter         (bvpos = 24)
      logical bvflag
 
      bvflag = .false.
      call uzero(temp, 1, 2 * mwflt * (maxmul + 1))
 
      if (lcelm .eq. 0) then
        key = ' '
        name = 'INITIAL'
        type = ' '
      else
        lkey = lq(lcelm+1)
        call diname(ldkey, iq(lkey+mbnam), key)
        name = elmnam
        type = ' '
        call utgnam(lcelm, 1, 1, type)
 
*---- Select element type.
        isp = iq(lcelm+mbsp)
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
 
*---- Beam instrument.
  240   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
        go to 500
 
*---- Rectangular or sector bend.
   20   continue
   30   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+meangb), temp(2), mwflt)
          call ucopy(q(lcelm+mek1b), temp(3), mwflt)
          call ucopy(q(lcelm+mek2b), temp(4), mwflt)
          call ucopy(q(lcelm+metltb), temp(5), mwflt)
          call ucopy(q(lcelm+mee1b), temp(6), mwflt)
          call ucopy(q(lcelm+mee2b), temp(7), mwflt)
          call ucopy(q(lcelm+meh1b), temp(8), mwflt)
          call ucopy(q(lcelm+meh2b), temp(9), mwflt)
          call ucopy(q(lcelm+meintbx+3*mcsiz), bvflag, 1)
 
*---- Fetch field errors, if any.
          if (lcfld .ne. 0  .and.  temp(1) .ne. 0.0) then
            nd = min(6*mwflt,iq(lcfld-1))
            call uzero(ferror, 1, 6*mwflt)
            call ucopy(q(lcfld+1), ferror, nd)
            temp(2) = temp(2) + ferror(1,0)
            temp(3) = temp(3) + ferror(1,1) / temp(1)
            temp(4) = temp(4) + ferror(1,2) / temp(1)
          endif
*--- HG000915 use bv flag to possibly invert angle
          if (bvflag) temp(2) = beambv * temp(2)
*--- apply inversion and scaling
          do i = 3, 4
            temp(i) = elmfact(i-2) * temp(i)
          enddo
        go to 500
 
*---- Quadrupole.
   50   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+mek1q), temp(3), mwflt)
          call ucopy(q(lcelm+metltq), temp(5), mwflt)
 
*---- Fetch field error, if any.
          if (lcfld .ne. 0) then
            call ucopy(q(lcfld+2*mwflt+1), dsk1, mwflt)
            temp(3) = temp(3) + dsk1 / temp(1)
          endif
        go to 500
 
*---- Sextupole.
   60   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+mek2s), temp(4), mwflt)
          call ucopy(q(lcelm+metlts), temp(5), mwflt)
 
*---- Fetch field error, if any.
          if (lcfld .ne. 0  .and. temp(1) .ne. 0.0) then
            call ucopy(q(lcfld+4*mwflt+1), dsk2, mwflt)
            temp(4) = temp(4) + dsk2 / temp(1)
          endif
        go to 500
 
*---- Octupole.
   70   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+mek3o), temp(6), mwflt)
          call ucopy(q(lcelm+metlto), temp(5), mwflt)
 
*---- Fetch field error, if any.
          if (lcfld .ne. 0  .and.  temp(1) .ne. 0.0) then
            call ucopy(q(lcfld+6*mwflt+1), dsk3, mwflt)
            temp(6) = temp(6) + dsk3 / temp(1)
          endif
        go to 500
 
*---- Multipole.
   80   continue
 
*---- Nominal dipole field and maximum order.
          kord = min(nord,maxmul)
          call ucopy(q(lcelm+meklm), sk0l, mwflt)
          call ucopy(q(lcelm+metltm), tilt0, mwflt)
          call utglog(lcelm, bvpos, bvpos, bvflag)
          if (bvflag) sk0l = beambv * sk0l
          temp(2) = sk0l * cos(tilt0)
          temp(3) = sk0l * sin(tilt0)
          write (idisk, 910) key, name, zero, (temp(i), i = 2, 3), kord
 
*---- Multipole components.
          nd = 2 * mwflt * (kord + 1)
          call uzero(field, 1, nd)
          na = min(2 * maxmul + 2, iq(lcelm+mbat))
          call utgflt(lcelm, 3, na, field)
 
*---- Field error data.
          call uzero(ferror, 1, nd)
          if (lcfld .ne. 0) then
            ne = min(iq(lcfld-1), nd)
            call ucopy(q(lcfld+1), ferror, ne)
          endif
 
*---- Dipole error.
          temp(1) = ferror(1,0)
          temp(2) = ferror(2,0)
 
*---- Other components and errors.
          do 85 iord = 0, kord
            if (iord .eq. 0) then
              val = 0.0
            else
              val = field(1,iord)
            endif
            ang = - field(2,iord) * float(iord+1)
            temp(iord*2+1) = val*cos(ang) + ferror(1,iord)
            temp(iord*2+2) = val*sin(ang) + ferror(2,iord)
   85     continue
 
          write (idisk, 920) (temp(2*i+1), i = 0, kord)
          write (idisk, 920) (temp(2*i+2), i = 0, kord)
        go to 9999
 
*---- Solenoid.
   90   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+mekss), temp(6), mwflt)
        go to 500
 
*---- RF cavity.
  100   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+mefrqc), temp(6), mwflt)
          call ucopy(q(lcelm+mevltc), temp(7), mwflt)
          call ucopy(q(lcelm+melagc), temp(8), mwflt)
        go to 500
 
*---- Electrostatic separator.
  110   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+metlte), temp(5), mwflt)
          call ucopy(q(lcelm+meflde), temp(6), mwflt)
        go to 500
 
*---- Coordinate rotations.
  120   continue
  130   continue
          call ucopy(q(lcelm+meangr), temp(6), mwflt)
        go to 500
 
*---- Orbit correctors.
  140   continue
  150   continue
  160   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
 
*---- Original setting.
          if (isp .eq. 14) then
            call ucopy(q(lcelm+mekick), xkick, mwflt)
            call ucopy(q(lcelm+mekick+mcsiz), bvflag, 1)
            ykick = 0.0
          else if (isp .eq. 16) then
            xkick = 0.0
            call ucopy(q(lcelm+mekick), ykick, mwflt)
            call ucopy(q(lcelm+mekick+mcsiz), bvflag, 1)
          else
            call ucopy(q(lcelm+mekick), xkick, mwflt)
            call ucopy(q(lcelm+mekick+mcsiz), ykick, mwflt)
            call ucopy(q(lcelm+mekick+2*mcsiz), bvflag, 1)
          endif
 
*---- Field errors.
          if (lcfld .ne. 0) then
            call ucopy(q(lcfld+1), ferror, 2*mwflt)
          else
            ferror(1,0) = 0.0
            ferror(2,0) = 0.0
          endif
 
*---- Correction from C.O. correction algorithm.
          if (lccom .ne. 0) then
            call ucopy(q(lccom+1), xcm, 2*mwflt)
          else
            xcm = 0.0
            ycm = 0.0
          endif
 
*---- Store sum of original setting and correction.
          temp(5) = xcm + xkick + ferror(1,0)
          temp(6) = ycm + ykick + ferror(2,0)
*--- HG000915 use bv flag to possibly invert angle
        if (bvflag) then
          temp(5) = beambv * temp(5)
          temp(6) = beambv * temp(6)
        endif
        go to 500
 
*---- Collimator.
  200   continue
  210   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+mexcol), temp(2), mwflt)
          call ucopy(q(lcelm+meycol), temp(3), mwflt)
        go to 500
 
*---- General bend (dipole, quadrupole, and skew quadrupole).
  260   continue
          call ucopy(q(lcelm+melen), temp(1), mwflt)
          call ucopy(q(lcelm+meangg), temp(2), mwflt)
          call ucopy(q(lcelm+mek1g), temp(3), mwflt)
          call ucopy(q(lcelm+meksg), temp(4), mwflt)
          call ucopy(q(lcelm+metltg), temp(5), mwflt)
 
*---- Fetch field errors, if any.
          if (lcfld .ne. 0  .and.  temp(1) .ne. 0.0) then
            nd = min(4*mwflt,iq(lcfld-1))
            call uzero(ferror, 1, 4*mwflt)
            call ucopy(q(lcfld+1), ferror, nd)
            temp(2) = temp(2) + ferror(1,0)
            temp(3) = temp(3) + ferror(1,1) / temp(1)
            temp(4) = temp(4) + ferror(2,1) / temp(1)
          endif
        go to 500
 
*---- Other elements.
   40   continue
  220   continue
  230   continue
  250   continue
  270   continue
  280   continue
  290   continue
  300   continue
  310   continue
  500   continue
      endif
 
      write (idisk, 930) key, name, (temp(i), i = 1, 9)
 
  910 format(a4,a16,f12.6,1p,2e16.9,i16)
  920 format(1p,5e16.9)
  930 format(a4,a16,f12.6,1p,3e16.9/5e16.9)
 
 9999 end
