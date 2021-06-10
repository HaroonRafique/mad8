      subroutine bmini2(ierr,jerr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initializes after command decoding                                 *
*-- Output:                                                            *
*   IERR    (int)    0: OK, 1: element with non-zero tilt,             *
*                    2: non-zero vert. dispersion at interaction pt.   *
*                    3: bet_x = zero, 4: bet_y = zero at int. point    *
*   JERR    (int)    (first) element causing the error                 *
*                                                                      *
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
 
      integer idcoup,idener,idtauq,iflgbm,ihilim,ihirng,ilolim,ilorng,
     +intrct,irg1,irg2,isup,iubdef,iucdef,lbmpm,lbmrs,lbref1,lbref2,
     +nbmcav,nbmelm,nint
      double precision alamda,amasc2,beamis,bucket,coupl,cvbtrf,cvfill,
     +cvfreq,cvharm,cvleng,cvpow,cvpsi,cvshnt,cvvolt,delq,dspytl,elak1,
     +elak2,elang,ele1d,ele2d,eleng,enerev,energv,exdata,fctkhm,fxbeam,
     +fybeam,game0,power,radius,si4i2,tauq,tauqs
      common /bmpmcm/   lbmpm, lbmrs, lbref1, lbref2
      save              /bmpmcm/
      common /bmpmin/   nint, ilolim, ihilim, ilorng, ihirng, intrct,
     +                  irg1, irg2, isup, nbmelm, nbmcav, idener,
     +                  idcoup, idtauq, iflgbm,
     +                  iucdef(mnmcav),iubdef(mnmbmi)
      save              /bmpmin/
      common /bmpmrl/   delq, bucket, si4i2, coupl, tauq,
     +                  exdata, fxbeam, fybeam, fctkhm, dspytl,
     +                  radius, energv, enerev, power, tauqs,
     +                  amasc2, alamda, game0, beamis(mnmbmi)
      save              /bmpmrl/
      common /bmpmel/   eleng, elang, elak1, elak2, ele1d, ele2d
      save              /bmpmel/
      common /bmpmcv/   cvvolt, cvpsi, cvfreq, cvleng, cvbtrf,
     +                  cvpow, cvshnt, cvfill, cvharm
      save              /bmpmcv/
      common /bmpmlg/   synrad, clorb, tousch, single, evary, polwig,
     +                  midarc, sym
      logical           synrad, clorb, tousch, single, evary, polwig,
     +                  midarc, sym
      save              /bmpmlg/
      common /bmpmch/   chname, chtype, chbvar(mnmbmi)
      save   /bmpmch/
      character*(mcnam) chname, chtype
      character*40      chbvar
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
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
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer i,ienum,ierr,iocc,ipos,jerr,k,l,ltemp,nnn,nrc
      double precision temp
 
      dimension temp(mnmcav)
      logical bmusrg
 
      character*(mcnam) elmnam
      character*8       chbnam(msbmrs)
      character * 60    stemp
 
      data chbnam/'beta_x','beta_y','brad_x','brad_y','sig_x*',
     +'sig_y*','RHOB','EC_synch','PW_synch','PHOTONS','BBARX',
     +'BBARY','PQX','PQY','PBX','PBY'/
 
      do 1 i = 1, mnmbmi
    1 beamis(i) = zero
 
*--- set defaults for unspecified variables
      if (iq(lq(lbmpm-1)+micmd+mptauq) .eq. 0) tauq = p6d2
      if (iq(lq(lbmpm-1)+micmd+mpdelq) .eq. 0) delq = p6dm2
      if (iq(lq(lbmpm-1)+micmd+mpxbsz) .eq. 0) fxbeam = one
      if (iq(lq(lbmpm-1)+micmd+mpybsz) .eq. 0) fybeam = one
      if (iq(lq(lbmpm-1)+micmd+mpkhm)  .eq. 0) fctkhm = zero
*--- TAUQ in [sec] rather than [min]
      tauqs  = sixty * tauq
      idcoup = iq(lq(lbmpm-1)+micmd+mpcoup)
      idtauq = iq(lq(lbmpm-1)+micmd+mptauq)
      iubdef(62) = iq(lq(lbmpm-1)+micmd+mpdelq)
      beamis(62) = delq
 
      ierr = 0
*--- store element numbers, count elements, get interaction point
      nbmelm = 0
      ihirng = 0
      ilorng = 0
      nrc = 0
      do 10  ipos = irg1, irg2
        call utelem(lcseq, ipos, iflgbm, elmnam, iocc, ienum)
        if (iq(lcelm+mbpr) .eq. mpelm)  then
*--- this is an element
          nbmelm = nbmelm + 1
          call bmgelm(lcelm, temp)
          if (temp(3) .ne. zero .and. temp(5) .ne. zero) then
*--- non-zero tilt
            ierr = 1
            jerr = nbmelm
            goto 999
          endif
          if (temp(2) * temp(3) .ne. zero .and. synrad)  then
            call aawarn('BMINI2', 1,
     +      'Gradient magnet suppresses synchrotron radiation.')
            synrad = .false.
          endif
          iq(lbmpm+nbmelm) = ipos
          if (ipos .ge. ilolim .and. ipos .le. ihilim)  then
            if (ilorng .eq. 0)  ilorng = nbmelm
            ihirng = nbmelm
          endif
          if (bmusrg(nbmelm, ilorng, ihirng, iflgbm))  nrc = nrc + 1
*--- RF cavity bank
          if (lq(lbmpm-2) .eq. 0)  then
*--- book bank
            call mzbook(1, l, lbmpm, -2, 'CAVS', 0, 0, 50, 0, -1)
            nbmcav = 0
          endif
          if (iq(lcelm+mbsp) .eq. 10) then
*--- RF cavity
            if (iq(lq(lbmpm-2)-1) .eq. nbmcav)  then
*--- push bank
              ltemp = lq(lbmpm-2)
              call mzpush(0, ltemp, 0, 50, 'I')
            endif
            nbmcav = nbmcav + 1
            iq(lq(lbmpm-2)+nbmcav) = ipos
          endif
        endif
   10 continue
*--- interaction element number
      if (intrct .eq. 0) then
        intrct = nbmelm
        if (iq(lq(lbmpm-1)+mncmd+mpintr) .ne. 0)  then
          call aawarn('BMINI2', 1, 'Invalid IP element set to #END.')
        else
          call aawarn('BMINI2', 1, 'Undefined IP element set to #END.')
        endif
      endif
*--- beta and disp. at interaction point
      call bmeget(intrct, nnn)
      if (disp(3) .gt. dspytl)  then
*--- fatal if vert. disp. is not zero
        ierr = 2
        jerr = intrct
        goto 999
      elseif (betx .eq. zero)  then
*--- fatal if betx = zero
        ierr = 3
        jerr = intrct
        goto 999
      elseif (bety .eq. zero)  then
*--- fatal if bety = zero
        ierr = 4
        jerr = intrct
        goto 999
      endif
*--- print warning if orbit at start not zero
      if (abs(orbit0(1)) + abs(orbit0(3)) .gt. p1dm9)  then
        stemp = 'non-zero orbit at start, x, y:'
        write(stemp(31:), '(2E14.6)')  orbit0(1), orbit0(3)
        call aawarn('BMINI2', 1, stemp)
      endif
*--- one-turn time, and turn frequency
      beamis(23) = circ / (betas * clight)
      beamis(24) = one / beamis(23)
*--- betas and etas at interaction point
      beamis(28) = betx
      beamis(29) = bety
      beamis(30) = disp(1)
      beamis(31) = disp(3)
      cvvolt = zero
      cvleng = zero
      cvpow  = zero
      power = zero
      if (nbmcav .ne. 0)  then
*--- set cavity parameters
*                      1 = cavity voltage [MV]
*                      2 = tuning angle [rad/2pi]
*                      3 = frequency [MHz]
*                      4 = length [m]
*                      5 = beta
*                      6 = power [MW]
*                      7 = shunt impedance  [MOhm]
*                      8 = unloaded filling time [micro-sec]
*                      9 = harmonic number
        do 20  i = 1, nbmcav
          call bmgcav(i, temp)
          if (temp(1) .gt. zero)  then
            cvvolt = cvvolt + temp(1)
            cvleng = cvleng + temp(4)
            cvpow  = cvpow  + temp(6)
            if (temp(4) .gt. zero)  then
              cvpsi  = twopi * temp(2)
              cvbtrf = temp(5)
              cvshnt = temp(7)
              cvfill = temp(8)
              cvfreq = temp(3)
              cvharm = temp(9)
            endif
          endif
   20   continue
        cvvolt = isup * cvvolt
        cvleng = isup * cvleng
        cvpow  = isup * cvpow
        if (cvshnt .gt. zero .and. cvleng .gt. zero ) then
*--- set total power
          power  = cvpow
*--- set flag that cavity values have been defined
          do 30  i = 1, mnmcav
   30     iucdef(i) = 1
*--- keep only active cavities if voltage given
          if (cvvolt .gt. zero)  then
            k = 0
            do 31  i = 1, nbmcav
              call bmgcav(i, temp)
              if (temp(1) .gt. 0 .and. temp(4) .gt. zero
     +        .and. temp(7) .gt. zero)  then
*--- cavity is kept if voltage, length, and impedance > 0
                k = k + 1
                iq(lq(lbmpm-2)+k) =iq(lq(lbmpm-2)+i)
              endif
   31       continue
            if (k .gt. 0)  then
              if (nbmcav .gt. k)  then
                stemp = '      inactive cavities supressed, leaving'
                write(stemp(:4), '(I4)')  nbmcav - k
                write(stemp(44:47), '(I4)')  k
                call aawarn('BMINI2', 1, stemp)
                nbmcav = k
              endif
            endif
          endif
        else
          call aawarn('BMINI2', 1, 'Cavity impedance or length'
     +    // ' not given --- POWER set to zero.')
        endif
      endif
*--- total cavity impedance
      beamis(32) = p1d6 * cvshnt * cvleng
*--- higher mode loss factor
      beamis(33) = p1d6 * beamis(23) * fctkhm / bunch
*--- tau_z
      beamis(34) = p1d6 * beamis(23) / (cvfill * bunch)
      if (midarc .and. .not. single)  beamis(34) = half * beamis(34)
*--- harmonic number
      if (iucdef(9) .eq. 0)
     +cvharm = int(p1d6 * beamis(23) * cvfreq / bunch + half) * bunch
*--- # of crossing points
      if (nint .eq. 0)  nint = two * bunch + p1dm6
*--- current - starting value from MAD
      if (currnt .ne. zero)  then
        iubdef(35) = 1
        beamis(35) = bunch * currnt
      endif
*--- RF voltage - starting value from MAD
      if (cvvolt .ne. zero)  then
        iubdef(36) = 1
        beamis(36) = p1d6 * cvvolt
      endif
*--- cavity phase angle and beta_rf
      if (cvpsi .ne. zero)  then
        iubdef(70) = 1
        beamis(70) = cvpsi
      endif
      if (cvbtrf .ne. zero)  then
        iubdef(71) = 1
        beamis(71) = cvbtrf
      endif
      if (power .gt. zero)  then
        iubdef(72) = 1
        beamis(72) = p1d6 * power
      endif
      if (nrc .gt. 0)  then
*--- book banks for results
        do 40  i = 1, msbmrs
   40   call mzbook(1, l, lbmrs, -i, chbnam(i), 0, 0, mwflt * nrc, 0, 0)
      endif
      if (cvharm .eq. zero)  then
        if (nbmcav .eq. 0)  then
          ierr = 5
        else
          ierr = 6
        endif
      endif
  999 end
