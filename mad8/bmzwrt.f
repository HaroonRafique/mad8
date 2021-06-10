      subroutine bmzwrt
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print final results.                                               *
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
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
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
      integer i,iret,iup,j,k,kbunch,l,mmpar,nharm,nline,nnn,npage,nrc
      double precision vec
 
      parameter (mmpar = 21)
 
      dimension vec(mmpar)
 
      integer ip(mmpar)
 
      logical usrng, dprint, bmusrg
 
      character*120     sline
      character * 160   sfmt1, sfmt2, sfout
      character * 60    stempt
      character*12      sltp(mmpar), sltx1(15), sltx2(9), sltx3(15),
     +                  sltx4(5), sltx5(3), sltx6(12), sltx7(3)
      character*13      sttp(mmpar), sttx1(15), sttx2(9), sttx3(15),
     +                  sttx4(5), sttx5(3), sttx6(12), sttx7(3)
      character*3       chuse(0:2), chint(8)
 
      data sfmt1 / '(1X, A12, '' = '', $, '' '', A13, A12, '' = '', $,
     +'' '', A13, A12, '' = '', $, '' '', A13)' /
      data sfmt2 / '(1X, A12, '' = '', I7,4X, '' '', A13, A12, '' = '',
     +$, '' '', A13, A12, '' = '', $, '' '', A13)' /
 
      data sltx1 /
     +'         Q_x', '     beta_x*', '       etax*',
     +'         Q_y', '     beta_y*', '       etay*',
     +'       alpha', '    circumf.', '         t_0',
     +'         J_x', '         J_y', '         J_e',
     +' dJ_x/(dE/E)', '        dE/E', '       df_RF'/
      data sttx1 /
     +' ', '[m]', '[m]', ' ', '[m]', '[m]', ' ', '[m]','[sec/turn]',
     +5 * ' ', '[Hz]'/
      data sltx2 /
     +'      energy', '    coupling', '     delta_Q',
     +'          U0', '       sig_E', '       B*rho',
     +'       tau_x', '       tau_y', '       tau_E'/
      data sttx2 /
     +'[GeV]',' ',' ','[MeV/turn]','dE/E','[Tm]',3 * '[sec]'/
      data sltx3 /
     +'       E_x_0', '       E_x_c', '       E_y_c',
     +'     sig_x_0', '     sig_x_c', '     sig_x_T',
     +'     sig_y_0', '     sig_y_c', '     sig_y_T',
     +'         L_x', '         n_x', '         I_x',
     +'         L_y', '         n_y', '         I_y'/
      data sttx3 /
     +3 * 'pi [micro-m]', 6 * '[mm]',
     +'[1/cm**2 sec]', 'per beam', '[A] / bunch',
     +'[1/cm**2 sec]', 'per beam', '[A] / bunch'/
      data sltx4 /
     +'     k_bunch', '     tau_pol', '   tau_brems',
     +'       n_int', 'pol_infinit.'/
      data sttx4 /
     +' ', '[min]', '[min]', ' ', 'percent'/
      data sltx5 /
     +'    f_RF/f_0', '    volt._RF', '        f_RF'/
      data sttx5 /
     +' ', '[MV]' ,'[MHz]'/
      data sltx6 /
     +'         phi', '         Q_s', '   f_synchr.',
     +'   sig_buck.', '       sig_s', '       tau_Q',
     +'  shunt imp.', '    L_cavity', '      t_fill',
     +'       power', '        K_hm', '    Touschek'/
      data sttx6 /
     +'[degrees]', ' ', '[kHz]', ' ', '[mm]', '[min]', '[MOhm/m]',
     +'[m]', '[microsec]', '[MW]', '[V/pC]', '[min]'/
      data sltx7 /
     +'     beta_RF', '         psi', '      bucket'/
      data sttx7 /
     +' ','[degrees]', ' '/
 
      data chuse / '(C)', '(D)', ' ' /
      data chint / 'I1', 'I2', 'I3', 'I4', 'I5', 'I6X', 'I6Y', 'I8' /
 
      dprint = iqlog .ne. iqpr2
 
*--- page and line count for print file
      npage = 1
      call prpage(iqpr2)
      write (iqpr2, 10070) ' BMPM output', npage
      call prline(iqpr2)
*--- print beam integrals
      do 10  i = 1, 8
        vec(i) = beamis(i)
        ip(i) = 2
   10 continue
      vec(8) = beamis(16)
      ip(4) = iq(lq(lbmpm-1)+micmd+mpi4i2)
      write (iqpr2, 10000) (chuse(ip(i)), chint(i), i = 1, 8)
      write (iqpr2, 10010) (vec(i), i = 1, 8)
      if (dprint)  then
        write (iqlog, 10000) (chuse(ip(i)), chint(i), i = 1, 8)
        write (iqlog, 10010) (vec(i), i = 1, 8)
      endif
*--- machine parameters
      do 20  i = 1, 15
        sltp(i) = sltx1(i)
        sttp(i) = sttx1(i)
   20 continue
      sltp(10)(7:9) = chuse(iubdef(25))
      sltp(12)(7:9) = chuse(iubdef(27))
      vec(1) = qx
      vec(2) = beamis(28)
      vec(3) = beamis(30)
      vec(4) = qy
      vec(5) = beamis(29)
      vec(6) = beamis(31)
      vec(7) = beamis(20)
      vec(8) = circ
      vec(9) = beamis(23)
      vec(10) = beamis(25)
      vec(11) = beamis(26)
      vec(12) = beamis(27)
      vec(13) = beamis(17)
      vec(14) = beamis(21)
      vec(15) = p1d6 * beamis(73)
      if (single) then
        sline = ' Global machine parameters ' // '(one beam), '
     +  // ' and local parameters at interaction point:'
      else
        sline = ' Global machine parameters ' // '(two beams),'
     +  // ' and local parameters at interaction point:'
      endif
      write (iqpr2, '(/A/)') sline
      if (dprint)  write (iqlog, '(/A/)') sline
      do 21  j = 1, 15, 3
        call bmzfmt(sfmt1, 11, 4, vec(j), sfout)
        write (iqpr2, sfout)  (sltp(i), vec(i), sttp(i), i = j, j + 2)
        if (dprint)
     +  write (iqlog, sfout)  (sltp(i), vec(i), sttp(i), i = j, j + 2)
   21 continue
*--- skip part of output if no cavities
      if (cvharm .eq. 0.)  goto 89
*--- beam parameters, part1
      do 30  i = 1, 9
        sltp(i) = sltx2(i)
        sttp(i) = sttx2(i)
   30 continue
      sltp(1)(4:6) = chuse(idener)
      sltp(2)(2:4) = chuse(idcoup)
      sltp(3)(3:5) = chuse(iubdef(62))
      vec(1) = energv
      vec(2) = coupl
      vec(3) = beamis(62)
      vec(4) = beamis(37)
      vec(5) = beamis(41)
      vec(6) = beamis(74)
      vec(7) = beamis(38)
      vec(8) = beamis(39)
      vec(9) = beamis(40)
      sline = ' Beam parameters and luminosities:'
      write (iqpr2, '(/A/)') sline
      if (dprint)  write (iqlog, '(/A/)') sline
      do 31  j = 1, 9, 3
        call bmzfmt(sfmt1, 11, 4, vec(j), sfout)
        write (iqpr2, sfout)  (sltp(i), vec(i), sttp(i), i = j, j + 2)
        if (dprint)
     +  write (iqlog, sfout)  (sltp(i), vec(i), sttp(i), i = j, j + 2)
   31 continue
*--- beam parameters, part2
      do 40  i = 1, 15
        sltp(i) = sltx3(i)
        sttp(i) = sttx3(i)
   40 continue
      sltp(2)(5:7)  = chuse(iubdef(45))
      sltp(12)(7:9) = chuse(iubdef(54))
      vec(1) = p1d6 * beamis(44)
      vec(2) = p1d6 * beamis(45)
      vec(3) = p1d6 * beamis(46)
      vec(4) = p1d3 * beamis(42)
      vec(5) = p1d3 * beamis(47)
      vec(6) = p1d3 * beamis(49)
      vec(7) = p1d3 * beamis(43)
      vec(8) = p1d3 * beamis(48)
      vec(9) = p1d3 * beamis(50)
      vec(10) = p1dm4 * beamis(58)
      vec(11) = beamis(56)
      vec(12) = beamis(54) / bunch
      vec(13) = p1dm4 * beamis(59)
      vec(14) = beamis(57)
      vec(15) = beamis(55) / bunch
      write (iqpr2, *) ' '
      if (dprint)  write (iqlog, *) ' '
      do 41  j = 1, 15, 3
        call bmzfmt(sfmt1, 11, 4, vec(j), sfout)
        write (iqpr2, sfout)  (sltp(i), vec(i), sttp(i), i = j, j + 2)
        if (dprint)
     +  write (iqlog, sfout)  (sltp(i), vec(i), sttp(i), i = j, j + 2)
   41 continue
*--- beam parameters, part3
      do 50  i = 1, 5
        sltp(i) = sltx4(i)
        sttp(i) = sttx4(i)
   50 continue
      kbunch = bunch + half
      vec(2) = beamis(76) / sixty
      vec(3) = beamis(63) / sixty
      vec(5) = beamis(15)
      write (iqpr2, *) ' '
      if (dprint)  write (iqlog, *) ' '
      call bmzfmt(sfmt2, 11, 4, vec(2), sfout)
      write (iqpr2, sfout)  sltp(1), kbunch, sttp(1),
     +(sltp(i), vec(i), sttp(i), i = 2, 3)
      if (dprint)  write (iqlog, sfout)  sltp(1), kbunch, sttp(1),
     +(sltp(i), vec(i), sttp(i), i = 2, 3)
      call bmzfmt(sfmt2, 11, 4, vec(5), sfout)
      write (iqpr2, sfout)  sltp(4), nint, sttp(4),
     +sltp(5), vec(5), sttp(5)
      if (dprint)  write (iqlog, sfout)  sltp(4), nint, sttp(4),
     +sltp(5), vec(5), sttp(5)
*--- RF parameters, part1
      do 60  i = 1, 3
        sltp(i) = sltx5(i)
        sttp(i) = sttx5(i)
   60 continue
      sltp(2)(2:4) = chuse(iubdef(36))
      nharm  = cvharm + half
      vec(2) = p1dm6 * beamis(36)
      vec(3) = cvfreq
      sline =
     +' RF related parameters (for a total of      cavities):'
      write(sline(40:43), '(I4)') nbmcav
      write (iqpr2, '(/A/)') sline
      if (dprint)  write (iqlog, '(/A/)') sline
      call bmzfmt(sfmt2, 11, 4, vec(2), sfout)
      write (iqpr2, sfout)  sltp(1), nharm, sttp(1),
     +(sltp(i), vec(i), sttp(i), i = 2, 3)
      if (dprint)  write (iqlog, sfout)  sltp(1), nharm, sttp(1),
     +(sltp(i), vec(i), sttp(i), i = 2, 3)
*--- RF parameters, part2
      do 70  i = 1, 12
        sltp(i) = sltx6(i)
        sttp(i) = sttx6(i)
   70 continue
      sltp(4)(1:3) = chuse(iubdef(51))
      sltp(6)(4:6) = chuse(idtauq)
      sltp(10)(5:7) = chuse(iubdef(72))
      vec(1) =rtodeg *  beamis(66)
      vec(2) = beamis(68)
      vec(3) = p1dm3 * beamis(69)
      vec(4) = beamis(51)
      vec(5) = p1d3 * beamis(67)
      vec(6) = tauqs / sixty
      vec(7) = cvshnt
      vec(8) = cvleng
      vec(9) = cvfill
      vec(10) = p1dm6 * beamis(72)
      vec(11) = fctkhm
 
      vec(12) = beamis(75) / sixty
      if (tousch)  then
        iup = 12
      else
        iup = 11
      endif
      do 71  j = 1, 12, 3
        call bmzfmt(sfmt1, 11, 4, vec(j), sfout)
        k = min(j+2, iup)
        write (iqpr2, sfout)  (sltp(i), vec(i), sttp(i), i = j, k)
        if (dprint)
     +  write (iqlog, sfout)  (sltp(i), vec(i), sttp(i), i = j, k)
   71 continue
*--- RF parameters, part3
      do 80  i = 1, 3
        sltp(i) = sltx7(i)
        sttp(i) = sttx7(i)
   80 continue
      sltp(1)(3:5) = chuse(iubdef(71))
      sltp(2)(7:9) = chuse(iubdef(70))
      sltp(3)(4:6) = chuse(iubdef(51))
      vec(1) = beamis(71)
      vec(2) = rtodeg * beamis(70)
      vec(3) = bucket
      call bmzfmt(sfmt1, 11, 4, vec(1), sfout)
      write (iqpr2, sfout)  (sltp(i), vec(i), sttp(i), i = 1, 3)
      if (dprint)  then
        write (iqlog, sfout)  (sltp(i), vec(i), sttp(i), i = 1, 3)
        write (iqlog, *) ' '
      endif
   89 continue
      if (ilorng .le. ihirng)  then
*--- element print-out
        nline = maxlin
        nrc = 0
        do 90  i = 1, nbmelm
          call bmeget(i, iret)
          usrng = bmusrg(i, ilorng, ihirng, iflgbm)
          if (usrng)  then
            if (nline + 1 .ge. maxlin)  then
              write(iqpr2, *) ' '
              npage = npage + 1
              call prpage(iqpr2)
              write (iqpr2, 10070) ' BMPM output', npage
              call prline(iqpr2)
              sline = ' '
              if (clorb)  then
                sline(17:) =
     +          '    PQX        PQY        PBX        PBY    '
              endif
              if (synrad)  then
                sline(61:) =
     +          '   rho       EC[keV]  power[W/m]  photons[/m*s*keV]'
              endif
              if (fxbeam .eq. one .and. fybeam .eq. one)  then
                stempt = ' '
              else
                stempt = ' attention - sig_x and sig_y factors:'
                write(stempt(41:), '(2G10.3)') fxbeam, fybeam
              endif
              write (iqpr2, 10040)  stempt
              nline = 4
              if (clorb .or. synrad)  then
                write(iqpr2, '(A)')  sline
                nline = nline + 1
              endif
              write(iqpr2, *) ' '
              nline = nline + 1
            endif
            call bmeget(i, nnn)
            nrc = nrc + 1
            do 100  j = 3, 10
              l = lq(lbmrs-j) + mwflt * (nrc-1) + 1
              call ucopy (q(l), vec(j-2), mwflt)
  100       continue
            do 110  j = 13, msbmrs
              l = lq(lbmrs-j) + mwflt * (nrc-1) + 1
              call ucopy (q(l), vec(j-4), mwflt)
  110       continue
            write(iqpr2, 10050) i, chname(:8), disp(1), disp(2),
     +      betx, bety, alfx, alfy, (vec(j), j = 1, 4)
            nline = nline + 1
            if (synrad .or. clorb)  then
              sline = ' '
              k = 11
              do 120  j = 1, 8
                k = k + 11
                sline(k:k) = '-'
  120         continue
              if (clorb .and. vec(9) .ne. zero)  then
                write (sline(17:), 10060) (vec(j), j = 9, 12)
              endif
              if (synrad .and. vec(5) .ne. zero)  then
                write (sline(61:), 10060) (vec(j), j = 5, 8)
              endif
              write (iqpr2, '(A)')  sline
              nline = nline + 1
            endif
          endif
   90   continue
      endif
10000 format(/' Synchrotron integrals:' // 8(5x, 2a3, 3x))
10010 format(1x, 1p, 8e14.4)
10040 format(//' Element range print-out:', a//
     +'   no.  name        eta        eta''      beta_x  ',
     +'   beta_y    alpha_x    alpha_y    sig_x      sig_y    ',
     +' sig_x''[rad]  sig_y''[rad]')
10050 format(1x, i5, 2x, a8, 1p, 10e11.4)
10060 format(1p, 4e11.4)
10070 format(a,t122,'page',i6)
 
      end
