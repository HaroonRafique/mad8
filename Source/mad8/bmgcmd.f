      subroutine bmgcmd(ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Returns unpacked BMPM command parameters.                          *
*                                                                      *
*+++ Input and output via common blocks                                *
* Output:                                                              *
*                                                                      *
*   IERR      (integer) =0: OK, >0 : error in command                  *
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
      integer i,i1,i2,ierr
      double precision tval
 
      integer    itype(mgcmd), ind(micmd)
 
      logical    eflag, ldum(mlcmd)
 
      dimension  tval(mgcmd)
 
*--- integers
      nint = 0
      intrct = 0
*--- logicals
      synrad = .false.
      clorb  = .false.
      tousch = .false.
      single = .false.
      evary  = .false.
      polwig = .false.
      midarc = .false.
*--- start command decoding
      ierr = 0
*--- get flag whether var. read or not
      call utgtyp(lccmd, itype)
*--- integers
      call utgint(lccmd, 1, micmd, ind)
      if (itype(mpnint) .ne. 0) nint = ind(mpnint)
*--- reals
      dspytl = p1dm3
      call utgflt(lccmd, micmd+1, mrcmd, tval(micmd+1))
      if (itype(micmd+mpdelq) .ne. 0) delq   = tval(micmd+mpdelq)
      if (itype(micmd+mptauq) .ne. 0) tauq   = tval(micmd+mptauq)
      if (itype(micmd+mpbuck) .ne. 0) bucket = tval(micmd+mpbuck)
      if (itype(micmd+mpcoup) .ne. 0) coupl  = tval(micmd+mpcoup)
      if (itype(micmd+mpi4i2) .ne. 0) si4i2  = tval(micmd+mpi4i2)
      if (itype(micmd+mpexda) .ne. 0) exdata = tval(micmd+mpexda)
      if (itype(micmd+mpxbsz) .ne. 0) fxbeam = tval(micmd+mpxbsz)
      if (itype(micmd+mpybsz) .ne. 0) fybeam = tval(micmd+mpybsz)
      if (itype(micmd+mpkhm)  .ne. 0) fctkhm = tval(micmd+mpkhm)
      if (itype(micmd+mpytol) .ne. 0) dspytl = tval(micmd+mpytol)
*--- logicals
      call utglog(lccmd, mrcmd+1, mlcmd, ldum(mrcmd+1))
      if (itype(mrcmd+mpsynr) .ne. 0) synrad = ldum(mrcmd+mpsynr)
      if (itype(mrcmd+mpclor) .ne. 0) clorb  = ldum(mrcmd+mpclor)
      if (itype(mrcmd+mptous) .ne. 0) tousch = ldum(mrcmd+mptous)
      if (itype(mrcmd+mpsing) .ne. 0) single = ldum(mrcmd+mpsing)
      if (itype(mrcmd+mpevar) .ne. 0) evary  = ldum(mrcmd+mpevar)
      if (itype(mrcmd+mpmidc) .ne. 0) midarc = ldum(mrcmd+mpexda)
*--- names
*              empty
*--- ranges
      if (itype(mncmd+mpintr) .ne. 0) then
        call utgrng(lq(lccmd-(mncmd+mpintr)), lcseq, i1, i2, eflag)
        if (eflag)  then
          intrct = 0
        else
          intrct = i1
        endif
      endif
      if (itype(mncmd+mprang) .ne. 0) then
        call utgrng(lq(lccmd-(mncmd+mprang)), lcseq, ilolim, ihilim,
     +  eflag)
        if (eflag)  ierr = 1
      else
        ilolim = 1
        ihilim = 0
      endif
*--- checks of defaults etc.
      if (si4i2 .eq. zero .and. itype(micmd+mpi4i2) .ne. 0) then
        itype(micmd+mpi4i2) = 0
        call aawarn('BMGCMD',1,'Illegal user value I4I2 = 0 ignored.')
      endif
      if (exdata .eq. zero .and. itype(micmd+mpexda) .ne. 0) then
        itype(micmd+mpexda) = 0
        call aawarn('BMGCMD',1,'Illegal user value EXDATA = 0 ignored.')
      endif
      if (bucket .eq. zero .and. itype(micmd+mpbuck) .ne. 0) then
        itype(micmd+mpbuck) = 0
        call aawarn('BMGCMD',1,'Illegal user value BUCKET = 0 ignored.')
      endif
      if (tauq .eq. zero .and. itype(micmd+mptauq) .ne. 0) then
        itype(micmd+mptauq) = 0
        call aawarn('BMGCMD',1,'Illegal user value TAUQ = 0 ignored.')
      endif
      tauqs = sixty * tauq
      if (delq .eq. zero .and. itype(micmd+mpdelq) .ne. 0) then
        itype(micmd+mpdelq) = 0
        call aawarn('BMGCMD',1,'Illegal user value DELQ = 0 ignored.')
      endif
      if (fxbeam .eq. zero .and. itype(micmd+mpxbsz) .ne. 0) then
        itype(micmd+mpxbsz) = 0
        call aawarn('BMGCMD',1,'Illegal user value FX = 0 ignored.')
      endif
      if (fybeam .eq. zero .and. itype(micmd+mpybsz) .ne. 0) then
        itype(micmd+mpybsz) = 0
        call aawarn('BMGCMD',1,'Illegal user value FY = 0 ignored.')
      endif
*--- store ITYPE in bank as 0: not given, 1: given by user
      do 10  i = 1, mgcmd
   10 iq(lq(lbmpm-1)+i) = min(1, itype(i))
 
  999 end
