      subroutine hacell
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Control routine for HARMON minimization. HCELL command.            *
* Attributes:                                                          *
*   TOLERANCE (real)    Tolerance desired.                             *
*   CALLS     (integer) Call limit for penalty function.               *
* Desired values for penalty functions:                                *
*   QX',   QY'               First momentum derivatives of tunes.      *
*   QX'',  QY''              Second momentum derivatives of tunes.     *
*   QX''', QY'''             Third momentum derivatives of tunes.      *
*   DQXDEX, DQYDEY, DQYDEX   Anharmonicities.                          *
*   DX'I,  DX''I             Derivatives of dispersion (int. point).   *
*   BX'I,  BY'I              Derivatives of beta's     (int. point).   *
*   RXI,   RYI               Resonances                (int. point).   *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      double precision ensige,ensigx,ensigy
 
*---- Communication area for HARMON module.
      common /harchr/   lngnam, shtnam
      common /harflt/   ensigx, ensigy, ensige
      save              /harchr/, /harflt/
      character*(mcnam) lngnam, shtnam
      integer mhfun
      double precision hdes,hfac,hfun,hwei
 
*---- Data for minimization in HARMON.
      parameter         (mhfun = 21)
      common /hafbad/   hdes(mhfun), hfun(mhfun), hwei(mhfun),
     +                  hfac(mhfun)
      save              /hafbad/
      integer lhalbf,lhaltb,lhasbf,lhastb,mlcol,mscol
 
*---- Link area for HARMON module.
      parameter         (mlcol = 17, mscol = 12)
      common /halink/   lhaltb, lhalbf, lhastb, lhasbf
      save              /halink/
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer idiag,idx,ifjac,ifvec,ipvt,iqtf,iwa1,iwa2,iwa3,iwa4,ixvec,
     +ncalls
      double precision one,precis
 
      parameter         (precis = 1.0d-8)
      parameter         (one    = 1.0d0)
      external          hafcn
      logical           skip
 
*---- Retrieve attributes.
      tol = precis
      call utgflt(lccmd, 1, 1, tol)
      tol = max(tol,sqrt(epsmch))
      ncalls = 1000
      call utgint(lccmd, 2, 2, ncalls)
      call uzero(hdes, 1, mhfun * mwflt)
      call utgflt(lccmd, 3, 23, hdes)
 
*---- Coefficients for tune shifts.
      hfac(1) = sige * ensige * hwei(1)
      hfac(2) = sige * ensige * hwei(2)
      hfac(3) = (sige * ensige)**2 * hwei(3)
      hfac(4) = (sige * ensige)**2 * hwei(4)
      hfac(5) = (sige * ensige)**3 * hwei(5)
      hfac(6) = (sige * ensige)**3 * hwei(6)
      hfac(7) = (ex * ensigx**2) * hwei(7)
      hfac(8) = (ey * ensigy**2) * hwei(8)
      hfac(9) = (ex * ensigx**2) * hwei(9) / 2.0
 
*---- Coefficients for quantities at interaction point.
      hfac(10) = (sige * ensige)    * hwei(10)
      hfac(11) = (sige * ensige)**2 * hwei(11)
      hfac(12) = (sige * ensige)    * hwei(12)
      hfac(13) = (sige * ensige)    * hwei(13)
      hfac(14) = hwei(14) / 100.0
      hfac(15) = hwei(15) / 100.0
 
*---- Coefficients for quantities at symmetry point.
      hfac(16) = (sige * ensige)    * hwei(16)
      hfac(17) = (sige * ensige)**2 * hwei(17)
      hfac(18) = (sige * ensige)    * hwei(18)
      hfac(19) = (sige * ensige)    * hwei(19)
      hfac(20) = hwei(20) / 100.0
      hfac(21) = hwei(21) / 100.0
 
*---- Penalty before match.
      call haprnt('before')
 
*---- Print data.
      write (iqpr2, 910)
      write (iqpr2, 920) ex, ey, sige
      write (iqpr2, 930) ensigx, ensigy, ensige
 
*---- Any variable parameters?
      skip = .false.
      ncon = mhfun
      if (nvar .le. 0) then
        call aawarn('HACELL', 1,
     +  'Unable to adjust multipoles --- Need at least one variable.')
        skip = .true.
      endif
      if (skip .or. error .or. scan) go to 9999
 
*---- Allocate working space.
      ixvec  = iwork
      idx    = ixvec  + nvar
      ifvec  = idx    + nvar
      idiag  = ifvec  + ncon
      ifjac  = idiag  + nvar
      ipvt   = ifjac  + nvar * ncon
      iqtf   = ipvt   + nvar
      iwa1   = iqtf   + nvar
      iwa2   = iwa1   + nvar
      iwa3   = iwa2   + nvar
      iwa4   = iwa3   + nvar
      iwork  = iwa4   + ncon
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Call minimization routine.
      nfcnmx = ncalls
      nfcn = 0
      call mtgeti(nvar, dq(ixvec+1), dq(idx+1))
      call lmdif(hafcn, ncon, nvar, dq(ixvec+1), dq(ifvec+1),
     +           precis, dq(idiag+1), one,
     +           dq(ifjac+1), ncon, dq(ipvt+1), dq(iqtf+1),
     +           dq(iwa1+1), dq(iwa2+1), dq(iwa3+1), dq(iwa4+1))
 
*---- Penalty after match.
      call haprnt('after')
      write (msg, 940) fmin
      call aainfo('HACELL', 1, msg)
 
*---- Release working storage.
      iwork = ixvec
 
  910 format(' '/' Minimisation data')
  920 format(' ',14x,'Ex',14x,'Ey',8x,'sigma(p)'/' ',1p,3e16.6)
  930 format(' ',7x,'factor(x)',7x,'factor(y)',7x,'factor(p)'/
     +       ' ',3f16.3)
  940 format('Last value of penalty function: ',1p,e14.6)
 
 9999 end
