      subroutine cocorr
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Complete closed orbit correction by MICADO algorithm.              *
*   Optionally also corrects dispersion.                               *
* CORRECT command:                                                     *
*   ERROR     (real)    Closed orbit quality desired.                  *
*   NCORR     (integer) Number of correctors allowed.                  *
*   NITER     (integer) Number of iterations allowed.                  *
*   C1LIST    (name)    Corrector list desired before correction.      *
*   C2LIST    (name)    Corrector list desired after correction.       *
*   M1LIST    (name)    Monitor list desired before correction.        *
*   M2LIST    (name)    Monitor list desired after correction.         *
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      integer lcobuf,lcocor,lcoelm,lcomon,lcotab
 
*---- Links for closed orbit correction module.
      common /colink/   lcotab, lcobuf, lcocor, lcomon, lcoelm
      save              /colink/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mmaxel,mwind,ndccnt,ndflag,nditer,ndocc,ndpos,ndtype,
     +nlpos
      double precision admatr,adorbt,adsuml,adtol,orbkpt,reforb,skpt
*--- common block for threader variables
      parameter (mwind = 500, mmaxel = 20000)
      common/thrcml/adthfl, adwofl, adcofl
      logical adthfl, adwofl, adcofl
      common/thrcmi/ndccnt, ndocc, nlpos, nditer,
     +ndpos(mwind), ndtype(mwind), ndflag(mmaxel)
      common/thrcmr/adtol(6), reforb(6), adsuml(mwind),
     +adorbt(6,mwind), admatr(6,6,mwind), orbkpt(6,mmaxel),
     +skpt(mmaxel)
      integer i,ilc1,ilc2,ilm1,ilm2,jiter,jpl,jplmax,jplmin,niter,nkick,
     +nlist,nok
      double precision adltol,drms,rrms,zero
 
      parameter         (nlist = 3, zero = 0.0d0)
 
      character*(mcnam) dlist(nlist), clist(4), cplane
      character*1       plane(2)
      dimension         drms(2), rrms(2)
      integer           ilist(2)
      logical           ok
      dimension adltol(6)
 
      data dlist        / 'NONE', 'USED', 'ALL' /
      data plane        / 'x', 'y' /
 
      data adltol / 1.d-3, 1.d-5, 1.d-3, 1.d-5, 1.d0, 1.d-1 /
 
*---- set flag for calls from COCORR
      adcofl = .true.
*---- Check main beam line.
      call lnchck('CORRECT', error)
      if (error) go to 800
 
*---- Extract data.
      qual = 1.0d-6
      call utgflt(lccmd, 1, 1, qual)
 
      ilist(1) = 0
      ilist(2) = 1
      call utgint(lccmd, 2, 3, ilist)
      nkick = ilist(1)
      niter = ilist(2)
 
      clist(1) = ' '
      clist(2) = ' '
      clist(3) = ' '
      clist(4) = ' '
      call utgnam(lccmd, 4, 7, clist)
      call utlook(clist(1), dlist, nlist, ilc1)
      call utlook(clist(2), dlist, nlist, ilc2)
      call utlook(clist(3), dlist, nlist, ilm1)
      call utlook(clist(4), dlist, nlist, ilm2)
 
      weight(1) = 0.0
      weight(2) = 0.0
      call utgflt(lccmd, 8, 9, weight)
 
      cplane = ' '
      call utgnam(lccmd, 10, 10, cplane)
      jplmin = 1
      jplmax = 2
      if (cplane .eq. 'X') then
        jplmax = 1
      else if (cplane .eq. 'Y') then
        jplmin = 2
      endif
*---- Threader count and tolerances
      nditer = 10000
      ndccnt = 0
      adthfl = .false.
      adwofl = .false.
      do 10 i = 1,6
   10 adtol(i) = adltol(i)
      call utgflt(lccmd, 11, 16, adtol)
      call utglog(lccmd, 17, 17, adthfl)
      call utglog(lccmd, 18, 18, adwofl)
      call utgint(lccmd, 19, 19, nditer)
*---- Set up corrector and monitor table.
      call cotble(error)
      if (error) go to 800
 
*---- Find closed orbit.
      call tmturn(lcseq, zero, error)
      if (error) go to 800
 
*---- Fill in monitor readings and dispersion.
      call cofill
      if (error) go to 800
 
*---- Suppress calculation, if no iterations wanted.
      if (niter .le. 0) then
        call copkik(max(ilc1, ilc2), '(no correction made)')
        call copdis(max(ilm1, ilm2), '(no correction made)', rrms, drms)
 
*---- Print corrector and monitor tables before correction.
      else
        call copkik(ilc1, '(before correction)')
        call copdis(ilm1, '(before correction)', rrms, drms)
 
*---- Iteration loop for non-linear problem.
        do 100 jiter = 1, niter
          nok = 0
          do 90 jpl = jplmin, jplmax
            if (weight(jpl) .eq. zero) then
              write (iqlog, 910) jiter, plane(jpl)
              call colorb(nkick, jpl, ok)
            else
              write (iqlog, 920) jiter, plane(jpl), weight(jpl)
              call coldis(nkick, jpl, ok)
            endif
            if (ok) nok = nok + jpl
   90     continue
          if (nok .eq. 3) go to 200
 
*---- Find new closed orbit and monitor readings.
          call tmturn(lcseq, zero, error)
          if (error) go to 800
 
*---- Fill in monitor readings and dispersion.
          call cofill
          if (error) go to 800
  100   continue
 
*---- Print corrector and monitor tables after correction.
  200   continue
        call copkik(ilc2, '(after correction)')
        call copdis(ilm2, '(after correction)', rrms, drms)
        write (iqlog, 930) niter, rrms, drms
      endif
      error = .false.
  800 continue
*---- reset flag for calls from COCORR
      adcofl = .false.
 
  910 format(' '/t11,'Beginning orbit iteration ',i4,' for plane ',a1,
     +       '.')
  920 format(' '/t11,'Beginning orbit and dispersion iteration ',i3,
     +       ' for plane ',a1,','/
     +       t11,'Weight for dispersion = ',f12.6)
  930 format(t11,'Remaining r.m.s. values after iteration ',i4,':'/
     +       t11,'Orbit:     ',t31,'<x> =  ',e16.6,' mm,',
     +       t71,'<y> =  ',e16.6,' mm.'/
     +       t11,'Dispersion:',t31,'<Dx> = ',e16.6,' m,',
     +       t71,'<Dy> = ',e16.6,' m.'/' ')
 
      end
