      subroutine trtbop(orb, em, tabnam, nturn, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Open a track table.                                                *
* Input:                                                               *
*   ORB(6)    (real)    Local position of closed orbit.                *
*   EM(6,6)   (real)    Eigenvector matrix.                            *
*   TABNAM    (char)    Name for track table.                          *
*   NTURN     (integer) Number of turns to be stored.                  *
*   KTRACK    (integer) Number of tracks to be stored.                 *
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
      integer i,j,maxcol,nb,nc,nr,ns,ktrack,nturn
      double precision dummy,em,orb
      character*(mcnam) tabnam
      dimension         orb(6), em(6,6)
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
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
      integer ltrbuf,ltrcur,ltrfbf,ltrnoi,ltrnum,ltrobs,ltrrbf,ltrstt,
     +ltrtab,ltrtmp
 
*---- Links used for tracking.
      common /trlink/   ltrnum, ltrstt, ltrnoi, ltrfbf, ltrrbf, ltrobs,
     +                  ltrcur, ltrtmp, ltrtab, ltrbuf
      save              /trlink/
      double precision aival,eigen,reval
 
*---- Initial conditions for optical functions for tracking.
      common /troptc/   eigen(6,6), reval(6), aival(6)
      save              /troptc/
 
      parameter         (maxcol = 8)
      integer           icform(maxcol)
      character*(mcnam) trkcol(maxcol)
      character*4       name
 
      data trkcol
     +  / 'TURNS', 'PARTICLE', 'X', 'PX', 'Y', 'PY', 'T', 'PT' /
 
      ns = nturn + 1
      nr = ktrack
      nc = maxcol
      nb = 1
      icform(1) = 2
      icform(2) = 2
      icform(3) = 3
      if (double) icform(3) = mreal
 
      do 10 i = 4, maxcol
        icform(i) = icform(3)
   10 continue
 
      call tbcrea(tabnam, ns, nr, nc, trkcol, icform, nb, ltrtab)
 
*---- Add descriptors in reverse order.
      do 30 i = 6, 1, -1
        do 20 j = 6, 1, -1
          write (name, '(''E'',I1,I1)') i, j
          call tbpdsc(ltrtab, name, mreal, 0, em(i,j), ' ')
   20   continue
   30 continue
 
      call tbpdsc(ltrtab, 'EX',   mreal, 0, ex,     ' ')
      call tbpdsc(ltrtab, 'EY',   mreal, 0, ey,     ' ')
      call tbpdsc(ltrtab, 'ET',   mreal, 0, et,     ' ')
      call tbpdsc(ltrtab, 'PY',   mreal, 0, orb(4), ' ')
      call tbpdsc(ltrtab, 'Y',    mreal, 0, orb(3), ' ')
      call tbpdsc(ltrtab, 'PX',   mreal, 0, orb(2), ' ')
      call tbpdsc(ltrtab, 'X',    mreal, 0, orb(1), ' ')
      call tbpdsc(ltrtab, 'TYPE', 5, 0, dummy, 'TRACK')
 
      end
