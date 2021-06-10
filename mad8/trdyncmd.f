      subroutine trdyncmd
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Determine dynamic aperture; DYNAP command.                         *
*   No attributes.                                                     *
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
      integer iffreq,ipfreq,itrfil,npart,ntrack
 
*---- Common flags for tracking.
      common /trkchr/   trktitle
      character * 32    trktitle
      common /trkint/   ipfreq, iffreq, npart, ntrack, itrfil
      common /trktim/   time1, time2, dtime
      common /trkflg/   onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      save              /trkint/, /trktim/, /trkflg/
      real              time1, time2, dtime
      logical           onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      integer itrturns,ktrturns
      double precision chkbelow,deltax,dtune,dynapfrac,fracmin,smear,
     +trstep,tunx,tuny,wxmax,wxmin,wxstart,wxymax,wxymin,wxystart,wymax,
     +wymin,wystart,yapunov,zendyn,zstart
 
*---- Communication area for DYNAP command.
      common /trcdyn/   trknam(2)
      character*(mcnam) trknam
      common /trfdyn/   chkbelow, deltax, dtune, dynapfrac, fracmin,
     +                  smear, trstep, tunx, tuny,
     +                  wxmax, wxmin, wymax, wymin, wxymax, wxymin,
     +                  wxstart, wystart, wxystart, yapunov,
     +                  zstart(6), zendyn(6)
      common /tridyn/   itrturns, ktrturns
      common /trldyn/   fastune, lyapflag, orbflag
      logical           fastune, lyapflag, orbflag
      save              /trcdyn/, /trfdyn/, /tridyn/, /trldyn/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
*---- Should have seen a DYNAPSTART command.
      if (.not. fstart) then
        call aafail('DYNAP', 1, 'Requires a "DYNAPSTART" command.')
 
*---- Initialise tracking.
      else
        dorad  = frad
        dodamp = fdamp
        dorand = frand
        call trbegn(.false., orbflag)
 
*---- Perform tracking.
        call trdynrun
 
*---- Output.
        write (iqpr2, 930) dynapfrac,
     +                     dynapfrac*zstart(1),dynapfrac*zstart(2),
     +                     dynapfrac*zstart(3),dynapfrac*zstart(4),
     +                               zstart(5),          zstart(6),
     +                     trstep,
     +                               zendyn(1),          zendyn(2),
     +                               zendyn(3),          zendyn(4),
     +                               zendyn(5),          zendyn(6),
     +                               ktrturns,           smear,
     +                               wxymin,             wxymax
        if (fastune)  write (iqpr2, 940) dtune, tunx, tuny
        if (lyapflag) write (iqpr2, 950) yapunov
 
*---- Make parameters accessible to VALUE and PUSH.
        call aasetp('DYNAPFRAC', dynapfrac)
        call aasetp('TURNS',     ktrturns*1.d0)
        call aasetp('XEND',      zendyn(1))
        call aasetp('PXEND',     zendyn(2))
        call aasetp('YEND',      zendyn(3))
        call aasetp('PYEND',     zendyn(4))
        call aasetp('TEND',      zendyn(5))
        call aasetp('PTEND',     zendyn(6))
        call aasetp('WXMIN',     wxmin)
        call aasetp('WXMAX',     wxmax)
        call aasetp('WYMIN',     wymin)
        call aasetp('WYMAX',     wymax)
        call aasetp('WXYMIN',    wxymin)
        call aasetp('WXYMAX',    wxymax)
        call aasetp('SMEAR',     smear)
        if (fastune) then
          call aasetp('TUNX',    tunx)
          call aasetp('TUNY',    tuny)
          call aasetp('DTUNE',   dtune)
        endif
        if (lyapflag) then
          call aasetp('LYAPUNOV', yapunov)
        endif
      endif
 
  930 format(' '/' Fractional dynamic aperture:',
     +  t34,'DYNAPFRAC =',f14.12/
     +  'BEGIN X =',t10,f14.12,t41,'PX =',f14.12/
     +  '      Y =',t10,f14.12,t41,'PY =',f14.12/
     +  '      T =',t10,f14.12,t41,'PT =',f14.12/
     +  ' Fractional step for downward search:',t39,'STEP =',f14.12/
     +  'END   X =',t10,f14.12,t41,'PX =',f14.12/
     +  '      Y =',t10,f14.12,t41,'PY =',f14.12/
     +  '      T =',t10,f14.12,t41,'PT =',f14.12/
     +  ' Survival over:',t17,'TURNS =',i7,t38,'SMEAR =', f14.12/
     +  ' WXYMIN =',t10,f14.12,t37,'WXYMAX =',f14.12)
  940 format(' Fast tune determination:  DTUNE =',f14.12/
     +  '   TUNX =',t10,f14.12,t39,'TUNY =',f14.12)
  950 format(' Interpolated Lyapunov exponent:  LYAPUNOV =',f14.12)
 
      end
