      subroutine emnorm
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute emittances by A. Chao's method, and track eigenvectors.    *
*   EMIT command.                                                      *
* Attributes:                                                          *
*   ORDER     (integer) Order of map (ignored).                        *
*   RFCAVITY  (name)    RF cavity to be adjusted (ignored).            *
*   DELTAP    (real)    Average relative energy error.                 *
*   EIGEN     (logical) Request eigenvectors.                          *
*   TWISS     (logical) Request Mais-Ripken functions.                 *
*   BEAM      (logical) Request Beam envelope.                         *
*   TABLE     (name)    Request eigenvector table.                     *
*----------------------------------------------------------------------*
* Modified: 30-NOV-1998, M. Woodley (SLAC)                             *
*   Add two (dummy) arguments to call to EMENGO (support for tape file *
*   output from ENVELOPE command)                                      *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer nlines
      double precision deltap
 
      integer           itype(7), jtype(3)
      logical           flag(3)
      character*(mcnam) blank, tabnam
 
      data blank        / ' ' /
 
*---- Retrieve option flags.
      call utgtyp(lccmd, itype)
      deltap = 0.0
      call utgflt(lccmd, 3, 3, deltap)
      flag(1) = .false.
      flag(2) = .false.
      flag(3) = .false.
      call utglog(lccmd, 4, 6, flag)
      tabnam = blank
      call utgnam(lccmd, 7, 7, tabnam)
 
*---- Ignored attributes.
      if (itype(1) .ne. 0  .or.  itype(2) .ne. 0) then
        call aawarn('EMNORM', 1,
     +    'Attributes "ORDER" and/or "RFCAVITY" ignored.')
      endif
 
*---- Build warning message.
      msg(1) = 'Command "NORMAL" is now obsolete, using the sequence:'
      if (itype(3) .eq. 0) then
        msg(2) = '     EMIT'
      else
        write (msg(2), 910) deltap
      endif
      nlines = 2
      if (flag(1)) then
        nlines = nlines + 1
        if (itype(7) .eq. 0) then
          msg(nlines) = '     EIGEN'
        else
          write (msg(nlines), 920) tabnam
        endif
      endif
      if (flag(2)) then
        nlines = nlines + 1
        msg(nlines) = '     TWISS3'
      endif
      if (flag(3)) then
        nlines = nlines + 1
        msg(nlines) = '     ENVELOPE'
      endif
      call aawarn('EMNORM', nlines, msg)
 
*---- Execute command sequence.
      jtype(1) = 0
      jtype(2) = 0
      jtype(3) = 0
      call ememgo(deltap)
      if (flag(1)) call emevgo(tabnam)
      if (flag(2)) call emtwgo(jtype, blank, blank, 3)
      if (flag(3)) call emengo(jtype, blank, blank, 3, .false., 0)
 
  910 format('     EMIT, DELTAP = ',f10.6)
  920 format('     EIGEN, SAVE = ',a)
 
      end
