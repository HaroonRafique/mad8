      subroutine comain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Control routine for orbit corrections.                             *
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
      integer lcobuf,lcocor,lcoelm,lcomon,lcotab
 
*---- Links for closed orbit correction module.
      common /colink/   lcotab, lcobuf, lcocor, lcomon, lcoelm
      save              /colink/
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer icoflg,ipr,isp
 
      data icoflg       / 0 /
 
*---- Initialize working space pointers for use by COLORB and COLDIS.
      iwork = 0
      nwork = 0
 
*---- Initialize link area.
      if (icoflg .eq. 0) then
        call mzlink(0, '/COLINK/', lcotab, lcotab, lcoelm)
        icoflg = 1
      endif
 
*---- User-defined code.
      if (isp .lt. 1  .or.  isp .gt. 20) then
        call usercm(ipr, isp)
 
*---- CORRECT --- Complete C.O. Correction including dispersion.
      else if (isp .eq. 1) then
        call cocorr
 
*---- GETORBIT --- Read monitor readings on a TFS file.
      else if (isp .eq. 2) then
        call cormon
 
*---- PUTORBIT --- Write monitor readings on a TFS file.
      else if (isp .eq. 3) then
        call cowmon
 
*---- GETKICK --- Read corrector settings on a TFS file.
      else if (isp .eq. 4) then
        call corkik
 
*---- PUTKICK --- Write corrector settings on a TFS file.
      else if (isp .eq. 5) then
        call cowkik
 
*---- MICADO --- Perform one MICADO step.
      else if (isp .eq. 6) then
        call comica
 
*---- GETDISP --- Read dispersion readings on a TFS file.
      else if (isp .eq. 7) then
        call cordis
 
*---- PUTDISP --- Write dispersion readings on a TFS file.
      else if (isp .eq. 8) then
        call cowdis
 
*---- USEKICK --- Activate/deactivate correctors.
      else if (isp .eq. 9) then
        call coukik
 
*---- USEMONITOR --- Activate/deactivate monitors.
      else if (isp .eq. 10) then
        call coumon
 
*---- COGUESS --- Enter starting vector + tolerance for c.o. calc.
      else if (isp .eq. 11) then
        call cogues
      endif
 
*---- Reset working space pointers.
      call mzwork(0, dq(1), dq(1), - 1)
      iwork = 0
      nwork = 0
 
      end
