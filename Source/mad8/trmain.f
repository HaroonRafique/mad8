      subroutine trmain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Control routine for tracking module.                               *
* Input:                                                               *
*   IPR       (integer) Process code.                                  *
*   ISP       (integer) Subprocess code.                               *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
      integer ltrbuf,ltrcur,ltrfbf,ltrnoi,ltrnum,ltrobs,ltrrbf,ltrstt,
     +ltrtab,ltrtmp
 
*---- Links used for tracking.
      common /trlink/   ltrnum, ltrstt, ltrnoi, ltrfbf, ltrrbf, ltrobs,
     +                  ltrcur, ltrtmp, ltrtab, ltrbuf
      save              /trlink/
      integer ipr,isp,itrflg
 
      data itrflg       / 0 /
      save              itrflg
 
*--- reset title
      trktitle = 'Tracking'
*---- Initialize polynomial package and link area.
      call painit(6)
      if (itrflg .eq. 0) then
        call mzlink(0, '/TRLINK/', ltrnum, ltrcur, ltrbuf)
        itrflg = 1
        fstart = .false.
      endif
 
*---- Conventional tracking commands, must be in a TRACK block.
      if (isp .le. 20) then
        fdynap = .false.
 
*---- Check valid use of command.
        if (isp .gt. 1  .and.  imodul .ne. ipr) then
          msg(1) =
     +    'Cannot run tracking subcommand outside tracking process,'
          msg(2) = 'TRACK command required first.'
          call aafail('TRMAIN', 2, msg)
        else if (isp .le. 1  .and.  imodul .ne. 0) then
          msg(1) =
     +      'Cannot initiate tracking while previous process ' //
     +      'is not complete,'
          msg(2) = 'Enter proper ENDxxxx command first.'
          call aafail('TRMAIN', 2, msg)
 
*---- TRACK --- Enter tracking mode.
        else if (isp .eq. 1) then
          call trtrkcmd
          if (.not. error) imodul = ipr
 
*---- ENDTRACK --- Leave tracking module and drop noise data.
        else if (isp .eq. 2) then
          call trend
          imodul = 0
 
*---- RUN --- Perform tracking.
        else if (isp .eq. 3) then
          call trrun
 
*---- CONTINUE --- Continue tracking.
        else if (isp .eq. 4) then
          msg(1) = 'CONTINUE command is now identical to RUN,'
          msg(2) = 'Turns will be numbered from 1 to N.'
          call aawarn('TRMAIN', 2, msg)
          call trrun
 
*---- START --- Define initial conditions.
        else if (isp .eq. 5) then
          call trstrt
 
*---- TSAVE --- Save end conditions as initial conditions for next run.
        else if (isp .eq. 6) then
          call trsave
 
*---- NOISE --- Define noise on a given element.
        else if (isp .eq. 7) then
          call trnois
 
*---- OBSERVE --- Define observation point.
        else if (isp .eq. 8) then
          call trrobs
 
*---- User-defined command.
        else
          call usercm(ipr, isp)
        endif
 
*---- Tracking commands which initialise themselves.
      else
 
*---- DYNAPSTART --- Initial conditions for DYNAP.
        if (isp .eq. 21) then
          call trinicmd
 
*---- DYNAP --- Find dynamic aperture.
        else if (isp .eq. 22) then
          call trdyncmd
 
*---- User-defined command.
        else
          call usercm(ipr, isp)
        endif
      endif
 
      end
