      subroutine flassi
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*    Re-assign I/O streams, ASSIGN command.                            *
* Attributes:                                                          *
*    DATA     (string)  File name or TERM.                             *
*    ECHO     (string)  File name or TERM.                             *
*    PRINT    (string)  File name or TERM.                             *
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
      integer iunit
 
      character*(mcfil) filnam, strnam(3)
 
*---- Fetch data.
      strnam(1) = ' '
      strnam(2) = ' '
      strnam(3) = ' '
      call utgstr(lccmd, 1, 3, strnam)
 
*---- DATA stream.
      if (strnam(1) .ne. ' ') then
        call flopen(strnam(1), 'SRFD', 0, 0, iunit, error)
        if (.not. error  .and.  iqread .ne. iunit) then
          call flname(iunit, filnam)
          msg(1) = 'Now reading file: ' // filnam
          call aainfo('FLASSI', 1, msg)
          iqread = iunit
        endif
      endif
 
*---- ECHO stream.
      if (strnam(2) .ne. ' ') then
        if (iqlog .gt. 20) then
          call flname(iqlog, filnam)
          call aainfo('FLASSI', 1, 'Closing ECHO file: ' // filnam)
          call flclos(iqlog, error)
        endif
        if (strnam(2)(1:4) .eq. 'TERM') then
          call aainfo('FLASSI', 1,
     +      'ECHO stream rerouted to standard output.')
          iqlog = iqtype
          call aainfo('FLASSI', 1,
     +      'ECHO stream rerouted to standard output.')
        else
          call flopen(strnam(2), 'SWFD', 0, 0, iunit, error)
          if (.not. error  .and.  iqlog .ne. iunit) then
            call flname(iunit, filnam)
            msg(1) = 'ECHO stream rerouted to file: ' // filnam
            call aainfo('FLASSI', 1, msg)
            iqlog = iunit
            call aainfo('FLASSI', 1, msg)
          endif
        endif
        iqprnt = iqlog
      endif
 
*---- PRINT stream.
      if (strnam(3) .ne. ' ') then
        if (iqpr2 .gt. 20) then
          call flname(iqpr2, filnam)
          call aainfo('FLASSI', 1, 'Closing PRINT file: ' // filnam)
          call flclos(iqpr2, error)
        endif
        if (strnam(3)(1:4) .eq. 'TERM') then
          call aainfo('FLASSI', 1,
     +      'PRINT stream rerouted to standard output.')
          iqpr2 = iqtype
        else
          call flopen(strnam(3), 'SWFD', 0, 0, iunit, error)
          if (.not. error  .and.  iqpr2 .ne. iunit) then
            call flname(iunit, filnam)
            msg(1) = 'PRINT stream rerouted to file: ' // filnam
            call aainfo('FLASSI', 1, msg)
            iqpr2 = iunit
          endif
        endif
      endif
 
      end
