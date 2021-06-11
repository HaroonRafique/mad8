      subroutine flclos(iunit, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Close I/O stream.                                                  *
* Input:                                                               *
*   IUNIT     (integer) Logical unit number.                           *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
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
      integer ierr,iunit
      logical           eflag
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer iost,maxdef,maxfil
 
*---- Table of I/O streams, files and units.
***** Do not access this table directly from user programs *****
      parameter         (maxdef = 20, maxfil = 50)
      common /fltabc/   iofn(0:maxfil), ioac(0:maxfil), iodr(0:maxfil),
     +                  iofr(0:maxfil), iolc(0:maxfil)
      common /fltabi/   iost(0:maxfil)
      save              /fltabc/, /fltabi/
      character*(mcfil) iofn
      character*1       ioac, iodr,  iofr, iolc
 
*---- Check stream number in range.
      eflag = .false.
      if (iunit .le. 0  .or.  iunit .gt. maxfil) then
        write (msg, 910) iunit
        call aawarn('FLCLOS', 1, msg)
 
*---- File should be open.
      else if (iost(iunit) .eq. 0) then
        write (msg, 920) iunit, iofn(iunit)
        call aawarn('FLCLOS', 2, msg)
 
*---- Default stream must not be closed.
      else if (iunit .le. maxdef) then
        write (msg, 930) iunit, iofn(iunit)
        call aawarn('FLCLOS', 2, msg)
 
*---- If all OK, close file.
      else if (iost(iunit) .gt. 0) then
        ierr = 0
        close (unit = iunit, status = 'KEEP', iostat = ierr)
 
*---- Test for error.
        if (ierr .ne. 0) then
          write (msg, 940) iunit, ierr, iofn(iunit)
          call aafail('FLCLOS', 2, msg)
          eflag = .true.
 
*---- Record inactive state.
        else
          iost(iunit) = 0
          iodr (iunit) = ' '
        endif
      endif
 
  910 format('Unable to close stream number ',i3,
     +       ' --- number out of range.')
  920 format('Stream number ',i3,' already closed,'/
     +       'file name: ',a)
  930 format('Stream number ',i3,' cannot be closed,'/
     +       'file name: ',a)
  940 format('Unable to close stream number ',i3,', code = ',i3,','/
     +       'file name: ',a)
 
      end
