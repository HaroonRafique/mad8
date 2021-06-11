      subroutine fldele(iunit, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Delete I/O file.                                                   *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
*---- Check stream number in range.
      eflag = .false.
      if (iunit .le. 0  .or.  iunit .gt. maxfil) then
        write (msg, 910) iunit
        call aawarn('FLDELE', 1, msg)
 
*---- File must not be deleted already.
      else if (iost(iunit) .lt. 0) then
        write (msg, 920) iunit
        call aawarn('FLDELE', 2, msg)
 
*---- Default stream must not be deleted.
      else if (iunit .le. maxdef) then
        write (msg, 930) iunit, iofn(iunit)
        call aawarn('FLDELE', 2, msg)
 
*--- If all OK, delete file.
      else
 
*---- If file is closed, reopen it.
        ierr = 0
        if (iost(iunit) .eq. 0) then
          open (unit = iunit, file = iofn(iunit), status = 'UNKNOWN',
     +          iostat = ierr)
        endif
 
*---- If open successful, close and delete.
        if (ierr .eq. 0) then
          close (unit = iunit, status = 'DELETE', iostat = ierr)
        endif
 
*---- Test for error.
        if (ierr .ne. 0) then
          write (msg, 950) iunit, ierr, iofn(iunit)
          call aafail('FLDELE', 2, msg)
          eflag = .true.
 
*---- Delete file from table.
        else
          msg(1) = 'File deleted: ' // iofn(iunit)
          call aainfo('FLDELE', 1, msg)
          iost(iunit) = - 1
          iodr (iunit) = ' '
        endif
      endif
 
  910 format('Unable to delete stream number ',i3,
     +       ', number out of range.')
  920 format('Stream number ',i3,' already deleted,'/
     +       'file name: ',a)
  930 format('Stream number ',i3,' cannot be deleted,'/
     +       'file name: ',a)
  950 format('Unable to delete stream number ',i3,', code = ',i3/
     +       'file name: ',a)
 
      end
