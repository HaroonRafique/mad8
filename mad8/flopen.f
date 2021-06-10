      subroutine flopen(strnam, mode, lrec, lfil, iunit, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Open I/O file and connect it to a stream.                          *
* Input:                                                               *
*   STRNAM    (char)    Stream name.                                   *
*   MODE      (char*4)  File mode.                                     *
*   LREC      (integer) Record length:                                 *
*                       For formatted files in characters,             *
*                       for unformatted files in ZEBRA words.          *
*   LFIL      (integer) File length in records (required on IBM).      *
* Output:                                                              *
*   IUNIT     (integer) Logical unit number.                           *
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
      integer iunit,leng,lfil,lrec
      character*(*)     strnam
      character*4       mode
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
 
      character*(mcfil) filnam
      character*1       acc, dir, frm, loc
 
*---- Unpack status flags.
      eflag = .false.
      acc = mode(1:1)
      dir = mode(2:2)
      frm = mode(3:3)
      loc = mode(4:4)
 
*---- Convert stream name to file name.
      call flnfix(strnam, mode, filnam, leng, eflag)
      if (eflag) go to 9999
 
*---- Check if attributes are consistent with previous use.
      do 50 iunit = 1, maxfil
        if (iofn(iunit)(1:leng) .eq. filnam(1:leng)) then
          if (iofr(iunit) .ne. frm) then
            write (msg, 910) 'format', iofr(iunit), frm
            call aafail('FLOPEN', 1, msg)
            eflag = .true.
          endif
          if (ioac(iunit) .ne. acc) then
            write (msg, 910) 'access', ioac(iunit), acc
            call aafail('FLOPEN', 1, msg)
            eflag = .true.
          endif
          filnam = iofn(iunit)
          go to 200
        endif
   50 continue
      if (eflag) go to 9999
 
*---- Not found, find free stream entry.
      do 70 iunit = maxdef + 1, maxfil
        if (iost(iunit) .lt. 0) go to 100
   70 continue
 
*---- No free entry found, take first closed entry.
      do 90 iunit = maxdef + 1, maxfil
        if (iost(iunit) .eq. 0) go to 100
   90 continue
 
*---- Stream table is full.
      call aafail('FLOPEN', 1, 'File name table full.')
      eflag = .true.
      go to 9999
 
*---- Fill in I/O unit table.
  100 continue
      iofn(iunit) = filnam
      ioac(iunit) = acc
      iodr(iunit) = ' '
      iofr(iunit) = frm
      iolc(iunit) = loc
      iost(iunit) = - 1
 
*---- Open file, unless already open.
  200 continue
      if (iost(iunit) .le. 0) then
        call flsyst(filnam, mode, lrec, lfil, iunit, eflag)
 
*---- Test for error.
        if (eflag) then
          if (dir .eq. 'R') then
            write (msg, 930) 'input', filnam
          else
            write (msg, 930) 'output', filnam
          endif
          call aafail('FLOPEN', 1, msg)
        else
          iost(iunit) = 1
          iodr(iunit) = dir
        endif
      else
        msg(1) = 'File already open: ' // filnam
        call aawarn('FLOPEN', 1, msg)
      endif
 
  910 format('Inconsistent ',a,' code. Old = ',a1,', new = ',a1)
  920 format('Inconsistent ',a,' length. Old = ',i3,', new = ',i3)
  930 format('Unable to open ',a,' file: ',a)
 
 9999 end
