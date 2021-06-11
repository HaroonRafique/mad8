      subroutine flsyst(filnam, mode, lrec, lfil, iunit, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   System-dependent part of file open operation.                      *
* Input:                                                               *
*   FILNAM    (char)    File name.                                     *
*   MODE      (char*4)  File mode.                                     *
*   LREC      (integer) Record length in characters.                   *
*   LFIL      (integer) File length in records.                        *
*   IUNIT     (integer) Logical unit number.                           *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
*----------------------------------------------------------------------*
* Modified: 30-APR-1999, M. Woodley (SLAC)                             *
*   NT and unix (AIX) specific code delineated with *WNT or *AIX       *
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
      integer ierr,irecl,iunit,lfil,lrec
      character*(*)     filnam
      character*4       mode
      logical           eflag
 
      character*12      form, stat
      character*1       acc, dir, frm, loc
 
*---- Unpack status flags.
      eflag = .false.
      acc = mode(1:1)
      dir = mode(2:2)
      frm = mode(3:3)
      loc = mode(4:4)
      if (dir .eq. 'R') then
        stat = 'OLD'
      else
        stat = 'UNKNOWN'
      endif
      if (frm .eq. 'F') then
        form = 'FORMATTED'
      else
        form = 'UNFORMATTED'
      endif
      ierr = 0
*---- Standard unix systems specify record length in bytes.
      if (acc .eq. 'D') then
        irecl = 4 * lrec
        open (unit = iunit, file = filnam, status = stat, form = form,
     +    access = 'DIRECT', recl = irecl, iostat = ierr)
      else
        open (unit = iunit, file = filnam, status = stat, form = form,
     +    access = 'SEQUENTIAL', iostat = ierr)
      endif
 
*---- Test for error.
      eflag = ierr .ne. 0
      if (eflag) then
        close (unit = iunit)
      endif
 
      end
