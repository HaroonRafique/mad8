      subroutine fltext(filnam, dir, lrec, iunit, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Open text file. System-dependent routine.                          *
* Input:                                                               *
*   FILNAM    (char)    File name.                                     *
*   DIR       (char*1)  Process direction: 'R' = read, 'W' = write.    *
*   LREC      (integer) Record length in characters.                   *
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
      integer ierr,iunit,lrec
      character*(*)     filnam
      character*1       dir
      logical           eflag
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
      character*12      form, stat
 
      form = 'FORMATTED'
      if (dir .eq. 'R') then
        stat = 'OLD'
      else
        stat = 'UNKNOWN'
      endif
      ierr = 0
*---- Standard open.
      open (unit = iunit, file = filnam, status = stat, form = form,
     +  access = 'SEQUENTIAL', iostat = ierr)
*---- Test for error.
      eflag = ierr .ne. 0
      if (eflag) then
        close (unit = iunit)
      else
        iofn(iunit) = filnam
        ioac(iunit) = 'S'
        iodr(iunit) = dir
        iofr(iunit) = 'F'
        iolc(iunit) = 'D'
        iost(iunit) = 1
      endif
 
      end
