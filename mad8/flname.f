      subroutine flname(iunit, filnam)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return file name for logical unit IUNIT.                           *
* Input:                                                               *
*   IUNIT     (integer) Logical unit number.                           *
* Output:                                                              *
*   FILNAM    (char)    External file name.                            *
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
      integer iunit
      character*(*)     filnam
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
 
      if (iost(iunit) .ge. 0) then
        filnam = iofn(iunit)
      else
        filnam = ' '
      endif
 
      end
