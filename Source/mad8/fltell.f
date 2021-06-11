      subroutine fltell
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   STATUS command: Print table of all files.                          *
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
      integer iunit
 
*---- List files.
      write (iqlog, 910)
      do 90 iunit = 1, maxfil
        if (iost(iunit) .ge. 0) then
          write (iqlog, 920) iunit, iofn(iunit), ioac(iunit),
     +                       iodr(iunit), iofr(iunit), iolc(iunit)
        endif
   90 continue
 
  910 format(' '/' FLTELL.  Known files:'/
     +       ' Stream',t16,'File name',t61,'  Status'/' ')
  920 format(' ',i6,t16,a,t61,4(' ',a1))
 
      end
