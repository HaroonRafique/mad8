      subroutine hachrm
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Calculate chromaticity; HCHROM command (no attributes).            *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      double precision xix,xiy
 
      call hachcl(xix, xiy)
      if (iqlog .ne. iqpr2) write (iqlog, 910) xix, xiy
      write (iqpr2, 910) xix, xiy
 
  910 format(' '/' Chromaticity: Horizontal = ',f12.6,
     +       ',  vertical = ',f12.6/' ')
 
      end
