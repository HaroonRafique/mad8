      subroutine suhead(lines, nline, npage)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Conditionally print page header for survey.                        *
* Input:                                                               *
*   LINES     (integer) Number of lines to be printed.                 *
*   NLINE     (integer) Current line position on page.                 *
*   NPAGE     (integer) Current page number.                           *
*   LINNAM    (char)    Beam line name.                                *
*   RNGNAM    (char)    Range name.                                    *
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
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer lenhed,lines,nline,npage
      double precision zero
 
      character*(*)     title
 
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
      parameter         (lenhed = 4, zero = 0.0d0)
      parameter         (title = 'Survey.')
 
      nline = nline + lines
      if (nline .gt. maxlin) then
        npage = npage + 1
        call prhead('SURVEY', title, zero, -1, nline, npage)
        nline = nline + lenhed + lines
        write (iqpr2, 910)
        call prline(iqpr2)
      endif
 
  910 format('       E L E M E N T   S E Q U E N C E       ',
     +       ' I            P O S I T I O N S            ',
     +       ' I               A N G L E S'/
     +       ' pos.  element occ.     sum(L)       arc     ',
     +       ' I     x             y             z       ',
     +       ' I     theta         phi           psi'/
     +       ' no.   name    no.      [m]          [m]     ',
     +       ' I     [m]           [m]           [m]     ',
     +       ' I     [rad]         [rad]         [rad]')
 
      end
