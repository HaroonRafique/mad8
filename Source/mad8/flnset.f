      subroutine flnset(strnam, filnam)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Define default file name.                                          *
*   System-dependent routine.                                          *
* Input:                                                               *
*   STRNAM    (char)    Stream name.                                   *
* Output:                                                              *
*   FILNAM    (char)    File name.                                     *
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
      character*(*)     strnam, filnam
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      filnam = strnam
 
      end
