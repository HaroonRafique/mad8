      subroutine flnfix(strnam, mode, filnam, leng, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Convert stream name to valid external file name.                   *
*   System dependent routine.                                          *
* Input:                                                               *
*   STRNAM    (char)    Stream name.                                   *
*   MODE      (char*4)  File mode.                                     *
* Output:                                                              *
*   FILNAM    (char)    External file name.                            *
*   LENG      (integer) Length of name for comparing.                  *
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
      integer len,leng
      character*(*)     strnam, filnam
      character*4       mode
      logical           eflag
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      eflag = .false.
      filnam = strnam
      leng = len(strnam)
 
      end
