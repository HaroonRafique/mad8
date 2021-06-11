      subroutine chinit
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize character code translation tables.                      *
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
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer i,ic,ichar,j
 
      character*1       digit(0:9), lower(26), upper(26)
      data digit        / '0','1','2','3','4','5','6','7','8','9' /
      data lower        / 'a','b','c','d','e','f','g','h','i','j',
     +                    'k','l','m','n','o','p','q','r','s','t',
     +                    'u','v','w','x','y','z' /
      data upper        / 'A','B','C','D','E','F','G','H','I','J',
     +                    'K','L','M','N','O','P','Q','R','S','T',
     +                    'U','V','W','X','Y','Z' /
 
      do 10 i = 0, 255
        ichtyp(i) = 11
        ch2low(i) = char(i)
        ch2upp(i) = char(i)
   10 continue
 
      do 20 j = 0, 9
        ic = ichar(digit(j))
        ichtyp(ic) = j
   20 continue
 
      do 30 j = 1, 26
        ic = ichar(lower(j))
        ichtyp(ic) = 10
        ch2upp(ic) = upper(j)
        ic = ichar(upper(j))
        ichtyp(ic) = 10
        ch2low(ic) = lower(j)
   30 continue
 
      end
