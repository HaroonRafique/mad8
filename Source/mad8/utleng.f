      subroutine utleng(word, leng)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find last non-blank character in WORD.                             *
* Input:                                                               *
*   WORD      (char)    Word to be processed.                          *
* Output:                                                              *
*   LENG      (integer) Length of WORD (1 if all blanks).              *
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
      integer i,len,leng
      character*(*)     word
 
      leng = 1
      do 10 i = len(word), 1, -1
        if (word(i:i) .ne. ' ') then
          leng = i
          return
        endif
   10 continue
 
      end
