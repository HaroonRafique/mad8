      subroutine utocnm(oldnam, iocc, newnam)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate a unique name from a name and an occurrence count.        *
* Input:                                                               *
*   OLDNAM    (char)    Name to be combined with occurrence count.     *
*   IOCC      (integer) Occurrence count.
* Output:                                                              *
*   NEWNAM    (char)    Generated name.                                *
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
      integer i,iocc,l
      character*(mcnam) oldnam, newnam
 
      character*8       number
 
      do 10 l = mcnam, 1, -1
        if (oldnam(l:l) .ne. ' ') go to 20
   10 continue
   20 write (number, '(I8)') iocc
      do 30 i = 1, 8
        if (number(i:i) .ne. ' ') go to 40
   30 continue
   40 l = min(l,i+mcnam-11)
      newnam = oldnam(1:l) // '[' // number(i:8) // ']'
 
      end
