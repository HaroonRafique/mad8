      subroutine setbit(i, m, l)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Store bit at L into I'th bit of bit string M.                      *
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
      integer i,ibit,iword,l
      integer           m(*)
 
      iword = i / 32 + 1
      ibit  = mod(i-1,32) + 1
      call sbit(l, m(iword), ibit)
 
      end
