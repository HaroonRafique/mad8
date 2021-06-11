      subroutine irngen
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate the next "NR" elements in the pseudo-random sequence.     *
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
      integer irn,maxran,next,nr
 
*---- Generate pseudo-random integers in batches of NR.
      parameter         (nr = 55)
*     The random integers are generated in the range [0, MAXRAN).
      parameter         (maxran = 1 000 000 000)
      common /ranqzq/   next, irn(nr)
      save              /ranqzq/
      integer i,j,nj
 
      parameter         (nj = 24)
 
      do 10 i = 1, nj
        j = irn(i) - irn(i+nr-nj)
        if (j .lt. 0) j = j + maxran
        irn(i) = j
   10 continue
 
      do 20 i = nj + 1, nr
        j = irn(i) - irn(i-nj)
        if (j .lt. 0) j = j + maxran
        irn(i) = j
   20 continue
 
      next = 1
 
      end
