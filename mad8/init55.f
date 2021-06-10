      subroutine init55(iseed)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Using the given seed value, initialize the pseudo-random number    *
*   generator and load the array IRN with a set of random numbers in   *
*   [0, MAXRAN).                                                       *
* Input:                                                               *
*   ISEED     (integer) A seed value in the range [0..MAXRAN).         *
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
      integer i,ii,iseed,j,k,nd
 
      parameter         (nd = 21)
 
      j = mod(abs(iseed), maxran)
      irn(nr) = j
      k = 1
      do 10 i = 1, nr - 1
        ii = mod(nd*i, nr)
        irn(ii) = k
        k = j - k
        if (k .lt. 0) k = k + maxran
        j = irn(ii)
   10 continue
 
*---- Call IRNGEN a few times to "warm it up".
      call irngen
      call irngen
      call irngen
 
      end
