      function frndm()
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return a real pseudo-random number in the range [0,1).             *
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
      double precision frndm,scale
 
      parameter         (scale  = 1.0d0 / maxran)
      if (next .gt. nr) call irngen
      frndm = scale * irn(next)
      next = next + 1
      end
