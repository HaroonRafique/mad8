      subroutine grndm(gr1, gr2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return two floating point numbers from a Gaussian distribution     *
*   with zero mean and unit sigma.                                     *
* Output:                                                              *
*   GR1, GR2  (real)    Two random numbers.                            *
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
      double precision frndm,gr1,gr2,xi1,xi2,zzr
   10 continue
        xi1 = frndm() * 2. - 1.
        xi2 = frndm() * 2. - 1.
        zzr = xi1**2 + xi2**2
      if (zzr .gt. 1.) go to 10
*---- Transform accepted point to Gaussian distribution:
      zzr = sqrt(-2. * log(zzr)/zzr)
      gr1 = xi1 * zzr
      gr2 = xi2 * zzr
      end
