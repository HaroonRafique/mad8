      subroutine m66exp(source, target, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   "Exponentiate" matrix.                                             *
*   Original author:    Liam Healy.                                    *
* Input:                                                               *
*   SOURCE(6,6) (real)  Input matrix.                                  *
* Output:                                                              *
*   TARGET(6,6) (real)  Output matrix: TARGET = exp(SOURCE).           *
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
      integer i,j
      double precision b,c,source,target
      dimension         source(6,6), target(6,6)
      logical           eflag
 
      dimension         b(6,6), c(6,6)
 
      call m66mpy(source, source, b)
      call m66mpy(source, b, c)
      do 20 j = 1, 6
        do 10 i = 1, 6
          b(i,j) = (source(i,j) - c(i,j) / 12.0) / 2.0
          c(i,j) = - b(i,j)
   10   continue
        b(j,j) = b(j,j) + 1.0
        c(j,j) = c(j,j) + 1.0
   20 continue
      call m66div(b, c, target, eflag)
 
      end
