      subroutine m66div(anum, aden, target, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   "Divide" matrices, i. e. postmultiply with inverse of denominator. *
* Input:                                                               *
*   ANUM(6,6)   (real)  "Numerator" matrix.                            *
*   ADEN(6,6)   (real)  "Denominator" matrix.                          *
* Output:                                                              *
*   TARGET(6,6) (real)  "Quotient" matrix: TARGET = ANUM * ADEN**(-1). *
*   EFLAG       (logical) Error flag.                                  *
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
      integer i,irank,j
      double precision aden,anum,augmat,target
      dimension         anum(6,6), aden(6,6), target(6,6)
      logical           eflag
 
      dimension         augmat(6,12)
 
*---- Copy input to local array.
      do 10 i = 1, 6
      do 10 j = 1, 6
        augmat(i,j)   = aden(i,j)
        augmat(i,j+6) = anum(i,j)
   10 continue
 
*---- Solve resulting system.
      call solver(augmat, 6, 6, irank)
      if (irank .lt. 6) then
        eflag = .true.
 
*---- Copy result.
      else
        eflag = .false.
        do 20 i = 1, 6
        do 20 j = 1, 6
          target(i,j) = augmat(i,j+6)
   20   continue
      endif
 
      end
