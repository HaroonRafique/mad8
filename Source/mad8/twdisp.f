      subroutine twdisp(rt, vect, disp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initial values for dispersion or its first derivative by delta.    *
*   Only the first four components of the vectors are set.             *
* Input:                                                               *
*   RT(6,6)   (real)    One turn transfer matrix.                      *
*   VECT(6)   (real)    Right-hand side:                               *
*                       Column 6 of RT for dispersion,                 *
*                       Auxiliary vector for derivative of dipersion.  *
* Output:                                                              *
*   DISP(6)   (real)    Dispersion vector.                             *
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
      double precision a,disp,rt,vect
      dimension         rt(6,6), vect(6), disp(6)
 
      dimension         a(4,5)
 
      do 20 i = 1, 4
        do 10 j = 1, 4
          a(i,j) = rt(i,j)
   10   continue
        a(i,i) = a(i,i) - 1.0
        a(i,5) = - vect(i)
   20 continue
 
      call solver(a, 4, 1, irank)
 
      if (irank .ge. 4) then
        do 30 i = 1, 4
          disp(i) = a(i,5)
   30   continue
      else
        call aawarn('TWDISP', 1,
     +  'Unable to compute dispersion --- dispersion set to zero.')
        do 40 i = 1, 4
          disp(i) = 0.0
   40   continue
      endif
 
      end
