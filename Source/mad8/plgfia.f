      subroutine plgfia(sin, ind, f, i, a)
      implicit none
************************************************************************
*
*   Purpose: get item (integer, floating point number, or string)
*
*-- Input:
*   SIN      input string
*-- Output:
*   IND      type: 1 integer, 2 floating, 3 exp. floating, 4 character
*            0 if input string is blank
*
************************************************************************
      integer i,ind,klch,l,lastnb
      double precision f
 
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      character * (*)  sin, a
 
      ind = 0
      if (sin .ne. ' ')  then
        l = lastnb(sin)
        if (sin(1:1) .eq. '"' .or. sin(1:1) .eq. '''')  then
          ind = 4
          a   = sin
        else
          call plgetv(sin, 1, l, i, f, ind, klch)
        endif
      endif
      end
