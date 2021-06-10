      subroutine twchtp(iloc, elmnam, idisk)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TAPE option of TWISS command, CHROM option.                        *
* Input:                                                               *
*   LCSEQ     /REFER/   Current beam line sequence.                    *
*   LCELM     /REFER/   Current beam element.                          *
*   ILOC      (integer) Position code:                                 *
*                       1 = Beginning of system.                       *
*                       2 = After an element.                          *
*                       3 = Summary at end of system.                  *
*   ELMNAM    (char)    Name associated with current element.          *
*   IDISK     (integer) Logical unit for output                        *
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
      integer i,idisk,iloc
      double precision utwopi
      character*(*)     elmnam
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0 / (2.0 * pi))
 
*---- Begin of system: Write header record.
      if (iloc .eq. 1) call tphead(idisk, 'CHROM')
 
*---- After an element.
      if (iloc .le. 2) then
        call tpelem(elmnam, idisk)
        write (idisk, 810)
     +    wx, phix * utwopi, dmux * utwopi, ddisp(1), ddisp(2),
     +    wy, phiy * utwopi, dmuy * utwopi, ddisp(3), ddisp(4),
     +    (orbit(i), i = 1, 4), suml
      endif
 
  810 format(1p,5e16.9/5e16.9/5e16.9)
 
      end
