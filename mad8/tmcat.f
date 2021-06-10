      subroutine tmcat(fsec, rb, tb, ra, ta, rd, td)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Concatenate two TRANSPORT maps.                                    *
*   This routine is time-critical and is carefully optimized.          *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   RB(6,6), TB(6,6,6)  Second map in beam line order.                 *
*   RA(6,6), TA(6,6,6)  First map in beam line order.                  *
* Output:                                                              *
*   RD(6,6), TD(6,6,6)  Result map.                                    *
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
      integer i1,i2,i3
      double precision ra,rb,rd,rw,ta,tb,td,ts,tw
      logical           fsec
      dimension         rb(6,6), tb(36,6), ra(6,6), ta(6,6,6)
      dimension         rd(6,6), td(6,6,6)
 
      dimension         rw(6,6), tw(6,6,6), ts(36,6)
 
*---- Transfer matrix.
      do 20 i2 = 1, 6
      do 20 i1 = 1, 6
        rw(i1,i2) = rb(i1,1) * ra(1,i2) + rb(i1,2) * ra(2,i2)
     +            + rb(i1,3) * ra(3,i2) + rb(i1,4) * ra(4,i2)
     +            + rb(i1,5) * ra(5,i2) + rb(i1,6) * ra(6,i2)
   20 continue
 
*---- Second order terms.
      if (fsec) then
        do 40 i3 = 1, 6
        do 40 i1 = 1, 36
          ts(i1,i3) = tb(i1,1) * ra(1,i3) + tb(i1,2) * ra(2,i3)
     +              + tb(i1,3) * ra(3,i3) + tb(i1,4) * ra(4,i3)
     +              + tb(i1,5) * ra(5,i3) + tb(i1,6) * ra(6,i3)
   40   continue
 
        do 60 i2 = 1, 6
        do 60 i3 = i2, 6
        do 60 i1 = 1, 6
          tw(i1,i2,i3) =
     +        rb(i1,1) * ta(1,i2,i3) + rb(i1,2) * ta(2,i2,i3)
     +      + rb(i1,3) * ta(3,i2,i3) + rb(i1,4) * ta(4,i2,i3)
     +      + rb(i1,5) * ta(5,i2,i3) + rb(i1,6) * ta(6,i2,i3)
     +      + ts(i1,   i2) * ra(1,i3) + ts(i1+ 6,i2) * ra(2,i3)
     +      + ts(i1+12,i2) * ra(3,i3) + ts(i1+18,i2) * ra(4,i3)
     +      + ts(i1+24,i2) * ra(5,i3) + ts(i1+30,i2) * ra(6,i3)
          tw(i1,i3,i2) = tw(i1,i2,i3)
   60   continue
      endif
 
*---- Copy result.
      call ucopy(rw, rd, 36*mwflt)
      if (fsec) call ucopy(tw, td, 216*mwflt)
 
      end
