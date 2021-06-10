      subroutine tmcat1(fsec, eb, rb, tb, ea, ra, ta, ed, rd, td)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Concatenate two TRANSPORT maps including zero-order terms.         *
*   This routine is time-critical and is carefully optimized.          *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   EB(6), RB(6,6), TB(6,6,6)  Second map in beam line order.          *
*   EA(6), RA(6,6), TA(6,6,6)  First map in beam line order.           *
* Output:                                                              *
*   ED(6), RD(6,6), TD(6,6,6)  Result map.                             *
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
      integer i,ij,j,k
      double precision ea,eb,ed,es,ew,ra,rb,rd,rw,ta,tb,td,ts,tw,two
      logical           fsec
      dimension         eb(6), rb(6,6), tb(36,6)
      dimension         ea(6), ra(6,6), ta(6,6,6)
      dimension         ed(6), rd(6,6), td(6,6,6)
 
      parameter         (two = 2.0d0)
 
      dimension         ew(6), rw(6,6), tw(6,6,6), ts(36,6), es(6,6)
 
*---- Second order terms.
      if (fsec) then
 
*---- Auxiliary terms.
        do 30 k = 1, 6
 
*---- Sum over S of TB(I,S,K) * EA(S).
          do 10 i = 1, 6
            es(i,k) = tb(i   ,k) * ea(1) + tb(i+ 6,k) * ea(2)
     +              + tb(i+12,k) * ea(3) + tb(i+18,k) * ea(4)
     +              + tb(i+24,k) * ea(5) + tb(i+30,k) * ea(6)
   10     continue
 
*---- Sum over S of TB(I,J,S) * RA(S,K).
          do 20 ij = 1, 36
            ts(ij,k) = tb(ij,1) * ra(1,k) + tb(ij,2) * ra(2,k)
     +               + tb(ij,3) * ra(3,k) + tb(ij,4) * ra(4,k)
     +               + tb(ij,5) * ra(5,k) + tb(ij,6) * ra(6,k)
   20     continue
   30   continue
 
*---- Final values.
        do 90 k = 1, 6
 
*---- Zero-order terms.
          ew(k) = eb(k) + (rb(k,1) + es(k,1)) * ea(1)
     +                  + (rb(k,2) + es(k,2)) * ea(2)
     +                  + (rb(k,3) + es(k,3)) * ea(3)
     +                  + (rb(k,4) + es(k,4)) * ea(4)
     +                  + (rb(k,5) + es(k,5)) * ea(5)
     +                  + (rb(k,6) + es(k,6)) * ea(6)
 
*---- First-order terms.
          do 40 j = 1, 6
            rw(j,k) = (rb(j,1) + two * es(j,1)) * ra(1,k)
     +              + (rb(j,2) + two * es(j,2)) * ra(2,k)
     +              + (rb(j,3) + two * es(j,3)) * ra(3,k)
     +              + (rb(j,4) + two * es(j,4)) * ra(4,k)
     +              + (rb(j,5) + two * es(j,5)) * ra(5,k)
     +              + (rb(j,6) + two * es(j,6)) * ra(6,k)
   40     continue
 
*---- Second-order terms.
          do 60 j = k, 6
            do 50 i = 1, 6
              tw(i,j,k) =
     +          + (rb(i,1)+two*es(i,1))*ta(1,j,k) + ts(i   ,j)*ra(1,k)
     +          + (rb(i,2)+two*es(i,2))*ta(2,j,k) + ts(i+ 6,j)*ra(2,k)
     +          + (rb(i,3)+two*es(i,3))*ta(3,j,k) + ts(i+12,j)*ra(3,k)
     +          + (rb(i,4)+two*es(i,4))*ta(4,j,k) + ts(i+18,j)*ra(4,k)
     +          + (rb(i,5)+two*es(i,5))*ta(5,j,k) + ts(i+24,j)*ra(5,k)
     +          + (rb(i,6)+two*es(i,6))*ta(6,j,k) + ts(i+30,j)*ra(6,k)
              tw(i,k,j) = tw(i,j,k)
   50       continue
   60     continue
   90   continue
 
*---- Copy second-order terms.
        call ucopy(tw, td, 216*mwflt)
 
*---- Second-order not desired.
      else
        do 190 k = 1, 6
 
*---- Zero-order terms.
          ew(k) = eb(k) + rb(k,1) * ea(1) + rb(k,2) * ea(2)
     +                  + rb(k,3) * ea(3) + rb(k,4) * ea(4)
     +                  + rb(k,5) * ea(5) + rb(k,6) * ea(6)
 
*---- First-order terms.
          do 110 j = 1, 6
            rw(j,k) = rb(j,1) * ra(1,k) + rb(j,2) * ra(2,k)
     +              + rb(j,3) * ra(3,k) + rb(j,4) * ra(4,k)
     +              + rb(j,5) * ra(5,k) + rb(j,6) * ra(6,k)
  110     continue
  190   continue
      endif
 
*---- Copy zero- and first-order terms.
      call ucopy(ew, ed, 6*mwflt)
      call ucopy(rw, rd, 36*mwflt)
 
      end
