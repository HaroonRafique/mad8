      subroutine pa6brk(f, n1, g, n2, pb)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute Poisson bracket of two homogeneous polynomials.            *
*   The case that both orders are one is forbidden.                    *
* Source:     MARYLIE, version 5.1 (routine PBKT).                     *
* Input:                                                               *
*   F(*)      (poly)    Homogeneous polynomial of order N1.            *
*   G(*)      (poly)    Homogeneous polynomial of order N2.            *
* Output:                                                              *
*   PB(*)     (poly)    Poisson bracket [F,G], order = N1 + N2 - 2.    *
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
      integer i,i1,i2,indexp,indexq,lbasb,lbasp,lbasq,lindp,lindq,n1,n2
      double precision f,fp,fq,g,gp,gq,pb
      dimension         f(*), g(*), pb(*)
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
 
*---- Clear result array for resulting order.
      do 10 i = ibot6(n1+n2-2), itop6(n1+n2-2)
        pb(i) = 0.0
   10 continue
 
*==== Loop over variable pairs.
      do 300 indexq = 1, 5, 2
        indexp = indexq + 1
        lbasq = lq(lprd6-indexq)
        lbasp = lq(lprd6-indexp)
        lindq = lexp6(indexq)
        lindp = lexp6(indexp)
 
*==== N1 is greater or equal to N2.
        if (n1 .ge. n2) then
 
*---- Special case N2 = 1.
          if (n2 .eq. 1) then
            gp = g(indexp)
            if(gp .ne. 0.0) then
              do 110 i1 = ibot6(n1-1), itop6(n1-1)
                pb(i1) = pb(i1) +
     +            gp * f(iq(lbasq+i1)) * (iq(lindq+i1) + 1)
  110         continue
            endif
 
            gq = g(indexq)
            if(gq .ne. 0.0) then
              do 120 i1 = ibot6(n1-1), itop6(n1-1)
                pb(i1) = pb(i1) -
     +            gq * f(iq(lbasp+i1)) * (iq(lindp+i1) + 1)
  120         continue
            endif
 
*---- General case N2 > 1.
          else
            do 160 i2 = ibot6(n2-1), itop6(n2-1)
              lbasb = lq(lprd6-i2) - itop6(n2-2)
              gp = g(iq(lbasp+i2)) * (iq(lindp+i2) + 1)
              if(gp .ne. 0.0) then
                do 140 i1 = ibot6(n1-1), itop6(n1-1)
                  pb(iq(lbasb+i1)) = pb(iq(lbasb+i1)) +
     +              gp * f(iq(lbasq+i1)) * (iq(lindq+i1) + 1)
  140           continue
              endif
 
              gq = g(iq(lbasq+i2)) * (iq(lindq+i2) + 1)
              if(gq .ne. 0.0) then
                do 150 i1 = ibot6(n1-1), itop6(n1-1)
                  pb(iq(lbasb+i1)) = pb(iq(lbasb+i1)) -
     +              gq * f(iq(lbasp+i1)) * (iq(lindp+i1) + 1)
  150           continue
              endif
  160       continue
          endif
 
*==== N1 is less than N2.
        else
 
*---- Special case N1 = 1.
          if (n1 .eq. 1) then
            fq = f(indexq)
            if(fq .ne. 0.0) then
              do 210 i2 = ibot6(n2-1), itop6(n2-1)
                pb(i2) = pb(i2) +
     +            fq * g(iq(lbasp+i2)) * (iq(lindp+i2) + 1)
  210         continue
            endif
 
            fp = f(indexp)
            if(fp .ne. 0.0) then
              do 220 i2 = ibot6(n2-1), itop6(n2-1)
                pb(i2) = pb(i2) -
     +            fp * g(iq(lbasq+i2)) * (iq(lindq+i2) + 1)
  220         continue
            endif
 
*---- General case N1 > 1.
          else
            do 280 i1 = ibot6(n1-1), itop6(n1-1)
              lbasb = lq(lprd6-i1) - itop6(n1-2)
              fq = f(iq(lbasq+i1)) * (iq(lindq+i1) + 1)
              if(fq .ne. 0.0) then
                do 260 i2 = ibot6(n2-1), itop6(n2-1)
                  pb(iq(lbasb+i2)) = pb(iq(lbasb+i2)) +
     +              fq * g(iq(lbasp+i2)) * (iq(lindp+i2) + 1)
  260           continue
              endif
 
              fp = f(iq(lbasp+i1)) * (iq(lindp+i1) + 1)
              if(fp .ne. 0.0) then
                do 270 i2 = ibot6(n2-1), itop6(n2-1)
                  pb(iq(lbasb+i2)) = pb(iq(lbasb+i2)) -
     +              fp * g(iq(lbasq+i2)) * (iq(lindq+i2) + 1)
  270           continue
              endif
  280       continue
          endif
        endif
  300 continue
 
      end
