      subroutine twbttk(chrom, plus)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track lattice functions, including chromatic effects.              *
* Input:                                                               *
*   CHROM     (logical) True, to track also chromatic functions.       *
*   PLUS      (logical) True, to force positive phase advance.         *
*----------------------------------------------------------------------*
 
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Scale by the energy change which is r(6,6)                         *
* Modified: 19-JUL-1999, T. Raubenheimer (SLAC)                        *
*   Added synchrotron integrals to calculations                        *
* Modified: 10-SEP-1999, M. Woodley (SLAC)                             *
*   Use the determinant of the longitudinal 2x2 part of the R-matrix   *
*   instead of R(6,6) for the energy scaling.                          *
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
      integer i,j,k
      double precision aux,auxp,ax1,ax2,ay1,ay2,bx1,bx2,by1,by2,proxim,
     +rep,t2,ta,tb,temp,tg,twopi,utwopi,x,y,zero,one,two,fre,frep,
     +curlyh,detl,f
      dimension         aux(6), auxp(6), rep(6,6)
      logical           chrom, plus
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
*---- Synchrotron integrals, etc.
      common /synch/    synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision  synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      dimension         fre(6,6), frep(6,6) ! MDW: for energy scaling
 
      parameter         (twopi = 2.0 * pi, utwopi = 1.0 / twopi)
      parameter         (zero = 0.0d0, one = 1.0d0, two = 2.0d0)
 
      proxim(x, y) = x + twopi * anint((y - x) * utwopi)
 
*---- Synchrotron integrals before element.
      curlyh = disp(1)**2 * (one + alfx**2) / betx
     +       + two*disp(1)*disp(2)*alfx + disp(2)**2*betx
      synch_1 = synch_1 + disp(1) * rhoinv * blen/two
      synch_2 = synch_2 + rhoinv**2 * blen/two
      synch_3 = synch_3 + abs(rhoinv**3) * blen/two
      synch_5 = synch_5 + curlyh * abs(rhoinv**3) * blen/two
 
*---- If CHROM, compute derivative of RE and track dispersion.
      if (chrom) then
        do 30 i = 1, 6
          aux(i) = zero
          auxp(i) = zero
          do 20 k = 1, 6
            temp = zero
            do 10 j = 1, 6
              temp = temp + te(i,j,k)*disp(j)
   10       continue
            aux(i) = aux(i) + re(i,k)*disp(k)
            auxp(i) = auxp(i) + temp*disp(k) + re(i,k)*ddisp(k)
            rep(i,k) = two*temp
   20     continue
   30   continue
        call ucopy(aux, disp, 6*mwflt)
        call ucopy(auxp, ddisp, 6*mwflt)
 
*---- Otherwise track dispersion only.
      else
        do 80 i = 1, 6
          aux(i) = zero
          do 70 k = 1, 6
            aux(i) = aux(i) + re(i,k)*disp(k)
   70     continue
   80   continue
        call ucopy(aux, disp, 6*mwflt)
      endif
 
*---- Tor: modified to cancel energy change
      disp(6) = one
 
*---- Tor/MDW: scale by square root of the determinant of the
*     longitudinal 2x2 part of the R-matrix
      detl = re(5,5)*re(6,6) - re(5,6)*re(6,5)
      f = one / sqrt(detl)
      call m66scl(f, re, fre)
      call m66scl(f, rep, frep)
 
*---- Track horizontal functions including energy scaling.
      if (stabx) then
        tb = fre(1,1)*betx - fre(1,2)*alfx
        ta = fre(2,1)*betx - fre(2,2)*alfx
        t2 = tb**2 + fre(1,2)**2
        tg = fre(1,1)*alfx - fre(1,2)*(one + alfx**2) / betx
 
*---- Linear functions.
        alfx = - (tb*ta + fre(1,2)*fre(2,2)) / betx
        betx = t2 / betx
        amux = amux + atan2(fre(1,2), tb)
*--- HG001025: line below commented out (creates integer phase jumps)
*        if (plus .and. fre(1,2) .lt. zero) amux = amux + twopi
 
*---- Chromatic functions.
        if (chrom) then
          bx1 = wx*cos(phix)
          ax1 = wx*sin(phix)
          bx2 = ((tb**2 - fre(1,2)**2)*bx1
     +        - two*tb*fre(1,2)*ax1) / t2
     +        + two*(tb*frep(1,1) - tg*frep(1,2)) / betx
          ax2 = ((tb**2 - fre(1,2)**2)*ax1
     +        + two*tb*fre(1,2)*bx1) / t2
     +        - (tb*(frep(1,1)*alfx + frep(2,1)*betx)
     +        - tg*(frep(1,2)*alfx + frep(2,2)*betx)
     +        + fre(1,1)*frep(1,2) - fre(1,2)*frep(1,1)) / betx
          wx = sqrt(ax2**2 + bx2**2)
          if (wx .gt. 1.0d-8) phix = proxim(atan2(ax2, bx2), phix)
          dmux = dmux + fre(1,2)*(fre(1,2)*ax1 - tb*bx1) / t2
     +         + (fre(1,1)*frep(1,2) - fre(1,2)*frep(1,1)) / betx
        endif
      endif
 
*---- Track vertical functions including energy scaling.
      if (staby) then
        tb = fre(3,3)*bety - fre(3,4)*alfy
        ta = fre(4,3)*bety - fre(4,4)*alfy
        t2 = tb**2 + fre(3,4)**2
        tg = fre(3,3)*alfy - fre(3,4)*(one + alfy**2) / bety
 
*---- Linear functions.
        alfy = - (tb*ta + fre(3,4)*fre(4,4)) / bety
        bety = t2 / bety
        amuy = amuy + atan2(fre(3,4), tb)
*--- HG001025: line below commented out (creates integer phase jumps)
*        if (plus .and. fre(3,4) .lt. zero) amuy = amuy + twopi
 
*---- Chromatic functions.
        if (chrom) then
          by1 = wy*cos(phiy)
          ay1 = wy*sin(phiy)
          by2 = ((tb**2 - fre(3,4)**2)*by1
     +        - two*tb*fre(3,4)*ay1) / t2
     +        + two*(tb*frep(3,3) - tg*frep(3,4)) / bety
          ay2 = ((tb**2 - fre(3,4)**2)*ay1
     +        + two*tb*fre(3,4)*by1) / t2
     +        - (tb*(frep(3,3)*alfy + frep(4,3)*bety)
     +        - tg*(frep(3,4)*alfy + frep(4,4)*bety)
     +        + fre(3,3)*frep(3,4) - fre(3,4)*frep(3,3)) / bety
          wy = sqrt(ay2**2 + by2**2)
          if (wy .gt. 1.0d-8) phiy = proxim(atan2(ay2, by2), phiy)
          dmuy = dmuy + fre(3,4)*(fre(3,4)*ay1 - tb*by1) / t2
     +         + (fre(3,3)*frep(3,4) - fre(3,4)*frep(3,3)) / bety
        endif
      endif
 
*---- Synchrotron integrals after element.
      curlyh = disp(1)**2 * (one + alfx**2) / betx
     +       + two*disp(1)*disp(2)*alfx + disp(2)**2*betx
      synch_1 = synch_1 + disp(1) * rhoinv * blen/two
      synch_2 = synch_2 + rhoinv**2 * blen/two
      synch_3 = synch_3 + abs(rhoinv**3) * blen/two
      synch_5 = synch_5 + curlyh * abs(rhoinv**3) * blen/two
 
      end
