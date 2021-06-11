      subroutine mtbttk(plus)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track coupled lattice functions.                                   *
* Input:                                                               *
*   PLUS      (logical) True, to force positive phase advance.         *
*----------------------------------------------------------------------*
* Modified: 19-JUL-1999, T. Raubenheimer (SLAC)                        *
*   Added /SYNCH/ common and calculation of synchrotron integrals      *
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
      double precision a,adet,b,c,dt,tempa,tempb,twopi
      logical           plus
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
      double precision curlyh
 
      dimension         a(2,2), b(2,2), c(2,2), dt(6)
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0 * pi)
      double precision zero, one, two
      parameter         (zero  = 0.0d0, one = 1.0d0, two = 2.0d0)
 
*---- Synchrotron integrals before element.
      curlyh = disp(1)**2 * (one + alfx**2) / betx
     +       + two*disp(1)*disp(2)*alfx + disp(2)**2*betx
      synch_1 = synch_1 + disp(1) * rhoinv * blen/two
      synch_2 = synch_2 + rhoinv**2 * blen/two
      synch_3 = synch_3 + abs(rhoinv**3) * blen/two
      synch_5 = synch_5 + curlyh * abs(rhoinv**3) * blen/two
 
*---- Dispersion.
      do 20 i = 1, 6
        dt(i) = zero
        do 10 j = 1, 6
          dt(i) = dt(i) + re(i,j) * disp(j)
   10   continue
   20 continue
      call ucopy(dt, disp, 6*mwflt)
 
*---- Auxiliary matrices.
      a(1,1) = re(1,1) - (re(1,3) * rmat(1,1) + re(1,4) * rmat(2,1))
      a(1,2) = re(1,2) - (re(1,3) * rmat(1,2) + re(1,4) * rmat(2,2))
      a(2,1) = re(2,1) - (re(2,3) * rmat(1,1) + re(2,4) * rmat(2,1))
      a(2,2) = re(2,2) - (re(2,3) * rmat(1,2) + re(2,4) * rmat(2,2))
      b(1,1) = re(3,1) - (re(3,3) * rmat(1,1) + re(3,4) * rmat(2,1))
      b(1,2) = re(3,2) - (re(3,3) * rmat(1,2) + re(3,4) * rmat(2,2))
      b(2,1) = re(4,1) - (re(4,3) * rmat(1,1) + re(4,4) * rmat(2,1))
      b(2,2) = re(4,2) - (re(4,3) * rmat(1,2) + re(4,4) * rmat(2,2))
      c(1,1) = re(3,3) + (re(3,1) * rmat(2,2) - re(3,2) * rmat(2,1))
      c(1,2) = re(3,4) - (re(3,1) * rmat(1,2) - re(3,2) * rmat(1,1))
      c(2,1) = re(4,3) + (re(4,1) * rmat(2,2) - re(4,2) * rmat(2,1))
      c(2,2) = re(4,4) - (re(4,1) * rmat(1,2) - re(4,2) * rmat(1,1))
 
*---- Track R matrix.
      adet = a(1,1) * a(2,2) - a(1,2) * a(2,1)
      rmat(1,1) = - (b(1,1) * a(2,2) - b(1,2) * a(2,1)) / adet
      rmat(1,2) =   (b(1,1) * a(1,2) - b(1,2) * a(1,1)) / adet
      rmat(2,1) = - (b(2,1) * a(2,2) - b(2,2) * a(2,1)) / adet
      rmat(2,2) =   (b(2,1) * a(1,2) - b(2,2) * a(1,1)) / adet
 
*---- Mode 1.
      tempb = a(1,1) * betx - a(1,2) * alfx
      tempa = a(2,1) * betx - a(2,2) * alfx
      alfx = - (tempa * tempb + a(1,2) * a(2,2)) / (adet * betx)
      betx =   (tempb * tempb + a(1,2) * a(1,2)) / (adet * betx)
      amux = amux + atan2(a(1,2),tempb)
 
*---- Mode 2.
      tempb = c(1,1) * bety - c(1,2) * alfy
      tempa = c(2,1) * bety - c(2,2) * alfy
      alfy = - (tempa * tempb + c(1,2) * c(2,2)) / (adet * bety)
      bety =   (tempb * tempb + c(1,2) * c(1,2)) / (adet * bety)
      amuy = amuy + atan2(c(1,2),tempb)
 
*---- Synchrotron integrals after element.
      curlyh = disp(1)**2 * (one + alfx**2) / betx
     +       + two*disp(1)*disp(2)*alfx + disp(2)**2*betx
      synch_1 = synch_1 + disp(1) * rhoinv * blen/two
      synch_2 = synch_2 + rhoinv**2 * blen/two
      synch_3 = synch_3 + abs(rhoinv**3) * blen/two
      synch_5 = synch_5 + curlyh * abs(rhoinv**3) * blen/two
 
      end
