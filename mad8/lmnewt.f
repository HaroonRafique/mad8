      subroutine lmnewt(nord, df, ndim, zi, zf)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Apply a canonical transformation, defined by the derivatives of    *
*   its generating function, to a phase space vector.                  *
*     DF contains the derivatives of F w.r.t. the six variables,       *
*     and the polynomial order of F is NORD.                           *
* Source:     MARYLIE, version 5.1 (routine NEWT).                     *
* Authors:    Liam Healy and Philippo Neri.                            *
* Input:                                                               *
*   NORD      (integer) Order of the generating function F.            *
*   DF(NDIM,6)(poly)    The six derivatives of F.                      *
*   ZI(6)     (real)    Input phase space vector.                      *
* Output:                                                              *
*   ZF(6)     (real)    Final phase space vector.                      *
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
      integer i,ipind,iqind,irank,itra,j,jord,n,ndim,nord
      double precision df,pvec,qvec,rdf,rjac,rmat,root,square,vect,zf,
     +zi
      dimension         df(ndim,6), zi(6), zf(6)
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
      integer ibot3,itop3,lexp3,lind31,lind32,lprd3
 
*---- Bookkeeping tables for polynomials of three variables.
      common /pa3lnk/   ibot3(-6:6), itop3(-6:6), lexp3(3),
     +                  lind31, lind32, lprd3
      save              /pa3lnk/
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
 
      dimension         pvec(0:55), qvec(0:55)
      dimension         rdf(0:55,3), rjac(0:35,3,3)
      dimension         rmat(3,4), vect(461)
      equivalence       (pvec(0), qvec(0), vect(1))
 
*---- Initialize.
      do 10 i = 1, 6
        zf(i) = zi(i)
   10 continue
 
*---- Basis monomials in Q.
      qvec(0) = 1.0
      qvec(1) = zi(1)
      qvec(2) = zi(3)
      qvec(3) = zi(5)
      do 30 jord = 2, nord-1
        do 20 i = ibot3(jord), itop3(jord)
          qvec(i) = qvec(iq(lind31+i)) * qvec(iq(lind32+i))
   20   continue
   30 continue
 
*---- Coefficients of RDF (reduced DF), function of P only.
      do 190 i = 1, 3
 
*---- Clear the table.
        do 110 j = 0, itop3(nord-1)
          rdf(j,i) = 0.0
  110   continue
 
*---- Add up the monomial coefficients.
*     (Note: This loop contains vector dependencies).
        do 130 j = 1, itop6(nord-1)
          rdf(iq(larrp+j),i) = rdf(iq(larrp+j),i) +
     +      qvec(iq(larrq+j))*df(j,2*i-1)
  130   continue
 
*---- Coefficients of the Jacobian (all orders through NORD-2).
        do 180 j = 1, 3
          call pa3dif(rdf(1,i), j, 1-nord, rjac(1,i,j))
  180   continue
  190 continue
 
*---- Loop to apply contraction mapping.
      do 390 itra = 1, 12
 
*---- Basis monomials in P.
        pvec(0) = 1.0
        pvec(1) = zf(2)
        pvec(2) = zf(4)
        pvec(3) = zf(6)
        do 220 jord = 2, nord-1
          do 210 i = ibot3(jord), itop3(jord)
            pvec(i) = pvec(iq(lind31+i)) * pvec(iq(lind32+i))
  210     continue
  220   continue
 
*---- Error vector.
        do 290 i = 1, 3
          rmat(i,4) = rdf(0,i) - zi(2*i)
          do 260 n = 1, itop3(nord-1)
            rmat(i,4) = rmat(i,4) + rdf(n,i) * pvec(n)
  260     continue
 
*---- Jacobian matrix.
          do 280 j = 1, 3
            rmat(i,j) = rdf(i,j)
            do 270 n = 1, itop3(nord-2)
              rmat(i,j) = rmat(i,j) + rjac(n,i,j) * pvec(n)
  270       continue
  280     continue
  290   continue
        call solver(rmat, 3, 1, irank)
        if(irank .lt. 3) then
          call aawarn('LMNEWT', 1,
     +    'Problems with matrix inversion for tracking.')
          return
        endif
        square = rmat(1,4)**2 + rmat(2,4)**2 + rmat(3,4)**2
        root = sqrt(square)
        do 330 i = 1, 3 
          zf(2*i) = zf(2*i) - rmat(i,4)
  330   continue
        if(root .lt. 1.0e-12) go to 400
  390 continue
      call aawarn('LMNEWT', 1, 'Newton search did not converge.')
 
*---- Compute new position coordinates.
  400 continue
 
*---- Basis monomials in Q and P.
      do 410 i = 1, 6
        vect(i) = zf(i)
  410 continue
      do 430 jord = 2, nord-1
        do 420 i = ibot6(jord), itop6(jord)
          vect(i) = vect(iq(lind61+i)) * vect(iq(lind62+i))
  420   continue
  430 continue
 
*---- Values of the new position coordinates.
      do 490 iqind = 1, 5, 2
        ipind = iqind + 1
        do 480 j = 7, itop6(nord-1)
          zf(iqind) = zf(iqind) + df(j,ipind) * vect(j)
  480   continue
  490 continue
 
      end
