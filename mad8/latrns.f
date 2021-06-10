      subroutine latrns(nord, pbh, ndim, zi, zf)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track through Lie-algebraic map with the "non-symplectic" method.  *
* Input:                                                               *
*   NORD      (integer) Order of the map.                              *
*   PBH(NDIM,6)         Poisson brackets as required.                  *
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
      integer i,isave,ivect,j,jord,ndim,nord
      double precision pbh,zf,zi
      dimension         pbh(ndim,6), zi(6), zf(6)
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
*---- Allocate working space.
      isave = iwork
      ivect = iwork
      iwork = ivect + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Compute basis monomials.
      do 10 i = 1, 6
        dq(ivect+i) = zi(i)
   10 continue
      do 30 jord = 2, nord-1
        do 20 i = ibot6(jord), itop6(jord)
          dq(ivect+i) = dq(ivect+iq(lind61+i))*dq(ivect+iq(lind62+i))
   20   continue
   30 continue
 
*---- Compute transformation.
      do 90 j = 1, 6
        zf(j) = 0.0
        do 80 i = 1, itop6(nord-1)
          zf(j) = zf(j) + dq(ivect+i) * pbh(i,j)
   80   continue
   90 continue
 
*---- Drop working storage.
      iwork = isave
 
      end
