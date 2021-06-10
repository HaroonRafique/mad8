      double precision function fitlyap(distvect, nturn)
      implicit none
*----------------------------------------------------------------------*
* Purpose:
*   Computes interpolated Lyapunov exponent.
*   DISTVECT (normalized) distance between two companion particles
*   NTURN is the number of turns.
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
      integer ilogd,ilogn,in,mf,n1,n2,n3,npoint,nturn
      double precision deltalog1,deltalog2,deltalog3,distvect,dlmax,
     +fitlyap1,fitlyap2,fitlyap3,slopexy
      dimension distvect(*)
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
*---- Assign working space.
      ilogd = iwork
      ilogn = ilogd + nturn
      in    = ilogn + nturn
      iwork = in    + nturn
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
      do 10 mf = 1, nturn
        dq(ilogd+mf) = log(distvect(mf))
        dq(in+mf)    = mf
        dq(ilogn+mf) = log(dq(in+mf))
   10 continue
 
*---- Loglog fit over 3 subsequent periods of NPOINT = NTURN/4 TURNS
*     starting at N1 = NPOINT + 1, i.e. at the second fourth
      npoint = int(nturn / 4)
      n1 = npoint + 1
      n2 = n1 + npoint
      n3 = n2 + npoint
 
*---- DELTALOG = deviation from 1 of loglog slope.
      dlmax = 1.0d-1
 
      deltalog1 = slopexy(dq(ilogn+n1), dq(ilogd+n1), npoint) - 1.0d0
      deltalog2 = slopexy(dq(ilogn+n2), dq(ilogd+n2), npoint) - 1.0d0
      deltalog3 = slopexy(dq(ilogn+n3), dq(ilogd+n3), npoint) - 1.0d0
 
      if (deltalog1 .lt. dlmax  .and.  deltalog2 .lt. dlmax  .and.
     +    deltalog3 .lt. dlmax) then
        fitlyap = 0.0d0
      else
        fitlyap1 = slopexy(dq(in+n1), dq(ilogd+n1), npoint)
        fitlyap2 = slopexy(dq(in+n2), dq(ilogd+n2), npoint)
        fitlyap3 = slopexy(dq(in+n3), dq(ilogd+n3), npoint)
 
        if (fitlyap1 .lt. fitlyap2) then
          fitlyap = fitlyap2
        else
          fitlyap = fitlyap1
        endif
        if (fitlyap .lt. fitlyap3) then
          fitlyap = fitlyap3
        endif
      endif
 
*---- release working store.
      iwork = ilogd
 
      end
