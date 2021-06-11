      double precision function tuneabt(x, xp, maxn)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Computes the tune using formula (18) of CERN SL/95-84 (AP).        *
*   No filter.                                                         *
*   (best suited for MAXN <= 64 TURNS)                                 *
* X, XP are the coordinates of the orbit,                              *
* MAXN  is the length of the orbit.                                    *
* Authors:                                                             *
*   R. Bartolini - CERN and Bologna University,                        *
*   E. TODESCO   - INFN and CERN.                                      *
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
      integer ix,maxn,mf,mft,nft,nftmax,npoint
      double precision arg,assk,cf1,cf2,cf3,ftmax,one,temp,two,x,
     +xp,zero
      dimension          x(*), xp(*)
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
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter          (zero = 0.0, one = 1.0, two = 2.0)
 
*---- Use first NPOINT points.
      mft = int(log(float(maxn)) / log(two))
      npoint = 2**mft
 
*---- Assign working space.
      ix    = iwork
      iwork = iwork + 2 * npoint
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Copy data to local storage, no filter.
      do 10 mf = 1, npoint
        dq(ix+2*mf-1) = x(mf)
        dq(ix+2*mf)   = xp(mf)
   10 continue
      call fft(dq(ix+1), npoint, -1)
 
*---- Search for maximum of Fourier spectrum.
      ftmax = zero
      nftmax = 0
      do 20 nft = 1, npoint
        temp = sqrt(dq(ix+2*nft-1)**2 + dq(ix+2*nft)**2)
        if (temp .gt. ftmax) then
          ftmax  = temp
          nftmax = nft
        endif
   20 continue
 
*---- Improve estimate by interpolation.
      cf1 = sqrt(dq(ix+2*nftmax-3)**2 + dq(ix+2*nftmax-2)**2)
      cf2 = sqrt(dq(ix+2*nftmax-1)**2 + dq(ix+2*nftmax)**2)
      cf3 = sqrt(dq(ix+2*nftmax+1)**2 + dq(ix+2*nftmax+2)**2)
      if (cf3 .gt. cf1) then
        arg  = sin(pi / npoint) / (cf2 / cf3 + cos(pi / npoint))
        assk = float(nftmax) + npoint / pi * atan(arg)
      else
        arg  = sin(pi / npoint) / (cf1 / cf2 + cos(pi / npoint))
        assk = float(nftmax-1) + npoint / pi * atan(arg)
      endif
      tuneabt = one - (assk - one) / float(npoint)
 
*---- Release working storage.
      iwork = ix
 
      end
