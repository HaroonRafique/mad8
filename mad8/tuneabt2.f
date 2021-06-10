      double precision function tuneabt2(x, xp, maxn)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Computes the tune using the interpolated FFT with Hanning filter.  *
*   See CERN SL/95-84 formula (25).                                    *
*   (best suited for MAXN > 64 turns)                                  *
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
      integer ix,maxn,mf,mft,nft,nftmax,nn,npoint
      double precision assk,cf1,cf2,cf3,co,ftmax,one,p1,p2,scra1,
     +scra2,scra3,scra4,si,step,temp,two,twopi,x,xp,zero
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
      parameter          (twopi = 2.0 * pi)
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
 
*---- Copy data to local storage using Hanning filter.
      step = pi / npoint
      do 10 mf = 1, npoint
        temp = sin(mf * step)**2
        dq(ix+2*mf-1) = temp * x(mf)
        dq(ix+2*mf)   = temp * xp(mf)
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
      cf1 = sqrt(dq(ix+2*nftmax-3)**2 + dq(ix+2*nftmax-2)**2)
      cf2 = sqrt(dq(ix+2*nftmax-1)**2 + dq(ix+2*nftmax)**2)
      cf3 = sqrt(dq(ix+2*nftmax+1)**2 + dq(ix+2*nftmax+2)**2)
      if (cf3 .gt. cf1) then
        p1 = cf2
        p2 = cf3
        nn = nftmax
      else
        p1 = cf1
        p2 = cf2
        nn = nftmax-1
      endif
 
*---- Interpolation.
      co = cos(twopi / npoint)
      si = sin(twopi / npoint)
      scra1 = co**2 * (p1 + p2)**2 - 2*p1*p2*(2*co**2 - co - one)
      scra2 = (p1 + p2*co) * (p1 - p2)
      scra3 = p1**2 + p2**2 + 2*p1*p2*co
      scra4 = (-scra2 + p2*sqrt(scra1)) / scra3
      assk = nn + npoint / twopi * asin(si * scra4)
      tuneabt2 = one - (assk - one) / float(npoint)
 
*---- Release working storage.
      iwork = ix
 
      end
