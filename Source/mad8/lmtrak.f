      subroutine lmtrak(nord, fp, fm, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track particles through a Lie-algebraic map.                       *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   FP, FM    (map)     Element map.                                   *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   KTRACK    (integer) number of surviving tracks.                    *
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
      integer idf,isave,itrack,ndim,nord,ktrack
      double precision fm,fp,track,zi,zt
      dimension         fp(*), fm(6,6), track(6,ktrack)
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
 
      dimension         zi(6), zt(6)
 
*---- Allocate working space.
      isave = iwork
      idf   = iwork
      ndim  = itop6(nord-1)
      iwork = idf + 6 * ndim
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Set up derivaties of generating function.
      call lmcanx(nord, fp, dq(idf+1), ndim)
 
*---- Loop for particles.
      do 90 itrack = 1, ktrack
 
*---- Linear part of the map (displacement and transfer matrix).
        zt(1) = track(1,itrack) - fp(2)
        zt(2) = track(2,itrack) + fp(1)
        zt(3) = track(3,itrack) - fp(4)
        zt(4) = track(4,itrack) + fp(3)
        zt(5) = track(5,itrack) - fp(6)
        zt(6) = track(6,itrack) + fp(5)
 
*---- Find new momenta by Newton search.
        if (nord .gt. 2) then
          call m66byv(fm, zt, zi)
          call lmnewt(nord, dq(idf+1), ndim, zi, track(1,itrack))
        else
          call m66byv(fm, zt, track(1,itrack))
        endif
   90 continue
 
*---- Drop working storage.
      iwork = isave
 
      end
