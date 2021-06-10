      subroutine ttwake(eleff, nbin, binmax, lfile, tfile, ener1, track,
     +                  ntrk)
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track wakefield effects.                                           *
* Input:                                                               *
*   ELEFF      (real)    Effective length of the structure.  If ELEFF  *
*                        is zero, the wakefield is assumed to be the   *
*                        integrated strength.                          *
*   NBIN       (integer) The number of bins to use.                    *
*   BINMAX     (real)    Maximum value for binning.                    *
*   LFILE      (string)  Longitudinal wakefield filename.  The         *
*                        wakefield consists of comment lines started   *
*                        with a "(" and data lines with Z in meters    *
*                        and WL in V/C/m.  It is  assumed that the Z   *
*                        values are monotonically increasing.          *
*   TFILE      (string)  Transverse wakelfield filename.               *
* Input/output:                                                        *
*   TRACK(6,*) (real)    Track coordinates: (X, PX, Y, PY, T, PT).     *
*   NTRK       (integer) Number of surviving tracks.                   *
*----------------------------------------------------------------------*
* Created:  06-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Routine is based on that in Tor's private version but has updated  *
*   common block definitions; the wakefields are read on unit #51      *
*----------------------------------------------------------------------*
 
      implicit none
      integer nbin, ntrk
      double precision ener1, eleff, binmax, track(6,*)
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer ietflg,ipnflg, liftbeam,currbeam
      double precision alfa,amass,arad,beta,betas,bunch,bxmax,bymax,
     +charge,cosmux,cosmuy,currnt,deltas,deltat,dtbyds,dxmax,dymax,
     +et,ex,exn,ey,eyn,freq0,gamma,gammas,gamtr,parnum,pc,pdamp,
     +qs,qx,qy,sigdx,sigdy,sige,sigt,sigx,sigxco,sigy,sigyco,sinmux,
     +sinmuy,u0,xcomax,xix,xiy,ycomax,en0,beambv,elkfact,elmfact
 
*---- Particles, emittances and sigmas.
      integer mfact, mbmult
      parameter (mfact = 50, mbmult = 20)
      common /beanam/   prtnam, bsequnam,
     +                  prtnames(mttact), bseqnames(mttact)
      common /beaflt/   amass, charge, en0, pc, gamma,
     +                  ex, exn, ey, eyn, et, sigt, sige,
     +                  bunch, parnum, currnt
      common /beaaux/   sigx, qx, xix, cosmux, sinmux, bxmax, dxmax,
     +                  xcomax, sigxco, sigdx,
     +                  sigy, qy, xiy, cosmuy, sinmuy, bymax, dymax,
     +                  ycomax, sigyco, sigdy,
     +                  qs, alfa, gamtr, deltas, dtbyds, deltat,
     +                  freq0, beta, u0, arad, beambv, pdamp(3),
     +                  gammas, betas,
     +                  elkfact(mfact), elmfact(0:mbmult)
      common /beaint/   ietflg, ipnflg, liftbeam, currbeam
      save   /beaint/
      common /bealog/   fbch, frad
      save              /beanam/, /beaflt/, /beaaux/, /bealog/
      logical           fbch, frad
      character*(mcnam) prtnam, bsequnam, prtnames, bseqnames
      integer iffreq,ipfreq,itrfil,npart,ntrack
 
*---- Common flags for tracking.
      common /trkchr/   trktitle
      character * 32    trktitle
      common /trkint/   ipfreq, iffreq, npart, ntrack, itrfil
      common /trktim/   time1, time2, dtime
      common /trkflg/   onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      save              /trkint/, /trktim/, /trkflg/
      real              time1, time2, dtime
      logical           onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer lzmax, lrec, j, icount, nb, itrk, ib, irec
      double precision tzmax, el, dzbin, z, ch, wx, wy, dz, ww,
     +f, wl, zero, half, one, two, three
      parameter         (zero = 0.0d0, half = 0.5d0)
      parameter         (one = 1.0d0, two = 2.0d0, three = 3.0d0)
 
*---- Local data and parameters.
      double precision  lwake(0:2500,2), twake(0:2500,2)
      double precision  bins(-1000:1000,4)
      character*80      lfile, tfile, lold, told, buffer
      integer           trec
      save              lold, told, lrec, trec, lwake, twake
      data              lold /" "/, told /" "/
      data              lzmax / -1.d0 /, tzmax / -1.d0 /
      data              lwake(0,1) / -1.d0 /, lwake(0,2) / 0.d0 /
      data              twake(0,1) / -1.d0 /, twake(0,2) / 0.d0 /
 
*---- Make sure the bunch charge and the energy have been set.
      if (ener1 .le. zero .or. parnum .eq. zero) goto 990
 
*---- Read the longitudinal wake: units= Volts/Coulomb/meter.
      if (lfile .ne. lold .and. lfile .ne. " ") then
        lrec = 0
        open (51, file=lfile, status='old', err=900)
   10   read (51, '(a)', end=100, err=910) buffer
        if (buffer(1:1) .ne. '(') then
          lrec = lrec + 1
          if (lrec .gt. 2500) goto 940
          read (buffer, *, end=910, err=910)
     +      lwake(lrec,1), lwake(lrec,2)
        endif
        goto 10
  100   continue
        close (51)
        if (lrec .gt. 0) then
          lzmax = lwake(lrec,1)
        else
          lzmax = -one
        endif
        write (iqlog, 810) lrec, lfile
        lold = lfile
      else if (lfile .eq. " ") then
        lrec = 0
        lzmax = -one
      endif
 
*---- Read the transverse dipole wake: units=Volts/Coulomb/meter**2.
      if (tfile .ne. told .and. tfile .ne. " ") then
        trec=0
        open (51, file=tfile, status='OLD', err=920)
   20   read (51, '(a)', end=200, err=930) buffer
        if (buffer(1:1) .ne. '(') then
          trec = trec + 1
          if (trec .gt. 2500) goto 950
          read (buffer, *, end=930, err=930)
     +      twake(trec,1), twake(trec,2)
        endif
        goto 20
  200   continue
        close (51)
        if (trec .gt. 0) then
          tzmax = twake(trec,1)
        else
          tzmax = -one
        endif
        write (iqlog, 810) trec, tfile
        told = tfile
      else if (tfile .eq. " ") then
        trec = 0
        tzmax = -one
      endif
 
*---- Compute the wakefields - set constants.
      ch = 1.6e-19 * parnum / npart
      if (eleff .gt. zero) then
        el = eleff
      else
        el = one
      endif
 
*---- Check to see if binning is desired.
      if (nbin .gt. 0) then
        if (nbin .gt. 2001) then
          write (iqlog, 860)
          return
        endif
 
*---- Set up the bins: Bins(*,1) = charge
*                      Bins(2,*) = average X position
*                      Bins(3,*) = average Y position
*                      Bins(4,*) = average Z position
        nb = nint((nbin-half)/two)
        nbin = 1 + 2*nb
        if (nb .gt. 0) then
          dzbin = binmax / nb
        else
          dzbin = 1.d10
        endif
        do 300 j = -nb, nb
          bins(j,1) = zero
          bins(j,2) = zero
          bins(j,3) = zero
          bins(j,4) = zero
  300   continue
 
*---- Bin the data.
        icount = 0
        do 310 itrk = 1, ntrk
          z = track(5, itrk)
          ib = nint(z/dzbin)
          if (abs(ib) .le. nb) then
            icount = icount + 1
            bins(ib,1) = bins(ib,1) + ch
            bins(ib,2) = bins(ib,2) + track(1,itrk)
            bins(ib,3) = bins(ib,3) + track(3,itrk)
            bins(ib,4) = bins(ib,4) + z
          endif
  310   continue
        write (iqlog, 870) nbin, binmax, icount, ntrk
 
*---- Get average positions.
        do 320 j = -nb, nb
          if (bins(j,1) .ne. zero) then
            bins(j,2) = bins(j,2) * ch / bins(j,1)
            bins(j,3) = bins(j,3) * ch / bins(j,1)
            bins(j,4) = bins(j,4) * ch / bins(j,1)
          else
            bins(j,2) = zero
            bins(j,3) = zero
            bins(j,4) = zero
          endif
  320   continue
 
*---- Calculate wakes.
        do 340 itrk = 1, ntrk
          z = track(5, itrk) + 1.d-8
          wl = zero
          wx = zero
          wy = zero
          do 330 j = -nb, nb
 
*---- If particle is outside of the bin.
            if (z .lt. (j-half)*dzbin) then
              dz = bins(j,4) - z
 
*---- Longitudinal - find the wakefield values spanning the separation
*     dz and then interpolate.
              if (dz .le. lzmax) then
                call tt_wake(dz, irec, lrec, lwake(0,1))
                ww = lwake(irec-1,2) + (dz - lwake(irec-1,1))
     +             * (lwake(irec,2)-lwake(irec-1,2))
     +             / (lwake(irec,1)-lwake(irec-1,1))
                wl = wl + ww * bins(j,1)
              endif
 
*---- Transverse.
              if (dz .le. tzmax) then
                call tt_wake(dz, irec, trec, twake(0,1))
                ww = twake(irec-1,2) + (dz - twake(irec-1,1))
     +             * (twake(irec,2)-twake(irec-1,2))
     +             / (twake(irec,1)-twake(irec-1,1))
                wx = wx + ww  * bins(j,1) * bins(j,2)
                wy = wy + ww  * bins(j,1) * bins(j,3)
              endif
 
*---- If the particle is inside the bin.
            else if (z .lt. (j+half)*dzbin) then
              dz = -((j-half)*dzbin - z)/two
              f = -(z - (j+half)*dzbin) / dzbin
 
*---- Longitudinal - find the wakefield values spanning the separation
*     dz and then interpolate.
              if (dz .le. lzmax) then
                call tt_wake(dz, irec, lrec, lwake(0,1))
                ww = lwake(irec-1,2) + (dz - lwake(irec-1,1))
     +             * (lwake(irec,2)-lwake(irec-1,2))
     +             / (lwake(irec,1)-lwake(irec-1,1))
                wl = wl + ww * bins(j,1) * f
              endif
 
*---- Transverse.
              if (dz .le. tzmax) then
                call tt_wake(dz, irec, trec, twake(0,1))
                ww = twake(irec-1,2) + (dz - twake(irec-1,1))
     +             * (twake(irec,2)-twake(irec-1,2))
     +             / (twake(irec,1)-twake(irec-1,1))
                wx = wx + ww  * bins(j,1) * bins(j,2) * f
                wy = wy + ww  * bins(j,1) * bins(j,3) * f
              endif
            endif
  330     continue
 
*---- Track.
          wl = wl * el * 1.e-9 / ener1
          wx = wx * el * 1.e-9 / ener1
          wy = wy * el * 1.e-9 / ener1
          track(2,itrk) = track(2,itrk) + wx
          track(4,itrk) = track(4,itrk) + wy
          track(6,itrk) = track(6,itrk) - wl
  340   continue
 
*---- No binning - calculate particle by particle.
      else
        do 410 itrk = 1, ntrk
          z = track(5, itrk)
          wl = zero
          wx = zero
          wy = zero
          do 400 j = 1, ntrk
            if (track(5,j) .ge. z) then
              dz = track(5,j) - z
 
*---- Longitudinal - find the wakefield values spanning the separation
*     dz and then interpolate.
              if (dz .le. lzmax) then
                call tt_wake(dz, irec, lrec, lwake(0,1))
                ww = lwake(irec-1,2) + (dz - lwake(irec-1,1))
     +             * (lwake(irec,2)-lwake(irec-1,2))
     +             / (lwake(irec,1)-lwake(irec-1,1))
                wl = wl + ww
              endif
 
*---- Transverse.
              if (dz .le. tzmax) then
                call tt_wake(dz, irec, trec, twake(0,1))
                ww = twake(irec-1,2) + (dz - twake(irec-1,1))
     +             * (twake(irec,2)-twake(irec-1,2))
     +             / (twake(irec,1)-twake(irec-1,1))
                wx = wx + ww  * track(1,j)
                wy = wy + ww  * track(3,j)
              endif
            endif
  400     continue
 
*---- Track.
          wl = wl * ch * el * 1.e-9 / ener1
          wx = wx * ch * el * 1.e-9 / ener1
          wy = wy * ch * el * 1.e-9 / ener1
          track(2,itrk) = track(2,itrk) + wx
          track(4,itrk) = track(4,itrk) + wy
          track(6,itrk) = track(6,itrk) - wl
  410   continue
      endif
 
  810 format (/' TTWAKE  -- Read ',i5,' lines from: ',a)
  820 format (/' TTWAKE  -- Error opening file: ',a)
  830 format (/' TTWAKE  -- Error reading file: ',a/
     +        10x,'at record # ',i5,' with buffer: ',a)
  840 format (/' TTWAKE  -- Wake file too large (2500 max.): ',a)
  860 format (/' TTWAKE  -- Too many bins (MAX=2001): ',i5)
  870 format (/' TTWAKE  -- Created ',i5,' bins with max Z: ',1pg12.4/
     +        10x,'Binned ',i5,' tracks out of ',i5,' total')
  890 format (/' TTWAKE  -- BEAM command must set Energy and # Part.')
 
*---- Error returns.
  900 write (iqlog, 820) lfile
      return
 
  910 write (iqlog, 830) lfile, lrec, buffer
      close (51)
      return
 
  920 write (iqlog, 820) tfile
      return
 
  930 write (iqlog, 830) tfile, trec, buffer
      close (51)
      return
 
  940 write (iqlog, 840) lfile
      close (51)
      return
 
  950 write (iqlog, 840) tfile
      close (51)
      return
 
  990 write (iqlog, 890)
      return
 
      end
