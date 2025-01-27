      subroutine trtbsvly(ltab, iturn, track)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save Lyapunov difference of particle positions in track table.     *
* Input:                                                               *
*   LTAB(1)   (pointer) Pointer to track table.                        *
*   ITURN     (integer) Turn number.                                   *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track.                       *
*   NTRACK    (integer) Number of surviving tracks.                    *
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
      integer icoor,iform,iturn
      double precision t,track
      dimension         track(6,*)
      integer           ltab(1)
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
      integer mtbact,mtbbky,mtbbuf,mtbcfm,mtbcnm,mtbcol,mtbcps,mtbdsc,
     +mtbf1,mtbf2,mtbfst,mtblst,mtbmod,mtbnam,mtbrow,mtbseg,mtbsiz,
     +mtbsky,mtbwid
 
*---- Parameters for table manager bank structure.
      parameter         (mtbact = 1, mtbbuf = 2, mtbmod = 1)
      parameter         (mtbf1  = 1,
     +                   mtbseg = 2, mtbrow = 3, mtbcol = 4, mtbwid = 5,
     +                   mtbf2  = 6,
     +                   mtbnam = 7, mtbsiz = mtbnam + mwnam - 1)
      parameter         (mtbsky = 2, mtbbky = 3, mtbcnm = 4,
     +                   mtbcfm = 5, mtbcps = 6, mtbdsc = 7,
     +                   mtbfst = 8, mtblst = 9)
      integer ltrbuf,ltrcur,ltrfbf,ltrnoi,ltrnum,ltrobs,ltrrbf,ltrstt,
     +ltrtab,ltrtmp
 
*---- Links used for tracking.
      common /trlink/   ltrnum, ltrstt, ltrnoi, ltrfbf, ltrrbf, ltrobs,
     +                  ltrcur, ltrtmp, ltrtab, ltrbuf
      save              /trlink/
 
      dimension         t(6)
      logical           eflag
 
      call tbseg(ltab, iturn + 1, eflag)
      iform = iq(lq(ltab(1)-mtbcfm)+3)
      call tbset(ltab, 1, 3, ltrbuf)
      iq(ltrbuf+1) = iturn
      iq(ltrbuf+2) = 1
      if (iform .eq. 4) then
        do 10 icoor = 1, 6
          t(icoor) = track(icoor,1) - track(icoor,2)
   10   continue
        call ucopy(t, q(ltrbuf+3), 6*mwflt)
      else
        do 20 icoor = 1, 6
          q(ltrbuf+icoor+2) = track(icoor,1) - track(icoor,2)
   20   continue
      endif
 
      end
