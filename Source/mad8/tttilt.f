      subroutine tttilt(tilt, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Change reference for a set of particles by S rotation.             *
* Input:                                                               *
*   TILT      (real)    Angle of rotation.                             *
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
      integer itrack,ktrack
      double precision ct,st,temp,tilt,track
      dimension         track(6,*)
 
      ct = cos(tilt)
      st = sin(tilt)
      do 10 itrack = 1, ktrack
        temp = track(1,itrack)
        track(1,itrack) = + ct*temp + st*track(3,itrack)
        track(3,itrack) = - st*temp + ct*track(3,itrack)
        temp = track(2,itrack)
        track(2,itrack) = + ct*temp + st*track(4,itrack)
        track(4,itrack) = - st*temp + ct*track(4,itrack)
   10 continue
 
      end
