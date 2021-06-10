      subroutine tttrak(ek, re, te, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track a set of particle with a given TRANSPORT map.                *
* Input:                                                               *
*   EK(6)     (real)    Kick.                                          *
*   RE(6,6)   (real)    First-order terms.                             *
*   TE(6,6,6) (real)    Second-order terms.                            *
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
      integer i,itrack,ktrack
      double precision ek,re,se,te,temp,track
      dimension         ek(6), re(36), te(36,6), track(6,*)
 
      dimension         temp(6), se(36)
 
      do 60 itrack = 1, ktrack
        do 10 i = 1, 36
          se(i) = re(i)
     +      + te(i,1)*track(1,itrack) + te(i,2) * track(2,itrack)
     +      + te(i,3)*track(3,itrack) + te(i,4) * track(4,itrack)
     +      + te(i,5)*track(5,itrack) + te(i,6) * track(6,itrack)
   10   continue
        do 30 i = 1, 6
          temp(i) = ek(i)
     +      + se(i)    * track(1,itrack) + se(i+ 6) * track(2,itrack)
     +      + se(i+12) * track(3,itrack) + se(i+18) * track(4,itrack)
     +      + se(i+24) * track(5,itrack) + se(i+30) * track(6,itrack)
   30   continue
        do 50 i = 1, 6
          track(i,itrack) = temp(i)
   50   continue
   60 continue
 
      end
