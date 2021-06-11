      subroutine trflow(iturn, isup, ipos, suml, track, number, ntrk)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Give overflow message.                                             *
*   This routine assumes that at least one particle is lost.           *
* Input:                                                               *
*   ITURN     (integer) Current turn number.                           *
*   ISUP      (integer) Current superperiod number.                    *
*   IPOS      (integer) Position counter.                              *
*   SUML      (integer) Accumulated length.                            *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track.                       *
*   NTRK      (integer) Number of surviving tracks.                    *
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
      integer i,icopy,iname,iocc,ipos,isup,itrack,iturn,jbyt,ntrk
      double precision eject,suml,track
      dimension         track(6,*)
      integer           number(*)
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer lcali,lcatt,lccls,lccmd,lccom,lcdef,lcelm,lcexp,lcfld,
     +lckey,lcseq,lcspl,lcsrc,lcvar,ldbnk,ldkey,lref1,lref2,lsali,lscom,
     +lsdir,lsfld,lsflg,lsnum,lsspl,lbeam,lconsm,ldummy
 
*---- Global reference links.
      common /refer/    lref1,
     +                  lcali, lcatt, lccls, lccmd, lccom, lcdef, lcelm,
     +                  lcexp, lcfld, lckey, lcseq, lcspl, lcsrc, lcvar,
     +                  lbeam, lconsm, ldbnk(4), ldkey(4), ldummy(10),
     +                  lsali, lscom, lsdir, lsfld, lsflg, lsnum, lsspl,
     +                  lref2
      save              /refer/
      integer liftseq, currseq
      common /seqinfi/ liftseq, currseq
      character * (mcnam) sequnam, seqnames
      common /seqinfc/ sequnam, seqnames(mttact)
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
 
      parameter         (eject = 1.0)
      character*(mcnam) elmnam
 
*---- Stop when loosing any particle in DYNAP mode.
      if (fdynap) then
        ntrk = 0
        return
      endif
 
*---- Pack remaining particles after an overflow is detected.
      iname = (iq(lsdir+ipos) - 1) * mwnam + 1
      call uhtoc(q(ldbnk(2)+iname), mcwrd, elmnam, mcnam)
      iocc = jbyt(iq(lsflg+ipos), mocc1, mocc2)
      write (iqpr2, 910) iturn, isup, suml, elmnam, iocc
      icopy = 0
      do 80 itrack = 1, ntrk
 
*---- Is particle outside aperture?
        if (abs(track(1,itrack)) .gt. eject  .or.
     +      abs(track(3,itrack)) .gt. eject) then
 
*---- Give message for particle.
          write (iqpr2, 920) number(itrack), (track(i,itrack),i=1,6)
 
*---- Loosing closed orbit is fatal.
          if (number(itrack) .eq. 0) then
            call aafail('TRFLOW', 1,
     +        'Closed orbit has been lost, cannot continue tracking.')
            ntrk = 0
            return
          endif
 
*---- If in aperture, compact track storage.
        else
          icopy = icopy + 1
          if (icopy .ne. itrack) then
            track(1,icopy) = track(1,itrack)
            track(2,icopy) = track(2,itrack)
            track(3,icopy) = track(3,itrack)
            track(4,icopy) = track(4,itrack)
            track(5,icopy) = track(5,itrack)
            track(6,icopy) = track(6,itrack)
            number(icopy) = number(itrack)
          endif
        endif
   80 continue
      ntrk = icopy
 
  910 format(' Particle(s) lost during turn ',i8,', superperiod ',i8,
     +  ', s = ',f12.6,' by overflow in ',a,'[',i8,']'/
     +  ' Number',6x,'x',15x,'px',14x,'y',15x,'py',14x,'t',15x,'pt')
  920 format(' ',i6,4x,1p,6e16.8)
 
      end
