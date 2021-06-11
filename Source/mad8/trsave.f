      subroutine trsave
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save final conditions of RUN as initial conditions for another     *
*   tracking run.                                                      *
* Attributes:                                                          *
*   FILENAME  (string)  Name of file to receive values.                *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
      integer ltrbuf,ltrcur,ltrfbf,ltrnoi,ltrnum,ltrobs,ltrrbf,ltrstt,
     +ltrtab,ltrtmp
 
*---- Links used for tracking.
      common /trlink/   ltrnum, ltrstt, ltrnoi, ltrfbf, ltrrbf, ltrobs,
     +                  ltrcur, ltrtmp, ltrtab, ltrbuf
      save              /trlink/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer ibias,ipart,isave,iwork,j
 
      character*(mcfil) filnam
 
*---- Fetch file name
      filnam = 'tsave'
      call utgstr(lccmd, 1, 1, filnam)
 
*---- Quit, if no particles.
      if (ntrack .le. 0) then
        call aawarn('TRSAVE', 1,
     +  'Cannot TSAVE --- no particles available.')
      else
 
*---- Open disk file and save last particle positions.
        call flopen(filnam, 'SWFD', 0, 0, isave, error)
        if (.not. error) then
          iwork = 6 * ntrack
          call mzwork(0, dq(1), dq(iwork+1), 2)
          call ucopy(q(ltrstt+1), dq(1), mwflt * iwork)
          ibias = 0
          do 10 ipart = 1, ntrack
            if (iq(ltrnum+ipart) .ne. 0) then
              write (isave, 910) iq(ltrnum+ipart),
     +          (dq(ibias+j) - orbit0(j), j = 1, 6)
            endif
            ibias = ibias + 6
   10     continue
          call mzwork(0, dq(1), dq(1), -1)
          write (isave, 920)
          call flname(isave, filnam)
          call flclos(isave, error)
          if (.not. error) then
            write (msg, 930) ntrack, filnam
            call aainfo('TRSAVE', 1, msg)
          endif
        endif
      endif
 
  910 format('! Particle number ',i6,':'/
     +       'START, X = ',g20.12,', PX     = ',g20.12,',&'/
     +       '       Y = ',g20.12,', PY     = ',g20.12,',&'/
     +       '       T = ',g20.12,', DELTAP = ',g20.12)
  920 format('RETURN')
  930 format(i5,' particles written on file: ',a)
 
      end
