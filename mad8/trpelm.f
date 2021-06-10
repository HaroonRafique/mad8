      subroutine trpelm(iturn, isup, ipos, suml, enerp, track, number,
     +ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print particle positions during tracking.                          *
* Input:                                                               *
*   ITURN     (integer) Turn number.                                   *
*   ISUP      (integer) Superperiod number.                            *
*   IPOS      (integer) Position counter.                              *
*   SUML      (real)    Cumulated length.                              *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track.                       *
*   KTRACK    (integer) number of surviving tracks.                    *
*----------------------------------------------------------------------*
* Modified: 14-AUG-1999, T. Raubenheimer (SLAC)                        *
*   Modified calling parameters and format statement to print out      *
*   energy also if the energy is non-zero                              *
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
      integer i,iname,iocc,ipos,isup,iturn,j,jbyt,ktrack
      double precision suml,track
      double precision enerp
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
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      integer locunit
      logical error
      character*(mcnam) elmnam
      data locunit / 0/
 
      if (optflg(12)) then
*--- dump into file
        if (locunit .eq. 0) then
          call flopen('trackdump', 'SWFD', 0, 0, locunit, error)
          if (error) locunit = 77
        endif
        do j = 1, ktrack
          if (number(j) .ne. 0)
     +    write(locunit, '(i5, 1p, 7e14.6)') number(j), suml,
     +    (track(i,j), i = 1,6)
        enddo
      else
      iname = (iq(lsdir+ipos) - 1) * mwnam + 1
      call uhtoc(q(ldbnk(2)+iname), mcwrd, elmnam, mcnam)
      iocc = jbyt(iq(lsflg+ipos), mocc1, mocc2)
      if (enerp .eq. 0.0d0) then
        write (iqpr2, 910) iturn, isup, suml, elmnam, iocc
        do 10 j = 1, ktrack
          if (number(j) .ne. 0) then
            write (iqpr2, 920) number(j), (track(i,j), i = 1, 6)
          endif
   10   continue
      else
        write (iqpr2, 915) iturn, isup, suml, elmnam, iocc
        do 20 j = 1, ktrack
          if (number(j) .ne. 0) then
            write (iqpr2, 925) number(j), (track(i,j), i = 1, 6), enerp
          endif
   20   continue
      endif
      endif
 
  910 format(' '/' Positions w.r.t. beam line axis, turn = ',
     +       i8,', superperiod = ',i5,', s = ',f12.6,
     +       ', element ',a,'[',i6,']'/
     +       ' number',7x,'x',15x,'px',14x,'y',15x,'py',14x,'t',15x,
     +       'pt')
  915 format(' '/' Positions w.r.t. beam line axis, turn = ',
     +       i8,', superperiod = ',i5,', s = ',f12.6,
     +       ', element ',a,'[',i6,']'/
     +       ' number',7x,'x',15x,'px',14x,'y',15x,'py',14x,'t',15x,
     +       'pt',18x,'energy')
  920 format(1x,i5,6x,1p,6e16.8)
  925 format(1x,i5,6x,1p,6e16.8,6x,1p,g10.3)
 
      end
