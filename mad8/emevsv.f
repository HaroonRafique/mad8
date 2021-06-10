      subroutine emevsv(iflag, elmnam, ipos, em, amu)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save eigenvectors in EIGEN command for postprocessing.             *
* Input:                                                               *
*   IFLAG     (integer) Position code:                                 *
*                       1 = Create table.                              *
*                       2 = Save.                                      *
*                       3 = Close.                                     *
*   ELMNAM    (char)    Element name; for IFLAG = 1|3: Table name.     *
*   IPOS      (integer) Position number.                               *
*   EM(6,6)   (real)    Eigenvector matrix.                            *
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
      integer i,ibias,iflag,ipos,l,maxcol,nb,nc,nr,ns
      double precision amu,dummy,em
      character*(mcnam) elmnam
      dimension         em(36), amu(3)
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
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
      integer ndelta
 
*---- Common for Twiss module.
      common /twchar/   funnam, optnam, sumnam, betnam
      common /twdata/   ndelta, chrom, couple
      save              /twdata/, /twchar/
      character*(mcnam) funnam, optnam, sumnam, betnam
      logical           chrom, couple
      parameter         (maxcol = 47)
      character*(mcnam) colnam(maxcol)
      integer           icfrm(maxcol)
 
      data colnam
     +  / 'NAME', 'S',
     +    'XCO',  'PXCO', 'YCO',  'PYCO', 'TCO',  'PTCO',
     +    'E11',  'E21',  'E31',  'E41',  'E51',  'E61',
     +    'E12',  'E22',  'E32',  'E42',  'E52',  'E62',
     +    'E13',  'E23',  'E33',  'E43',  'E53',  'E63',
     +    'E14',  'E24',  'E34',  'E44',  'E54',  'E64',
     +    'E15',  'E25',  'E35',  'E45',  'E55',  'E65',
     +    'E16',  'E26',  'E36',  'E46',  'E56',  'E66',
     +    'MU1',  'MU2',  'MU3' /
 
*---- Create new table for eigenvectors.
*     Warning: L is local link. Be careful with Zebra calls.
      if (iflag .eq. 1) then
        lsnum = lq(lcseq-msnum)
        ns = 1
        nr = iq(lq(lcseq-msflg)-1)
        nc = maxcol
        nb = 1
        icfrm(1) = 5
        icfrm(2) = 3
        if (double) icfrm(2) = mreal
        do 10 i = 3, maxcol
          icfrm(i) = icfrm(2)
   10   continue
        call tbcrea(elmnam, ns, nr, nc, colnam, icfrm, nb, ltwfun)
        call tbpdsc(ltwfun, 'TYPE', 5, 0, dummy, 'EIGEN')
        call mzbook(2, l, ltwfun, -1, 'BRNG', 0, 0, mss, 7, 0)
        call ucopy(q(lcseq+1), q(l+1), mss)
*---- Save one position.
      else if (ltwfun .ne. 0) then
        if (iflag .eq. 2) then
          call tbset(ltwfun, ipos, 3, ltwbuf)
          call uctoh(elmnam, q(ltwbuf+1), mcwrd, mcnam)
          ibias = 1 + mwnam
          if (double) then
            call ucopy(suml, q(ltwbuf+ibias), mwflt)
            ibias = ibias + mwflt
            call ucopy(orbit, q(ltwbuf+ibias), 6*mwflt)
            ibias = ibias + 6 * mwflt
            call ucopy(em, q(ltwbuf+ibias), 36*mwflt)
            ibias = ibias + 36 * mwflt
            call ucopy(amu, q(ltwbuf+ibias), 3*mwflt)
          else
            q(ltwbuf+ibias) = suml
            do 20 i = 1, 6
              q(ltwbuf+ibias+i) = orbit(i)
   20       continue
            ibias = ibias + 6
            do 30 i = 1, 36
              q(ltwbuf+ibias+i) = em(i)
   30       continue
            ibias = ibias + 36
            do 40 i = 1, 3
              q(ltwbuf+ibias+i) = amu(i)
   40       continue
          endif
*---- Close table file.
        else if (iflag .eq. 3) then
          call tbpdsc(ltwfun, 'CIRCUM', mreal, 0, nsup * suml, ' ')
          call tbclos(ltwfun)
          msg(1) = 'Orbit and eigenvectors saved in table: ' // elmnam
          call aainfo('EMEVSV', 1, msg)
        endif
      endif
 
      end
