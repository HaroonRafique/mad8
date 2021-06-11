      subroutine comica
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Closed orbit correction by Micado algorithm, given monitor reads.  *
* MICADO command:                                                      *
*   ERROR     (real)    Tolerance for MICADO algorithm.                *
*   NCORR     (integer) Number of correctors allowed.                  *
*   C2LIST    (name)    Corrector list desired after correction.       *
*   M2LIST    (name)    Monitor list desired after correction.         *
*   HWEIGHT   (real)    Weight for horizontal dispersion correction.   *
*   VWEIGHT   (real)    Weight for vertical dispersion correction.     *
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      integer lcobuf,lcocor,lcoelm,lcomon,lcotab
 
*---- Links for closed orbit correction module.
      common /colink/   lcotab, lcobuf, lcocor, lcomon, lcoelm
      save              /colink/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer ilc2,ilm1,jpl,jplmax,jplmin,nkick,nlist
      double precision drms,one,rrms,zero
 
      parameter         (nlist = 3, zero = 0.0d0, one = 1.0d0)
      character*(mcnam) dlist(nlist), clist(2), cplane
      character*1       plane(2)
      dimension         drms(2), rrms(2)
      logical           ok
 
      data dlist        / 'NONE', 'USED', 'ALL' /
      data plane        / 'X', 'Y' /
 
*---- Check main beam line.
      call lnchck('MICADO', error)
      if (error) go to 9999
      lscom = lq(lcseq-mscom)
      call ncopy(iq(lscom+1), ncor, 2)
      call ncopy(iq(lscom+3), nmon, 2)
      call ucopy(q(lscom+5), halfqx, 2*mwflt)
 
*---- Extract data.
      qual = zero
      call utgflt(lccmd, 1, 1, qual)
 
      nkick = 0
      call utgint(lccmd, 2, 2, nkick)
 
      clist(1) = ' '
      clist(2) = ' '
      call utgnam(lccmd, 3, 4, clist)
      call utlook(clist(1), dlist, nlist, ilc2)
      call utlook(clist(2), dlist, nlist, ilm1)
 
      weight(1) = zero
      weight(2) = zero
      call utgflt(lccmd, 5, 6, weight)
 
      cplane = ' '
      call utgnam(lccmd, 7, 7, cplane)
      jplmin = 1
      jplmax = 2
      if (cplane .eq. 'X') then
        jplmax = 1
      else if (cplane .eq. 'Y') then
        jplmin = 2
      endif
 
*---- Check presence of corrector and monitor table.
      if (lq(lcseq-mscom) .eq. 0) then
        call aafail('COMICA', 1, 'No monitor readings available.')
        go to 9999
      endif
 
*---- Print values before correction.
      call copkik(1, '(before correction)')
      call copdis(ilm1, '(before correction)', rrms, drms)
 
*---- Compute new corrector settings.
      do 90 jpl = jplmin, jplmax
        if (weight(jpl) .eq. zero) then
          write (iqlog, 910) plane(jpl)
          call colorb(nkick, jpl, ok)
        else
          write (iqlog, 920) plane(jpl), weight(jpl)
          call coldis(nkick, jpl, ok)
        endif
   90 continue
 
*---- Print corrector and monitor tables after correction.
      call copkik(ilc2, '(after correction)')
 
  910 format(' '/t11,'Beginning orbit correction for plane ',a1,'.')
  920 format(' '/
     +       t11,'Beginning orbit and dispersion correction for plane ',
     +       a1,','/
     +       t11,'Weight for dispersion = ',f12.6)
 
 9999 end
