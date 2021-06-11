      subroutine mtinit
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize matching module.                                        *
*----------------------------------------------------------------------*
* Modified: 07-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 26 -> MAXVAL = 27; added ENERGY weight = 1 to     *
*   initial values; set initial weights for 2nd order terms to zero    *
* Modified: 04-MAR-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 27 -> MAXVAL = 28; added CIRC weight = 1 to       *
*   initial values                                                     *
* Modified: 25-MAR-1999, M. Woodley (SLAC)                             *
*   Set default weight = 0 for ENERGY and CIRC                         *
* Modified: 03-MAY-1999, M. Woodley (SLAC)                             *
*   Change size of IGFLAG array (in /MTIRGO/) from 30 to 31            *
* Modified: 14-JUL-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 28 -> MAXVAL = 36; added /SYNCH/ common block     *
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
      integer maxlin,maxval,mconf1,mconf2,mconmn,mconmx,mcontp,mconvl,
     +mconwt
 
*---- Parameters for matching module.
      parameter         (maxlin = 16, maxval = 36)
      parameter         (mconf1 = 1, mcontp = 2, mconf2 = maxval + 2)
      parameter         (mconmn = mconf2 + 1)
      parameter         (mconmx = mconmn + maxval * mwflt)
      parameter         (mconvl = mconmx + maxval * mwflt)
      parameter         (mconwt = mconvl + maxval * mwflt)
      integer energy_val, chrom_val
      parameter         (energy_val = 27, chrom_val = 26)
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
 
*---- Flags for matching.
      common /mtcflg/   flbeta, florb, flrmat, fltmat, flchrm
      save              /mtcflg/
      logical           flbeta, florb, flrmat, fltmat, flchrm
      character *(mcnam)  sequd, betnm
      common / dmatchc / sequd(2), betnm(2)
      integer mtdbfl, imsequ
      common / dmatchi / mtdbfl, imsequ
      logical bdtflg
      common / dmatchl / bdtflg(2)
      double precision wgt
 
*---- Information for matching module.
      common /mtcwgt/   wgt(maxval)
      save              /mtcwgt/
      integer iformula,igflag,nuglob,nuloc
      double precision gpesi,gtarget
 
*---- Communication area for GLOBAL constraints.
      common /mtfrgo/   gtarget(29,2), gpesi(29)
      common /mtirgo/   nuglob, nuloc, igflag(32,2), iformula
      common /mtlrgo/   dynapflag, fixpointfl, statflag
      logical           dynapflag, fixpointfl, statflag
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
*---- Synchrotron integrals, etc.
      common /synch/    synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision  synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      integer imtflg,nl,i,locpt
      double precision piby2,worig
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (piby2 = pi / 2.0d0)
      dimension         worig(maxval)
 
*---- DATA WORIG        /  BETX,  ALFX,   MUX,  BETY,  ALFY,   MUY,
*                             X,    PX,     Y,    PY,     T,    PT,
*                            DX,   DPX,    DY,   DPY,
*                            WX,  PHIX,  DMUX,    WY,  PHIY,  DMUY,
*                           DDX,  DDPX,   DDY,  DDPY,
*                        ENERGY,  CIRC,    I1,    I2,    I3,    I4,
*                            I5,  I5I2,  I5I1,  DUMM  /
 
      data worig        /   1.0,  10.0,  10.0,   1.0,  10.0,  10.0,
     +                     10.0, 100.0,  10.0, 100.0,  10.0, 100.0,
     +                     10.0, 100.0,  10.0, 100.0,
     +                      0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     +                      0.0,   0.0,   0.0,   0.0,
     +                      0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
     +                      0.0,   0.0,   0.0,   0.0  /
      data imtflg       / 0 /
      save              imtflg
 
*---- Initialize global matching weights.
      gpesi(1)  = 10.
      gpesi(2)  = 10.
      gpesi(3)  = 1.
      gpesi(4)  = 1.
      gpesi(5)  = .1
      gpesi(6)  = .1
      gpesi(7)  = .01
      gpesi(8)  = .01
      gpesi(9)  = .01
      gpesi(10) = 100.
      gpesi(11) = 10.
      gpesi(12) = 10.
      gpesi(13) = 10.
      gpesi(14) = 1.
      gpesi(15) = 0.1
      gpesi(16) = 1.
      gpesi(17) = 1.
      gpesi(18) = 1.
      gpesi(19) = 1.
      gpesi(20) = 1.
      gpesi(21) = 1.
      gpesi(22) = 1.
      gpesi(23) = 1.
      gpesi(24) = 1.
      gpesi(25) = 1.
      gpesi(26) = 1.
      gpesi(27) = 1.
      gpesi(28) = 1.
      gpesi(29) = 1.
 
*---- Initialize link area.
      if (imtflg .eq. 0) then
        call mzlink(0, '/MTLINK/', lsmat, lmtlin, lptr)
        imtflg = 1
      else
        lsmat = 0
        lmcon = 0
        lmvar = 0
        lmtlin = 0
        lmtbet = 0
        lbeta0(1) = 0
        lbeta0(2) = 0
        lcon  = 0
        lref  = 0
        lvar  = 0
        lptr  = 0
      endif
 
*---- Lift banks for linking constraints
      if (mtdbfl .eq. 0)  then
        nl = iq(lq(lcseq-msdir)-1)
      else
        nl = 0
        do i = 1, mtdbfl
          if (sequd(i) .ne. ' ') call get_active(sequd(i), 'MTINIT')
          nl = max(nl, iq(lq(lcseq-msdir)-1))
        enddo
      endif
      call mzbook(2, lsmat, lsmat, 1, 'SMAT', 2, 2, 0, 0, 0)
      do i = 1, 2
        call mzbook(2, locpt, lsmat, -i, 'MSUB', nl, nl, 0, 2, 0)
      enddo
*---- Matching flags.
      flbeta = .false.
      florb  = .false.
      flrmat = .false.
      fltmat = .false.
      flchrm = .false.
 
*---- Default matching weights.
      call ucopy(worig, wgt, maxval*mwflt)
 
*---- Matching counters and switches.
      ilevel = 1
      istrat = 1
      ncon = 0
      nvar = 0
      nfcn = 0
      nfcnmx = 0
      icall = 0
      icovar = 0
      ifirst = 0
      iwork = 0
      nwork = 0
 
*---- Clear counters for user constraints.
      nuglob = 0
      nuloc = 0
 
*---- Initial matching parameters.
      call timex(time1)
      fmin = 1.0d5
      tol = 1.0d-8
      up = 1.0
      edm = 1.0d5 * up
 
      end
