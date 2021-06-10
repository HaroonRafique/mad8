      subroutine mtucon
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Constraints on STATIC variables and dynamic aperture (in MTGLOB).  *
* Attributes:                                                          *
*   q1        (const.)   Desired tune of mode 1                        *
*   q2        (const.)   Desired tune of mode 2                        *
*   xi1       (const.)   Desired chromaticity of mode 1                *
*   xi2       (const.)   Desired chromaticity of mode 2                *
*   xin1      (const.)   Desired nonlinear chromaticity of mode 1      *
*   xin2      (const.)   Desired nonlinear chromaticity of mode 2      *
*   dq1de1    (const.)   Desired anharmonicity of mode 1               *
*   dq1de2    (const.)   Desired cross anharmonicity                   *
*   dq2de2    (const.)   Desired anharmonicity of mode 2               *
*   dtune     (const.)   Desired fast detuning over turns/2            *
*   tunx      (const.)   Desired fast tune of mode x                   *
*   tuny      (const.)   Desired fast tune of mode y                   *
*   dynapfrac (const.)   Desired fractional dynamic aperture           *
*   smear     (const.)   Desired smear                                 *
*   turns     (const.)   Desired number of turns (for survival)        *
*   lyapunov  (const.)   Desired Lyapunov exponent                     *
*   xend      (const.)   Desired xend                                  *
*   pxend     (const.)   Desired pxend                                 *
*   yend      (const.)   Desired yend                                  *
*   pyend     (const.)   Desired pyend                                 *
*   tend      (const.)   Desired tend                                  *
*   ptend     (const.)   Desired ptend                                 *
*   wxmin     (const.)   Desired wxmin                                 *
*   wxmax     (const.)   Desired wxmax                                 *
*   wymin     (const.)   Desired wymin                                 *
*   wymax     (const.)   Desired wymax                                 *
*   wxymin    (const.)   Desired wxymin                                *
*   wxymax    (const.)   Desired wxymax                                *
*   formula   (const.)   Desired value for user-defined formula        *
*   call      (name)     Subroutine to be called before formula        *
*   sequence  (name)     name of matched sequence                      *
*   fixpoint  (logical)  flag for fixed point search                   *
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer i,k,idir,ileng
 
      character*(mcnam) newout, subnam, seqnm
 
*---- Set flag for quick call to LASTAT to compute only tunes
*     and chromaticities (unless nonlinear variables are needed).
      statflag   = .false.
      dynapflag  = .false.
      fixpointfl = .false.
 
      subnam = ' '
      call utgnam(lccmd, 30, 30, subnam)
*---- get sequence name
      seqnm = ' '
      call utgnam(lccmd, 31, 31, seqnm)
      if (seqnm .ne. ' ')  then
*---  keep sequence pointer
        k = 0
        do i = 1, 2
          if (seqnm .eq. sequd(i))  k = i
        enddo
        if (k .eq. 0)  then
          call utleng(seqnm, ileng)
          msg(1) = 'Global SEQUENCE "' // seqnm(1:ileng)
     +    // '" not in MATCH or CELL command - first sequence used.'
          call aawarn('MTUCON', 1, msg)
          imsequ = 1
        else
          imsequ = k
        endif
      else
        imsequ = 1
      endif
*---- Copy data to local storage.
      call uzero(gtarget(1,imsequ), 1, 29 * mwflt)
      call utgflt(lccmd, 1, 29, gtarget(1,imsequ))
      if (sequd(imsequ) .ne. ' ' .and. sequd(imsequ) .ne. sequnam)
     +call get_active(sequd(imsequ), 'MTUCON')
      call utgtyp(lccmd, igflag(1,imsequ))
 
*---- Count constraint.
      nuglob = 0
      do i = 1, 29
        if (igflag(i,imsequ) .ne. 0) then
          nuglob = nuglob + 1
          if (i .lt. 10) then
            statflag = .true.
          else if (i .lt. 29) then
            dynapflag = .true.
          endif
        endif
      enddo
 
      ncon = ncon + nuglob
 
*---- Initialise for CALL switch.
      iformula = 0
      lmtsub = 0
      if (subnam .ne. ' ') then
        call utleng(subnam, ileng)
        call difind(ldbnk, subnam(1:ileng), idir, lmtsub)
        if (lmtsub .eq. 0) then
          call aafail('AASUBR', 1,
     +      '"' // subnam(1:ileng) // '" is not a subroutine.')
        else if (iq(lmtsub+mbpr) .ne. mpsub  .or.
     +           iq(lmtsub+mbsp) .ne. 5) then
          call aafail('AASUBR', 1,
     +      '"' // subnam(1:ileng) // '" is not a subroutine.')
        else
          newout = 'CALL'
          call flopen(newout, 'SWFD', 0, 0, iformula, error)
        endif
      endif
 
*---- Set Fixpoint flag.
      if (igflag(32,imsequ) .ne. 0) fixpointfl = .true.
 
 9999 end
