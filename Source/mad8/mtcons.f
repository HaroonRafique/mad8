      subroutine mtcons
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   CONSTRAINT Command.                                                *
* Attributes:                                                          *
*   RANGE    (range)   Range to limit selections.                      *
*   CLASS    (name)    Class of elements to be affected in RANGE.      *
*   PATTERN  (string)  Regular expression to limit choice.             *
*   LINE     (line)    Match to this beam line.                        *
*   BETX     (real)    Horizontal beta.                                *
*   ALFX     (real)    Horizontal alpha.                               *
*   MUX      (real)    Horizontal phase.                               *
*   BETY     (real)    Vertical beta.                                  *
*   ALFY     (real)    Vertical alpha.                                 *
*   MUY      (real)    Vertical phase.                                 *
*   X        (real)    Horizontal orbit.                               *
*   PX       (real)    Horizontal orbit slope.                         *
*   Y        (real)    Vertical orbit.                                 *
*   PY       (real)    Vertical orbit slope.                           *
*   T        (real)    Longitudinal orbit.                             *
*   PT       (real)    Momentum error.                                 *
*   DX       (real)    Horizontal dispersion.                          *
*   DPX      (real)    Horizontal dispersion slope.                    *
*   DY       (real)    Vertical dispersion.                            *
*   DPY      (real)    Vertical dispersion slope.                      *
*   WX       (real)    Horizontal chromatic amplitude.                 *
*   PHIX     (real)    Horizontal chromatic phase.                     *
*   DMUX     (real)    Horizontal chromatic derivative of MUX.         *
*   WY       (real)    Vertical chromatic amplitude.                   *
*   PHIY     (real)    Vertical chromatic phase.                       *
*   DMUY     (real)    Vertical chromatic derivative of MUY.           *
*   DDX      (real)    Horizontal 2nd dispersion.                      *
*   DDPX     (real)    Horizontal 2nd dispersion slope.                *
*   DDY      (real)    Vertical 2nd dispersion.                        *
*   DDPY     (real)    Vertical 2nd dispersion slope.                  *
*   BETA0    (name)    Name of BETA0 bank defining constraints.        *
*   sequence (name)    Name of sequence for constraint                 *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer icc
      double precision cmax,cmin,cval,cwgt
 
*---- Working area for a single matching constraint.
      common /mtccon/   icc(maxval), cmin(maxval), cmax(maxval),
     +                  cwgt(maxval), cval(maxval)
      save   /mtccon/
 
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
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
      double precision wgt
 
*---- Information for matching module.
      common /mtcwgt/   wgt(maxval)
      save              /mtcwgt/
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer idata,idir,ileng,iline,jatt,jcon,mbeta,mclass,mcons,mline,
     +mpatt,mrange,nd,nfun,msequ,i,k
 
      parameter         (mrange = 1, mclass = 2, mpatt  = 3)
      parameter         (mline  = 4)
      parameter         (mcons  = 5, mbeta  = maxval + 5,
     +                   msequ = maxval + 6)
 
      external          mtacon
      character*(mcnam) betnam, class, seqnm
      character*(mcstr) patt
      logical           done, linflg, betflg, flwarn
 
*---- Lift constraint bank.
*     link 1 (structural): Sequence bank for LINE constraint,
*     link 2 (structural): Chain of expressions defining constraint values,
*     link 3 (reference):  BETA0 bank, if used.
      nd = 2 + maxval + 4 * maxval * mwflt
      call mzbook(2, lcon, lmcon, 1, 'CNST', 4, 2, nd, 7, 0)
      iq(lcon+mconf1) = 16 * maxval + 2
      iq(lcon+mconf2) = 16 * 4 * maxval * mwflt + mreal
      nfun = 0
 
*---- get sequence name
      seqnm = ' '
      call utgnam(lccmd, msequ, msequ, seqnm)
      if (seqnm .ne. ' ')  then
*---  keep sequence pointer
        k = 0
        do i = 1, 2
          if (seqnm .eq. sequd(i))  k = i
        enddo
        if (k .eq. 0)  then
          call utleng(seqnm, ileng)
          msg(1) = 'Constraint SEQUENCE "' // seqnm(1:ileng)
     +    // '" not in MATCH command - first sequence used.'
          call aawarn('MTCONS', 1, msg)
          imsequ = 1
        else
          imsequ = k
        endif
      else
        imsequ = 1
      endif
      if (sequd(imsequ) .ne. ' ' .and. sequd(imsequ) .ne. sequnam)
     +call get_active(sequd(imsequ), 'MTCONS')
*---- Link to BETA0 bank, if used.
      betnam = ' '
      call utgnam(lccmd, mbeta, mbeta, betnam)
      betflg = betnam .ne. ' '
      if (betflg) then
        call utleng(betnam, ileng)
        call difind(ldbnk, betnam(1:ileng), idir, lptr)
        if (idir .eq. 0) then
          msg(1) = 'Constraint BETA0 bank "' // betnam(1:ileng)
     +    // '" not found.'
          call aafail('MTCONS', 1, msg)
        else if (iq(lptr+mbpr).ne.mpenv .or. iq(lptr+mbsp).ne.2) then
          msg(1) = '"' // betnam(1:ileng) // '" is not a "BETA0" bank.'
          call aafail('MTCONS', 1, msg)
        else
          lq(lcon-3) = lptr
        endif
      endif
 
*---- Set up LINE constraint.
      iline = mbat + (mline - 1) * mcsiz
      linflg = mod(iq(lccmd+iline+mctyp),10) .ne. 0
      if (linflg) then
        if (betflg) then
          call aafail('MTCONS', 1,
     +    'Conflicting options BETA0 and LINE have been specified.')
        else
          call lnrefe(lccmd, mline, lmtseq, lcon, -1)
        endif
      endif
      if (error) go to 9999
 
*---- Copy remaining data.
      flwarn = .false.
      jatt = mcons
      idata = mbat
      do 90 jcon = 1, maxval
 
*---- Set up for unconstrained value.
        icc(jcon) = 0
        cmin(jcon) = 0.0
        cmax(jcon) = 0.0
        cwgt(jcon) = 0.0
        if (wgt(jcon) .ne. 0.0) then
          lcatt = lq(lccmd-jatt)
 
*---- BETA0 option. Cannot constrain phases.
          if (betflg  .and.  jcon .ne. 3  .and.  jcon .ne. 6) then
            if (florb  .or.  jcon .le. 6  .or.  jcon .gt. 12) then
              icc(jcon) = 4
              cwgt(jcon) = wgt(jcon)
              if (lq(lptr-jcon) .ne. 0) then
                call mzcopy(2, lq(lptr-jcon), 2, lcon, -2, 'Z')
                lcexp = lq(lcon-2)
                iq(lcexp+mxsiz*iq(lcexp-3)+mxval) =
     +            mconmn + (jcon - 1) * mwflt
                call exlkex
              else
                call ucopy(q(lptr+idata+mcval), cmin(jcon), mwflt)
              endif
            else
              icc(jcon) = 0
              cwgt(jcon) = 0.0
            endif
 
*---- LINE option. Cannot constrain phases.
          else if (linflg  .and.  jcon .ne. 3  .and.  jcon .ne. 6) then
            if (florb  .or.  jcon .le. 6  .or.  jcon .gt. 12) then
              icc(jcon) = 4
              cwgt(jcon) = wgt(jcon)
            else
              icc(jcon) = 0
              cwgt(jcon) = 0.0
            endif
 
*---- Other constraints.
          elseif (lcatt .ne. 0  .and.  iq(lcatt+2) .ne. 0) then
 
*---- For orbit constraints, set orbit flag.
            if (jcon .gt. 6  .and.  jcon .le. 12) florb = .true.
 
*---- Minimum value.
            if (lq(lcatt-1) .ne. 0) then
              call mzcopy(2, lq(lcatt-1), 2, lcon, -2, 'Z')
              lcexp = lq(lcon-2)
              iq(lcexp+mxsiz*iq(lcexp-3)+mxval) =
     +          mconmn + (jcon - 1) * mwflt
              call exlkex
            else
              call ucopy(q(lcatt+mcval+2), cmin(jcon), mwflt)
            endif
 
*---- Maximum value.
            if (lq(lcatt-2) .ne. 0) then
              call mzcopy(2, lq(lcatt-2), 2, lcon, -2, 'Z')
              lcexp = lq(lcon-2)
              iq(lcexp+mxsiz*iq(lcexp-3)+mxval) =
     +          mconmx+(jcon-1) * mwflt
              call exlkex
            else
              call ucopy(q(lcatt+mcsiz+mcval+2), cmax(jcon), mwflt)
            endif
 
*---- Constraint type and weight.
            cwgt(jcon) = wgt(jcon)
            icc(jcon) = iq(lcatt+2)
          endif
 
*---- Tor: check for linear constraints
*     (NOTE: this section moved here to check for BETA0 and LINES also)
          if (jcon .gt. maxlin .and. jcon .lt. energy_val) then
            flchrm = .true.
            flwarn = .true.
          endif
 
*---- Count constraints.
          if (icc(jcon) .ne. 0) then
            nfun = nfun + 1
            if (jcon .gt. maxlin) then
              flchrm = .true.
              flwarn = .true.
            endif
          endif
*---- Tor: if the weight is zero, but the function is being fit, issue
*     a warning message to remind the user
          lcatt = lq(lccmd-jatt)
          if (lcatt .ne. 0  .and.  iq(lcatt+2) .ne. 0 .and.
     +        wgt(jcon) .eq. 0.0) then
            msg(1) = 'You have chosen a constraint with weight = 0.'
            call aawarn('mtcons', 1, msg)
          endif
        endif
        jatt = jatt + 1
        idata = idata + mcsiz
   90 continue
 
*---- Warn user that no coupling can be present.
      if (flwarn) then
        msg(1) = 'You have selected a chromatic function for matching.'
        msg(2) = 'The match module will IGNORE any coupling.'
        call aawarn('MTCONS', 2, msg)
      endif
 
*---- Copy data to constraint bank.
      call ncopy(icc, iq(lcon+mcontp), maxval)
      call ucopy(cmin, q(lcon+mconmn), maxval*mwflt)
      call ucopy(cmax, q(lcon+mconmx), maxval*mwflt)
      call ucopy(cwgt, q(lcon+mconwt), maxval*mwflt)
 
*---- Call ENSRNG for setting constraints.
      class = ' '
      patt  = ' '
      call utgnam(lccmd, mclass, mclass, class)
      call utgstr(lccmd, mpatt,  mpatt,  patt)
      lcatt = lq(lccmd-mrange)
      call ensrng(lcatt, class, patt, mtacon, 1, nfun, done)
 
*---- Clean up.
      if (done  .and.  .not. error) then
        ifirst = 0
        icovar = 0
        flbeta = .true.
      else
        call lndrop(lq(lcon-1))
        call aadrop(lcon)
      endif
 
 9999 end
