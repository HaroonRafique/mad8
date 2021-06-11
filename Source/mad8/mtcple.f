      subroutine mtcple
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   COUPLE Command.                                                    *
* Attributes:                                                          *
*   RANGE    (range)   Where to apply the constraint.                  *
*   MUX      (real)    Horizontal phase difference.                    *
*   MUY      (real)    Vertical phase difference.                      *
*----------------------------------------------------------------------*
* Modified: 07-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 26 -> MAXVAL = 27; added ENERGY_VAL parameter to  *
*   check for linear constraints; added linear constrain checking      *
* Modified: 04-MAR-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 27 -> MAXVAL = 28                                 *
* Modified: 14-JUL-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 28 -> MAXVAL = 34                                 *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer idata,ipos1,ipos2,jatt,jcon,nd,nfun
      logical flwarn
 
*---- Valid range required.
      idata = mbat
      if (iq(lccmd+idata+mctyp) .ne. 10*mtrng + 1) then
        call aafail('MTCPLE', 1, 'COUPLE command requires a RANGE.')
        go to 9999
      endif
      lcatt = lq(lccmd-1)
      call utgrng(lcatt, lcseq, ipos1, ipos2, error)
      if (error) go to 9999
 
*---- Lift constraint bank.
*     NOTE. First link must always be zero (see MTPINI).
      nd = 2 + maxval + 4 * maxval * mwflt
      call mzbook(2, lcon, lmcon, 1, 'CPLE', 4, 2, nd, 7, 0)
      iq(lcon+mconf1) = 16 * maxval + 2
      iq(lcon+mconf2) = 16 * 4 * maxval * mwflt + mreal
      nfun = 0
 
*---- Fill in constraint types: Cannot constrain orbit.
      do 10 jcon = 1, maxval
        if (jcon .le. 6  .or.  jcon .ge. 11) then
          icc(jcon) = 4
          cwgt(jcon) = wgt(jcon)
        else
          icc(jcon) = 0
          cwgt(jcon) = 0.0
        endif
        cmin(jcon) = 0.0
        cmax(jcon) = 0.0
   10 continue
 
*---- Desired values for phase differences.
      do 20 jatt = 2, 3
        jcon = 3 * (jatt - 1)
        lcatt = lq(lccmd-jatt)
        if (lcatt .ne. 0) then
 
*---- Minimum value.
          lcexp = lq(lcatt-1)
          if (lcexp .ne. 0) then
            call mzcopy(2, lcexp, 2, lcon, -2, 'Z')
            lcexp = lq(lcon-2)
            iq(lcexp+mxsiz*iq(lcexp-3)+mxval) = mconmn + (jcon-1)*mwflt
            call exlkex
          else
            call ucopy(q(lcatt+mcval+2), cmin(jcon), mwflt)
          endif
 
*---- Maximum value.
          lcexp = lq(lcatt-2)
          if (lcexp .ne. 0) then
            call mzcopy(2, lcexp, 2, lcon, -2, 'Z')
            lcexp = lq(lcon-2)
            iq(lcexp+mxsiz*iq(lcexp-3)+mxval) = mconmx + (jcon-1)*mwflt
            call exlkex
          else
            call ucopy(q(lcatt+mcsiz+mcval+2), cmax(jcon), mwflt)
          endif
 
*---- Constraint type and weight.
          icc(jcon) = iq(lcatt+2)
          cwgt(jcon) = wgt(jcon)
 
*---- Not constrained.
        else
          icc(jcon) = 0
          cmin(jcon) = 0.0
          cmax(jcon) = 0.0
          cwgt(jcon) = 0.0
        endif
   20 continue
 
*---- Count constraints.
      flwarn = .false.
      do 30 jcon = 1, maxval
        if (cwgt(jcon) .eq. 0.0) then
          icc(jcon) = 0
        else
          nfun = nfun + 1
*---- Tor: check for linear constraints
          if (jcon .gt. maxlin .and. jcon .lt. energy_val) then
            flchrm = .true.
            flwarn = .true.
          endif
        endif
   30 continue
 
*---- Tor: warn user that no coupling can be present.
      if (flwarn) then
        msg(1) = 'You have selected a chromatic function for matching.'
        msg(2) = 'The match module will IGNORE any coupling.'
        call aawarn('MTCONS', 2, msg)
      endif
 
*---- Copy data to constraint bank.
      call ucopy(icc, iq(lcon+mcontp), maxval)
      call ucopy(cmin, iq(lcon+mconmn), maxval*mwflt)
      call ucopy(cmax, iq(lcon+mconmx), maxval*mwflt)
      call ucopy(cwgt, iq(lcon+mconwt), maxval*mwflt)
 
*---- Link constraint to proper places.
      imsequ = 1
      call mtacon(ipos1, 2, 0, error)
      call mtacon(ipos2, 3, nfun, error)
      ifirst = 0
      icovar = 0
      flbeta = .true.
 
 9999 end
