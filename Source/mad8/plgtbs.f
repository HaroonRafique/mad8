      subroutine plgtbs (sval, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Returns table information such as biasses for all var. etc.        *
* Input:                                                               *
*   SVAL     (char)     table name                                     *
* Output:                                                              *
*   variables in /PLCOMM/                                              *
*   Top bank LTBR:                                                     *
*                  1 = no. of valid rows (refs. in daughter 1)         *
*                  2 = no. of valid segments (refs. daughter 2)        *
*                  3 = 0 if no parameter (all in one frame)            *
*                      1 if one frame per row                          *
*                      2 if one frame per segment                      *
*   IERR     (integer)  error flag:                                    *
*                       0 = OK                                         *
*                       else > 0                                       *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      integer maux,maxitp,maxppt,mdsv,mint,mksmax,mntmax,mnvar,mpanno,
     +mpascl,mpbars,mpbtit,mpcolr,mpfelm,mpfont,mpfram,mplscl,mplscw,
     +mpmax,mpmin,mpmxvr,mpname,mpparn,mppcrn,mpsclf,mpspli,
     +mpsscl,mpstyl,mpsymb,mpsymf,mptscl,mpttit,mpvaxr,mpxsiz,mpysiz,
     +mqadd,mtbv,mtitl,musrv,mxdep,mxipar,mxlabl,mxqbnk,mxqcnd
 
      real              pflmax
 
      parameter         (mpparn = 11, mppcrn = 170)
      parameter         (mpmxvr = 5,  mxipar = 8, mtitl  = 128)
      parameter         (mxlabl = 40, pflmax = 1.e20)
      parameter         (mtbv = 6, mdsv = 3, musrv = 3)
      parameter         (maxppt = 20000, mnvar = 74, mxdep = 2)
      parameter         (mint = 10, maux = mint + 1, maxitp = 5000)
      parameter         (mxqcnd = 10, mxqbnk = 1000, mqadd = 100000)
      parameter         (mntmax = 20, mksmax = 10)
 
      parameter         (mpfont = 1, mpxsiz = 3, mpysiz = 4)
      parameter         (mplscl = 6, mptscl = 8, mpascl = 5)
      parameter         (mplscw = 2, mpsscl = 7, mpfelm = 9)
      parameter         (mpfram = 2, mpmin  = 1, mpmax  = 2)
      parameter         (mpsclf = 3, mpvaxr = 4, mpname = 5)
      parameter         (mpstyl = 1, mpspli = 2, mpbars = 3)
      parameter         (mpsymf = 4, mpcolr = 5, mpsymb = 6)
      parameter         (mpanno = 7)
      parameter         (mpttit = mpname + mtitl / mcwrd)
      parameter         (mpbtit = mpttit + mtitl / mcwrd)
 
*--- preceding parameters: see LPMAIN description (routine PLPLOT)
      integer idsbis,idsfrm,ihpntr,iqrang,irg1,irg2,irpos,itbv,ivpar,
     +ivpntr,laux,lbias,lbuf,lcnt,lexpv,lform,lframe,lhval,lindx,lm1,
     +lm2,locc,lpint,lpmain,lpparl,lproc,lqv1,lrvc,lrvv,ltab,ltbr,ltmp,
     +lvcurv,lvrw,lvsg,lvval,lvvar,nexpvr,nform,nntv,nocc,ntmax,ntvvar,
     +nvvar
      double precision usrv
      common /plcomm/      lpmain, ltbr, lexpv, ltab, lvsg, lvrw, locc,
     +                     lcnt, lproc, lform, lbias, lpint, lm1, lm2,
     +                     ltmp, lframe, lvvar, lvcurv, lhval, lvval,
     +                     lindx, lpparl, lrvv(4), laux(maux), lqv1,
     +                     lrvc(4*mpmxvr), lbuf
      save   /plcomm/
      common /plcoms/ haxis, vaxis, type, table, sparm, title,
     +                plfnam, plpnam, qcond(mxqcnd)
      save   /plcoms/
      character*(mcnam) haxis, type, table, sparm,
     +                  vaxis(mpmxvr,4)
      character*(mtitl) title
      character*(mcstr) qcond, plfnam, plpnam
      common /plcomp/      nntv(musrv), ntvvar, ihpntr, nocc, nform,
     +                     idsbis(mtbv), idsfrm(mtbv), irg1, irg2, itbv,
     +                     ntmax, nexpvr,
     +                     sortfl, splifl, multfl, fftfl, dumpfl,
     +                     helpfl,
     +                     ivpntr(mpmxvr,4), nvvar(4), ivpar(mxipar),
     +                     irpos(2), iqrang(3,mxqcnd), hrange(2),
     +                     vrange(2,4), qsval
      save   /plcomp/
 
      real                 hrange, vrange, qsval
 
      logical              sortfl, splifl, multfl, fftfl, dumpfl, helpfl
 
      common /plcomd/      usrv(25, musrv)
      save   /plcomd/
      integer icvref,iframe,ipar,ipxval,ipyval,ivnarw,nptval
      common /plotcl/   fpmach
      save   /plotcl/
 
      logical           fpmach
      common /plotcr/   yvtop, fdum, chh,
     +vpt(4), window(4,4), actwin(4,4), range(2), xax(2), yax(8)
      save   /plotcr/
 
      real              yvtop, fdum, chh
      real              vpt, window, actwin, range, xax, yax
 
      common /plotci/   iframe, ivnarw,
     +                  ipar(50), nptval(4), ipxval(4), ipyval(4),
     +                  icvref(4)
      save   /plotci/
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer mtbact,mtbbky,mtbbuf,mtbcfm,mtbcnm,mtbcol,mtbcps,mtbdsc,
     +mtbf1,mtbf2,mtbfst,mtblst,mtbmod,mtbnam,mtbrow,mtbseg,mtbsiz,
     +mtbsky,mtbwid
 
*---- Parameters for table manager bank structure.
      parameter         (mtbact = 1, mtbbuf = 2, mtbmod = 1)
      parameter         (mtbf1  = 1,
     +                   mtbseg = 2, mtbrow = 3, mtbcol = 4, mtbwid = 5,
     +                   mtbf2  = 6,
     +                   mtbnam = 7, mtbsiz = mtbnam + mwnam - 1)
      parameter         (mtbsky = 2, mtbbky = 3, mtbcnm = 4,
     +                   mtbcfm = 5, mtbcps = 6, mtbdsc = 7,
     +                   mtbfst = 8, mtblst = 9)
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer i,icopy,ienum,ierr,iflag,iocc,ipcd,itpv,j,jbyt,jrow,jseg,
     +k,kf,kl,kt,ll,msegin,nloc,nmrow,nsbsiz,nsup,nvrw,nvsg
      double precision eps,one,tpval,tval,two,twopi,zero
      parameter         (zero = 0.d0)
      parameter         (one = 1.d0, two = 2.d0, twopi = two * pi)
 
      parameter            (msegin = 1000)
*
*     MSEGIN               increment for segment bank
*
      dimension            eps(3)
      real                 hrgdef(2,mtbv), vrgdef(2,4,mtbv)
*
*     EPS                  limit for diff. between user and table val.
*     HRGDEF               default values for hor. ranges
*     VRGDEF               default values for vertical ranges
*
 
      character         sval*(*)
      character         tnam*(mcnam), dum1*(mcnam), dum2*(mcrng)
      character         stbv(mtbv)*(mcnam)
      character         spav(mdsv,mtbv)*(mcnam)
*
*     SVAL                 input variable
*     TNAM                 table name
*     DUM1, DUM2           dummy variables
*     STBV                 table names
*     SPAV                 potential parameter names
*                          (always 1 = row, 2 = segment)
 
      integer              npav(mtbv), ndfv(mtbv), nmlv(mtbv)
      integer              jppv(mdsv,mtbv), ivdf(mxipar,mtbv)
      integer              ieqd(2)
      equivalence          (tval, ieqd(1))
*
*     NPAV                 no. of parameters in SPAV
*     NDFV                 number of the default parameter in SPAV
*     NMLV                 if 1 for a table, "multiple" flag used
*     JPPV                 -1   if parameter corresponds to a range
*                          0    if simple column
*                          > 0  : pointer to count in NNTV and
*                          to user specified values in USRV
*     IVDF                 plot parameter defaults (--> IVPAR)
 
      logical              eflag, symm, rangfl, valid, plstrg
 
      data eps / 1.e-6, 0.1, 0.1 /
 
*--- reminder: last table must remain 'any'
 
      data stbv / 'TWISS', 'TUNES', 'TRACK', 'SITF', 'EIGEN', 'any' /
      data spav / 'S',        'DELTAP', ' ',
     +            'DELTAP',   ' ',      ' ',
     +            'PARTICLE', 'TURNS',  ' ',
     +            ' ',        ' ',      ' ',
     +            'S',        ' ',      ' ',
     +            ' ',        ' ',      ' ' /
 
      data npav / 2, 1, 2, 0, 1, 0 /
      data ndfv / 2, 1, 1, 1, 1, 1 /
      data nmlv / 0, 0, 1, 0, 0, 0 /
      data jppv / -1, 1, 0,
     +             1, 0, 0,
     +             2, 3, 0,
     +             0, 0, 0,
     +            -1, 0, 0,
     +             0, 0, 0 /
      data ivdf / 0, 1, 0, 10, 1, -1, -1, -1,
     +            0, 1, 0, 10, 1, -1, -1, -1,
     +            0, 0, 1, 10, 1, -1, -1, -1,
     +            0, 1, 2, 10, 1, -1, -1, -1,
     +            0, 1, 0, 10, 1, -1, -1, -1,
     +            0, 1, 0, 10, 1, -1, -1, -1 /
 
      data hrgdef / 2 * pflmax,
     +              2 * pflmax,
     +              2 * pflmax,
     +              2 * pflmax,
     +              2 * pflmax,
     +              2 * pflmax /
      data vrgdef / 8 * pflmax,
     +              8 * pflmax,
     +              8 * pflmax,
     +              0., 100., 0., 100., 0., 100., 0., 100.,
     +              8 * pflmax,
     +              8 * pflmax /
 
      ierr = 0
      tnam = sval
      tpval = -1.e20
      do 10 itbv = 1, mtbv - 1
        if (tnam .eq. stbv(itbv))  goto 20
   10 continue
      call aawarn('PLGTBS', 1,
     +'Default plot for unknown table = ' // tnam)
      itbv = mtbv
   20 continue
      if (itbv .eq. 1 .and. .not. (stabx .and. staby)) then
        ierr = 1
        goto 999
      endif
*--- get biasses and formats of parameters
      rangfl = .false.
      do 30  i = 1, npav(itbv)
        rangfl = rangfl .or. jppv(i,itbv) .lt. 0
        call tbcol (ltab, spav(i,itbv), idsfrm(i), idsbis(i))
        if (idsfrm(i) .eq. 0)  then
          call aawarn('PLGTBS', 1,
     +    'Table does not contain ' // spav(i,itbv))
          ierr = 2
          goto 999
        endif
   30 continue
*--- set plot defaults
      do 40  i = 1, mxipar
        if (ivpar(i) .lt. 0)  ivpar(i) = ivdf(i,itbv)
   40 continue
*--- plot ranges
      do 41  i = 1, 2
        if (hrange(i) .eq. pflmax)  hrange(i) = hrgdef(i,itbv)
        do 41  j = 1, 4
          if (vrange(i,j) .eq. pflmax)  vrange(i,j) = vrgdef(i,j,itbv)
   41 continue
*--- ranges if any
      if (rangfl)  then
        call utbeam (lq(ltab-1), irg1, irg2, symm, nsup, dum1, dum2)
        if (irg1 .eq. 0 .or. irg2 .eq. 0) then
          call aawarn('PLGTBS', 1,
     +    'Table "' // tnam // '" contains no range.')
          ierr = 3
          goto 999
        endif
*--- limit to user range if valid
        if (irpos(1) .gt. 0 .and. irpos(2) .ge. irpos(1)) then
          if (irpos(1) .gt. irg2 .or. irpos(2) .lt. irg1) then
            write (dum2, '(4I8)') irpos(1), irpos(2), irg1, irg2
            call aawarn('PLGTBS', 1,
     +      'Conflicting user and table range: ' // dum2)
            ierr = 4
            goto 999
          else
            irg1 = max (irg1, irpos(1))
            irg2 = min (irg2, irpos(2))
          endif
        endif
      else
        irg1 = 1
        irg2 = iq(ltab+mtbrow)
      endif
*--- find plot parameter (one frame for each value of this par.)
      if (sparm .eq. ' ') sparm = spav(ndfv(itbv),itbv)
      if (sparm .ne. ' ')  then
        call gxpnbl(sparm, kf, kl)
        do 50  itpv = 1, mdsv
          if (sparm(:kl) .eq. spav(itpv,itbv)(:kl)) goto 60
   50   continue
        call aawarn('PLGTBS', 1, 'Unknown plot parameter = ' // sparm)
        ierr = 5
        goto 999
   60   continue
        sparm = spav(itpv,itbv)
      endif
      call gxpnbl(haxis, kf, kl)
      if (sparm(:kl) .eq. haxis(:kl)) then
*--- if parameter is hor. axis, switch to other if any
        do 70  i = 1, mdsv
          if (i .ne. itpv)  then
            sparm = spav(i,itbv)
            goto 80
          endif
   70   continue
   80   itpv = i
      endif
      if (sparm .eq. ' ' .or. multfl .and. nmlv(itbv) .ne. 0
     +.or. npav(itbv) .le. 1)  itpv = 0
*--- max. no. of rows
      nmrow = irg2 + 1 - irg1
      if (jppv(1,itbv) .gt. 0)  then
      if (nntv(jppv(1,itbv)) .gt. 0)
     +nmrow = min(nmrow, nntv(jppv(1,itbv)))
      endif
*--- book work bank
      call mzbook (1, ll, ltbr, 1, 'WORK', 10, 10, 3, 0, 0)
*--- book bank for row numbers
      call mzbook (1, lvrw, ltbr, -1, 'VRW ', 0, 0, nmrow, 0, -1)
*--- book bank for segment numbers
      call mzbook (1, lvsg, ltbr, -2, 'VSG ', 0, 0, msegin, 0, -1)
*--- book bank for pointers to LFORM and LBIAS
      call mzbook (1, locc, ltbr, -3, 'OCC ', 0, 0, mnvar, 0, -1)
*--- book bank for counts in LFORM and LBIAS
      call mzbook (1, lcnt, ltbr, -4, 'CNT ', 0, 0, mnvar, 0, -1)
*--- book bank for process flag ( 0: take variable as is, 1: take root,
*    2: process code for function PLPVAL)
      call mzbook (1, lproc, ltbr, -5, 'PROC', 0, 0, mnvar, 0, -1)
*--- book bank for formats (2 = integer, 3 = real, else d.p.)
      call mzbook (1, lform, ltbr, -6, 'FORM', 0, 0, 2 * mnvar, 0, -1)
*--- book bank for biasses in table
      call mzbook (1, lbias, ltbr, -7, 'BIAS', 0, 0, 2 * mnvar, 0, -1)
*--- book bank for interpolation flag: if 0 use spline, else process
*    code in routine PLINTP (daughters are for temp. coord. banks)
      call mzbook (1, lpint, ltbr, -8, 'PINT', maux, maux, mnvar, 0,
     +-1)
*--- book banks for string expression variables
      nexpvr = 4 * mpmxvr + 1
      call mzbook (1, lexpv, lexpv, 1, 'EXPV', nexpvr, nexpvr, 0, 0,
     +-1)
      nexpvr = 0
      nocc = 0
      nform = 0
      nsbsiz = msegin
*--- get valid rows
      nvrw = 0
      do 100  jseg = 1, iq(ltab+mtbseg)
        call tbseg (ltab, jseg, eflag)
        if (.not.eflag)  then
          do 110   jrow = irg1, irg2
            call tbset(ltab, jrow, 1, lbuf)
            if (lbuf .ne. 0)  then
              if (itbv .eq. 1)  then
*--- twiss table
                call utelem (lcseq, jrow, iflag, dum1, iocc, ienum)
*--- keep only first point, and element end points
                valid = jrow .eq. 1 .or. jbyt(iflag, 1, mcode) .eq. 1
              else
                valid = .true.
              endif
              if (valid)  then
*--- valid row
                if (idsfrm(1) .eq. 2)  then
                  tval = iq(lbuf+idsbis(1)+1)
                elseif (idsfrm(1) .eq. 3)  then
                  tval = q(lbuf+idsbis(1)+1)
                else
                  do 111  icopy = 1, mwflt
  111             ieqd(icopy) = iq(lbuf+idsbis(1)+icopy)
                endif
                if (tval .ne. tpval)  then
                  tpval = tval
                  if (jppv(1,itbv) .gt. 0)  then
                    if (nntv(jppv(1,itbv)) .gt. 0)  then
*--- user has specified values - select rows accordingly
                      k = jppv(1,itbv)
                      do 120  j = 1, nntv(k)
                        if (abs(tval - usrv(j,k)) .le. eps(k))
     +                  goto 130
  120                 continue
                      goto 110
                    endif
                  endif
                endif
              endif
*--- store row number
  130         continue
              nvrw = nvrw + 1
              iq(lvrw+nvrw) = jrow
            endif
  110     continue
          goto 140
        endif
  100 continue
  140 continue
      if (nvrw .eq. 0)  then
        call aawarn('PLGTBS', 1, 'No rows in table ' // tnam)
        ierr = 6
        goto 999
      endif
*--- get valid segments
      nvsg = 0
      do 200  jseg = 1, iq(ltab+mtbseg)
        call tbseg (ltab, jseg, eflag)
        if (.not.eflag)  then
*--- valid segment
          do 210   jrow = irg1, irg2
            call tbset(ltab, jrow, 1, lbuf)
            if (lbuf .ne. 0)  then
              if (jppv(2,itbv) .gt. 0)  then
                if (nntv(jppv(2,itbv)) .gt. 0)  then
*--- user has specified values - select segments accordingly
                  if (idsfrm(2) .eq. 2)  then
                    tval = iq(lbuf+idsbis(2)+1)
                  elseif (idsfrm(2) .eq. 3)  then
                    tval = q(lbuf+idsbis(2)+1)
                  else
                    do 211  icopy = 1, mwflt
  211               ieqd(icopy) = iq(lbuf+idsbis(2)+icopy)
                  endif
                  k = jppv(2,itbv)
                  do 220  j = 1, nntv(k)
                    if (abs(tval - usrv(j,k)) .le. eps(k)) goto 230
  220             continue
                  goto 200
                endif
              endif
  230         continue
              if (nvsg .eq. nsbsiz)  then
                call mzpush(0, lvsg, 0, msegin, 'I')
                nsbsiz = nsbsiz + msegin
              endif
*--- store segment number
              nvsg = nvsg + 1
              iq(lvsg+nvsg) = jseg
              goto 200
            endif
  210     continue
        endif
  200 continue
      if (nvsg .eq. 0)  then
        call aawarn('PLGTBS', 1, 'No segments in table ' // tnam)
        ierr = 7
        goto 999
      endif
*--- store row no., segment no., type
      iq(ltbr+1) = nvrw
      iq(ltbr+2) = nvsg
      iq(ltbr+3) = itpv
*--- check for expression variable
      if (plstrg(haxis, ltab))  then
        call exstrg(haxis, ltab, lexpv, -(nexpvr + 1) , tval, eflag)
        nexpvr = nexpvr + 1
        ihpntr = - nexpvr
        dum1 = '_' // haxis
        haxis = dum1
      else
*--- biasses and formats for variables
        call plcoli(haxis, ihpntr, ipcd)
        if (ihpntr .eq. 0)  then
          call aawarn('PLGTBS', 1, 'Horizontal variable not found.')
          ierr = 8
          goto 999
        endif
      endif
      ntvvar = 0
      kt     = 0
      do 310 i = 1, 4
        nloc = 0
        do 300 j = 1, nvvar(i)
*--- check for expression variable
          if (plstrg(vaxis(j,i), ltab))  then
            call exstrg(vaxis(j,i), ltab, lexpv, -(nexpvr + 1),
     +      tval, eflag)
            nexpvr = nexpvr + 1
            nloc = nloc + 1
            kt   = kt + 1
            dum1 = '_' // vaxis(j,i)
            vaxis(j,i) = dum1
            vaxis(nloc,ntvvar+1)   = vaxis(j,i)
            ivpntr(nloc,ntvvar+1)  = - nexpvr
            iq(lpint+kt)           = 0
          else
            call plcoli(vaxis(j,i), ivpntr(j,i), ipcd)
            if (ivpntr(j,i) .eq. 0)  then
              call aawarn('PLGTBS', 1,
     +        'Ignoring non-existing variable = ' // vaxis(j,i))
              goto 300
            else
              nloc = nloc + 1
              kt   = kt + 1
              vaxis(nloc,ntvvar+1)   = vaxis(j,i)
              ivpntr(nloc,ntvvar+1)  = ivpntr(j,i)
              iq(lpint+kt)           = ipcd
            endif
          endif
  300   continue
        if (nloc .ne. 0)  then
          ntvvar           = ntvvar+1
          nvvar(ntvvar)    = nloc
          vrange(1,ntvvar) = vrange(1,i)
          vrange(2,ntvvar) = vrange(2,i)
        endif
  310 continue
  999 end
