      subroutine erfcca(ipos, ncount, idum2, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:
*   Assign field errors to current element.
* Input:
*   IPOS      (integer) Current position number.
*   NCOUNT(2) (integer) Counter for field error sets assigned.
*   IDUM2     (integer) Unused.
* Output:
*   EFLAG     (logical) Error flag.
*--- doom modification: store order and reference radius of multipoles
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
      integer ibase,ibegin,idum2,iend,ileng,inorma,inormr,iocc,iord,
     +iorder,ipos,ipr,iskewa,iskewr,isp,jbyt,jnorma,jnormr,jskewa,
     +jskewr,knorma,knormr,kskewa,kskewr,mnorma,mord,mrad,nd,
     +nord,mproc
      double precision corstr,el,err1,err2,fact,factor,flderr,radius,
     +relerr,strg
      logical           eflag, proc_flag
      integer           ncount(2)
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
      integer meangb,meangg,meangr,mechg,mee1b,mee1g,mee2b,mee2g,meflde,
     +mefrqc,megapb,megapg,meh1b,meh1g,meh2b,meh2g,mehrmc,meintb,meintg,
     +mek1b,mek1g,mek1q,mek2b,mek2s,mek3b,mek3o,mekick,meklm,meksg,
     +mekss,melagc,melen,mesigx,mesigy,metltb,metlte,metltg,metltm,
     +metlto,metltq,metlts,metyp,mevltc,mexcol,mexma,meycol,meyma
      integer meintbx,meintgx,meapro,mek0lm,met0m,mek1lm,met1m,
     +mek2lm,met2m,mek3lm,met3m,meaprm,meapss,melosc,meaprc,mee0l,
     +medel,mephil,mefrql,melosl,mevoll,melagl,meaprl
 
*---- Bias for element attribute values.
*     These statements MUST be consistent with the command dictionary.
*     Routines using this group must also include BANKHEAD and CMDGROUP.
*     Common to all elements: TYPE and L attributes.
      parameter    (metyp  = mbat   + mcval, melen  = metyp  + mcsiz)
*     Common to RBEND and SBEND.
      parameter    (meangb = melen  + mcsiz, mek1b  = meangb + mcsiz,
     +              mee1b  = mek1b  + mcsiz, mee2b  = mee1b  + mcsiz,
     +              metltb = mee2b  + mcsiz, mek2b  = metltb + mcsiz,
     +              meh1b  = mek2b  + mcsiz, meh2b  = meh1b  + mcsiz,
     +              megapb = meh2b  + mcsiz, meintb = megapb + mcsiz)
      parameter (meintbx = meintb + mcsiz, mek3b  = meintbx + mcsiz)
*     QUADRUPO.
      parameter    (mek1q  = melen  + mcsiz, metltq = mek1q  + mcsiz)
      integer meaprq
      parameter    (meaprq = metltq + mcsiz)
*     SEXTUPOL.
      parameter    (mek2s  = melen  + mcsiz, metlts = mek2s  + mcsiz)
      integer meaprs
      parameter    (meaprs = metlts + mcsiz)
*     OCTUPOLE.
      parameter    (mek3o  = melen  + mcsiz, metlto = mek3o  + mcsiz)
      parameter    (meapro = metlto + mcsiz)
*     MULTIPOL.
      parameter    (mek0lm = melen  + mcsiz, met0m  = mek0lm + mcsiz,
     +              mek1lm = met0m  + mcsiz, met1m  = mek1lm + mcsiz,
     +              mek2lm = met1m  + mcsiz, met2m  = mek2lm + mcsiz,
     +              mek3lm = met2m  + mcsiz, met3m  = mek3lm + mcsiz,
     +              meaprm = melen  + 21*mcsiz)
*     MULTIPOL.
      parameter    (meklm  = melen  + mcsiz, metltm = meklm  + mcsiz)
*     SOLENOID.
      parameter    (mekss  = melen  + mcsiz, meapss = mekss  + mcsiz)
*     RFCAVITY.
      parameter    (mevltc = melen  + mcsiz, melagc = mevltc + mcsiz,
     +              mefrqc = melagc + mcsiz, mehrmc = mefrqc + mcsiz)
      parameter    (melosc = mehrmc + 5*mcsiz,
     +              meaprc = melosc + 3*mcsiz)
*     ELSEPARA.
      parameter    (meflde = melen  + mcsiz, metlte = meflde + mcsiz)
*     Common to SROT and YROT.
      parameter    (meangr = melen  + mcsiz)
*     Common to KICK, HKICK, and VKICK.
      parameter    (mekick = melen  + mcsiz)
*     Common to ECOLLIMA and RCOLLIMA.
      parameter    (mexcol = melen  + mcsiz, meycol = mexcol + mcsiz)
*     BEAMBEAM.
      parameter    (mesigx = melen  + mcsiz, mesigy = mesigx + mcsiz,
     +              mexma  = mesigy + mcsiz, meyma  = mexma  + mcsiz,
     +              mechg  = meyma  + mcsiz)
*     GBEND.
      parameter    (meangg = melen  + mcsiz, mek1g  = meangg + mcsiz,
     +              mee1g  = mek1g  + mcsiz, mee2g  = mee1g  + mcsiz,
     +              metltg = mee2g  + mcsiz, meksg  = metltg + mcsiz,
     +              meh1g  = meksg  + mcsiz, meh2g  = meh1g  + mcsiz,
     +              megapg = meh2g  + mcsiz, meintg = megapg + mcsiz)
*     lcavity.
      parameter    (mee0l  = melen  + mcsiz, medel  = mee0l  + mcsiz,
     +              mephil = medel  + mcsiz, mefrql = mephil + mcsiz,
     +              melosl = mefrql + mcsiz, mevoll = melosl + mcsiz,
     +              melagl = mevoll + mcsiz, meaprl = melagl + mcsiz)
      parameter (meintgx = meintg + mcsiz)
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
 
*---- Option for additive error components.
      common /erdata/   adderr
      logical           adderr
      save              /erdata/
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      integer maxmul
 
*---- Maximum order of multipoles.
      parameter         (maxmul = 20)
      parameter (mord = 1, mrad = 2, mnorma = 3, mproc = 87)
 
      character*(mcnam) elmnam
      dimension         flderr(2,0:maxmul), corstr(2)
 
*---- Test for valid beam element.
      eflag = .false.
      lcelm = lq(ldbnk(3)-iq(lsdir+ipos))
      if (lcelm .eq. 0) go to 9999
      ipr = iq(lcelm+mbpr)
      isp = iq(lcelm+mbsp)
      if (ipr.ne.mpelm .or. isp.le.0 .or. isp.gt.30) go to 9999
      call diname(ldbnk, iq(lsdir+ipos), elmnam)
      iocc = jbyt(iq(lsflg+ipos), mocc1, mocc2)
 
*---- Branch on subprocess code.
      go to (800, 100, 100, 800, 200, 250, 300, 350, 800, 800,
     +       800, 800, 800, 150, 150, 150, 800, 800, 800, 800,
     +       800, 800, 800, 800, 800, 400, 800, 800, 800, 800,
     +       800, 800, 800, 800, 800, 800, 800, 800, 800, 800), isp
 
*---- Bending magnets.
  100 continue
        call ucopy(q(lcelm+meangb), strg, mwflt)
        iorder = 0
        ibegin = 0
        iend = 3
      go to 700
 
*---- Corrector magnets.
  150 continue
        call ucopy(q(lcelm+mekick), corstr(1), mwflt)
        if (isp .eq. 15) then
          call ucopy(q(lcelm+mekick+mcsiz), corstr(2), mwflt)
        else
          corstr(2) = 0.0
        endif
        strg = sqrt(corstr(1)**2 + corstr(2)**2)
        iorder = 0
        ibegin = 0
        iend = 0
      go to 700
 
*---- Quadrupole.
  200 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek1q), strg, mwflt)
        strg = strg * el
        iorder = 1
        ibegin = 1
        iend = 1
      go to 700
 
*---- Sextupole.
  250 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek2s), strg, mwflt)
        strg = strg * el
        iorder = 2
        ibegin = 2
        iend = 2
      go to 700
 
*---- Octupole.
  300 continue
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mek3o), strg, mwflt)
        strg = strg * el
        iorder = 3
        ibegin = 3
        iend = 3
      go to 700
 
*---- Multipole.
  350 continue
        iorder = 0
        proc_flag = .false.
        call utgint(lccmd, mord, mord, iorder)
        call utglog(lccmd, mproc, mproc, proc_flag)
        ibase = 2 * iorder + 3
        strg = 0.0
        call utgflt(lcelm, ibase, ibase, strg)
        ibegin = 0
        iend = maxmul
      go to 700
 
*---- General bend.
  400 continue
        call ucopy(q(lcelm+meangb), strg, mwflt)
        iorder = 0
        ibegin = 0
        iend = 3
      go to 700
 
*---- Test for redefinition of errors.
  700 continue
      call uzero(flderr, 1, 2*mwflt*(maxmul+1))
      nord = -1
      lcfld = lq(lsfld-ipos)
      if (lcfld .eq. 0) then
        ncount(1) = ncount(1) + 1
      else
        if (adderr) then
          call ucopy(q(lcfld+1), flderr, iq(lcfld-1))
          nord = (iq(lcfld-1) / (2 * mwflt)) - 1
        endif
        ncount(2) = ncount(2) + 1
        call mzdrop(0, lcfld, '.')
      endif
 
*---- Use of relative error requires RADIUS to be given.
      radius = 1.0
      call utgflt(lccmd, mrad, mrad, radius)
*---- Calculate errors in any element.
      knorma = mnorma
      kskewa = knorma + 1 + iadim1(4)
      knormr = kskewa + 1 + iadim1(6)
      kskewr = knormr + 1 + iadim1(8)
      inorma = mbat + (knorma - 1) * mcsiz
      iskewa = mbat + (kskewa - 1) * mcsiz
      inormr = mbat + (knormr - 1) * mcsiz
      iskewr = mbat + (kskewr - 1) * mcsiz
 
*---- Loop over multipole components -
      if (proc_flag)  then
*--- first all normal, then skew
      do iord = 0, maxmul
 
*---- Normal component.
        err1 = 0.0
        jnorma = 0
        if (iord .le. iadim1(4)) then
          jnorma = mod(iq(lccmd+inorma+mctyp),10)
          if (jnorma .ne. 0) then
            if (jnorma .eq. 3) then
              lcexp = lq(lccmd-knorma-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+inorma+mcval), err1, mwflt)
          endif
        endif
        if (jnorma .eq. 0  .and.  iord .le. iadim1(8)) then
          jnormr = mod(iq(lccmd+inormr+mctyp),10)
          if (jnormr .ne. 0) then
            if (jnormr .eq. 3) then
              lcexp = lq(lccmd-knormr-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+inormr+mcval), relerr, mwflt)
            fact = factor(iord)/factor(iorder) * radius**(iorder-iord)
            err1 = relerr * strg * fact
          endif
        endif
*---- Can the current element handle this component?
        if (err1 .ne. 0.0) then
          if (iord .lt. ibegin  .or.  iord .gt. iend) then
            call utleng(elmnam, ileng)
            write (msg, 910) iord, elmnam(1:ileng), iocc
            call aawarn('ERFCCA', 1, msg)
          else
            flderr(1,iord) = flderr(1,iord) + err1
            if (iord .gt. nord) nord = iord
          endif
        endif
 
*---- Next component.
        inorma = inorma + mcsiz
        iskewa = iskewa + mcsiz
        inormr = inormr + mcsiz
        iskewr = iskewr + mcsiz
      enddo
 
      inorma = mbat + (knorma - 1) * mcsiz
      iskewa = mbat + (kskewa - 1) * mcsiz
      inormr = mbat + (knormr - 1) * mcsiz
      iskewr = mbat + (kskewr - 1) * mcsiz
 
      do iord = 0, maxmul
 
*---- Skewed component.
        err2 = 0.0
        jskewa = 0
        if (iord .le. iadim1(6)) then
          jskewa = mod(iq(lccmd+iskewa+mctyp),10)
          if (jskewa .ne. 0) then
            if (jskewa .eq. 3) then
              lcexp = lq(lccmd-kskewa-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+iskewa+mcval), err2, mwflt)
          endif
        endif
        if (jskewa .eq. 0  .and.  iord .le. iadim1(10)) then
          jskewr = mod(iq(lccmd+iskewr+mctyp),10)
          if (jskewr .ne. 0) then
            if (jskewr .eq. 3) then
              lcexp = lq(lccmd-kskewr-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+iskewr+mcval), relerr, mwflt)
            fact = factor(iord)/factor(iorder) * radius**(iorder-iord)
            err2 = relerr * strg * fact
          endif
        endif
 
*---- Can the current element handle this component?
        if (err2 .ne. 0.0) then
          if (iord .lt. ibegin  .or.  iord .gt. iend) then
            call utleng(elmnam, ileng)
            write (msg, 910) iord, elmnam(1:ileng), iocc
            call aawarn('ERFCCA', 1, msg)
          else
            flderr(2,iord) = flderr(2,iord) + err2
            if (iord .gt. nord) nord = iord
          endif
        endif
 
*---- Next component.
        inorma = inorma + mcsiz
        iskewa = iskewa + mcsiz
        inormr = inormr + mcsiz
        iskewr = iskewr + mcsiz
      enddo
      else
*--- normal + skew alternating (previous order)
      do 790 iord = 0, maxmul
 
*---- Normal component.
        err1 = 0.0
        jnorma = 0
        if (iord .le. iadim1(4)) then
          jnorma = mod(iq(lccmd+inorma+mctyp),10)
          if (jnorma .ne. 0) then
            if (jnorma .eq. 3) then
              lcexp = lq(lccmd-knorma-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+inorma+mcval), err1, mwflt)
          endif
        endif
        if (jnorma .eq. 0  .and.  iord .le. iadim1(8)) then
          jnormr = mod(iq(lccmd+inormr+mctyp),10)
          if (jnormr .ne. 0) then
            if (jnormr .eq. 3) then
              lcexp = lq(lccmd-knormr-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+inormr+mcval), relerr, mwflt)
            fact = factor(iord)/factor(iorder) * radius**(iorder-iord)
            err1 = relerr * strg * fact
          endif
        endif
 
*---- Skewed component.
        err2 = 0.0
        jskewa = 0
        if (iord .le. iadim1(6)) then
          jskewa = mod(iq(lccmd+iskewa+mctyp),10)
          if (jskewa .ne. 0) then
            if (jskewa .eq. 3) then
              lcexp = lq(lccmd-kskewa-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+iskewa+mcval), err2, mwflt)
          endif
        endif
        if (jskewa .eq. 0  .and.  iord .le. iadim1(10)) then
          jskewr = mod(iq(lccmd+iskewr+mctyp),10)
          if (jskewr .ne. 0) then
            if (jskewr .eq. 3) then
              lcexp = lq(lccmd-kskewr-iord)
              call exeval(lcexp)
            endif
            call ucopy(q(lccmd+iskewr+mcval), relerr, mwflt)
            fact = factor(iord)/factor(iorder) * radius**(iorder-iord)
            err2 = relerr * strg * fact
          endif
        endif
 
*---- Can the current element handle this component?
        if (err1**2 + err2**2 .ne. 0.0) then
          if (iord .lt. ibegin  .or.  iord .gt. iend) then
            call utleng(elmnam, ileng)
            write (msg, 910) iord, elmnam(1:ileng), iocc
            call aawarn('ERFCCA', 1, msg)
          else
            flderr(1,iord) = flderr(1,iord) + err1
            flderr(2,iord) = flderr(2,iord) + err2
            if (iord .gt. nord) nord = iord
          endif
        endif
 
*---- Next component.
        inorma = inorma + mcsiz
        iskewa = iskewa + mcsiz
        inormr = inormr + mcsiz
        iskewr = iskewr + mcsiz
  790 continue
      endif
*---- Allocate space for errors.
      if (nord .ge. 0) then
        nd = 2 * mwflt * (nord + 1)
        call mzbook(2, lcfld, lsfld, -ipos, 'EFLD', 0, 0, nd, mreal, 0)
        call ucopy(flderr, q(lcfld+1), nd)
      else
        call utleng(elmnam, ileng)
        write (msg, 920) elmnam(1:ileng), iocc
        call aawarn('ERFCCA', 1, msg)
      endif
 
  800 continue
 
  910 format('Element cannot handle field errors of order ',i2,
     +       ': ',a,'[',i8,'].')
  920 format('Field errors for the following element are zero: ',
     +       a,'[',i8,'].')
 
 9999 end