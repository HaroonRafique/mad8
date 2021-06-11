      subroutine ersave
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save error definitions: ESAVE command.                             *
* Attribute:                                                           *
*   FILENAME  (string)  File name to receive output.                   *
*   ALIGN     (logical) Save alignment errors.                         *
*   FIELD     (logical) Save field errors.                             *
*   ORDER     (integer) Base order for normalisation.                  *
*   RADIUS    (real)    Radius for normalisation.                      *
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
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
      integer i,ibase,ibegin,iend,ienum,iflag,iocc,iord,iorder,ipos,isp,
     +j,jbit,jbyt,malign,mfield,mfile,morder,mrad,nali,nd,nfld
      double precision corstr,data,el,fact,factor,field,radius,strg
 
      integer maxmul
 
*---- Maximum order of multipoles.
      parameter         (maxmul = 20)
      parameter         (mfile  = 1, mfield = 2, malign = 3,
     +                   morder = 4, mrad   = 5)
 
      character*(mcfil) filnam
      character*(mcnam) elmnam
      logical           flag(2)
      dimension         corstr(2), data(8), field(2,0:maxmul)
 
*---- Check main beam line.
      call lnchck('ESAVE', error)
      if (.not. error) then
 
*---- Open desired file.
        filnam = 'esave'
        call utgstr(lccmd, 1, 1, filnam)
        call flopen(filnam, 'SWFD', 0, 0, isave, error)
        if (.not. error) then
 
*---- Get logical flags and clear counters.
          flag(1) = .false.
          flag(2) = .false.
          call utglog(lccmd, mfield, malign, flag)
          nali = 0
          nfld = 0
 
*---- Unpack sequence definition.
          lsdir = lq(lcseq-msdir)
          lsali = lq(lcseq-msali)
          lsfld = lq(lcseq-msfld)
          lcali = 0
          lcfld = 0
 
*---- Loop over beam line.
          do 900 ipos = 1, iq(lsdir-1)
            call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
            if (jbyt(iflag,1,mcode) .le. 2  .and.
     +          jbit(iq(lsflg+ipos), mserr) .ne. 0) then
 
*---- Write SELECT commands.
              if (flag(1) .and. lsali .ne. 0) lcali = lq(lsali-ipos)
              if (flag(2) .and. lsfld .ne. 0) lcfld = lq(lsfld-ipos)
              if (lcali .ne. 0  .or. lcfld .ne. 0) then
                write (isave, 920) elmnam, iocc
              endif
 
*---- Write alignment errors.
              if (lcali .ne. 0) then
                call uzero(data, 1, 8*mwflt)
                call ucopy(q(lcali+1),data,min(iq(lcali-1),8*mwflt))
                if (iq(lcali-1) .le. 6*mwflt) then
                  write (isave, 910) (data(j), j = 1, 6)
                else
                  write (isave, 910) (data(j), j = 1, 8)
                endif
                nali = nali + 1
              endif
 
*---- Write field error.
              if (lcfld .ne. 0) then
                nd = min(iq(lcfld-1),mwflt*(2*maxmul+2))
                call ucopy(q(lcfld+1), field, nd)
 
*---- Test for valid beam element.
                isp = iq(lcelm+mbsp)
                call diname(ldbnk, iq(lsdir+ipos), elmnam)
                iocc = jbyt(iq(lsflg+ipos), mocc1, mocc2)
 
*---- Branch on subprocess code.
        go to (900, 100, 100, 900, 200, 250, 300, 350, 900, 900,
     +         900, 900, 900, 150, 150, 150, 900, 900, 900, 900,
     +         900, 900, 900, 900, 900, 400, 900, 900, 900, 900,
     +         900, 900, 900, 900, 900, 900, 900, 900, 900, 900), isp
 
*---- Bending magnets.
 100            continue
                call ucopy(q(lcelm+meangb), strg, mwflt)
                iorder = 0
                ibegin = 0
                iend = 3
                go to 700
 
*---- Corrector magnets.
 150            continue
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
 200            continue
                call ucopy(q(lcelm+melen), el, mwflt)
                call ucopy(q(lcelm+mek1q), strg, mwflt)
                strg = strg * el
                iorder = 1
                ibegin = 1
                iend = 1
                go to 700
 
*---- Sextupole.
 250            continue
                call ucopy(q(lcelm+melen), el, mwflt)
                call ucopy(q(lcelm+mek2s), strg, mwflt)
                strg = strg * el
                iorder = 2
                ibegin = 2
                iend = 2
                go to 700
 
*---- Octupole.
 300            continue
                call ucopy(q(lcelm+melen), el, mwflt)
                call ucopy(q(lcelm+mek3o), strg, mwflt)
                strg = strg * el
                iorder = 3
                ibegin = 3
                iend = 3
                go to 700
 
*---- Multipole.
 350            continue
                iorder = 0
                call utgint(lccmd, morder, morder, iorder)
                ibase = 2 * iorder + 3
                strg = 0.0
                call utgflt(lcelm, ibase, ibase, strg)
                ibegin = 0
                iend = maxmul
                go to 700
 
*---- General bend.
 400            continue
                call ucopy(q(lcelm+meangb), strg, mwflt)
                iorder = 0
                ibegin = 0
                iend = 3
                go to 700
 
*---- Normalize the errors.
 700            continue
                call ucopy(q(lcfld+1), field, iq(lcfld-1))
                iend = min(iend, (iq(lcfld-1) / (2*mwflt)) - 1)
 
*---- Use of relative error requires RADIUS to be given.
                radius = 1.0
                call utgflt(lccmd, mrad, mrad, radius)
 
*---- Loop over multipole components.
                do 790 iord = ibegin, iend
                  fact = factor(iord) / factor(iorder) *
     +                  radius**(iorder-iord)
                  field(1,iord) = field(1,iord) / (strg * fact)
                  field(2,iord) = field(2,iord) / (strg * fact)
 790            continue
 
*---- Dipole component.
                write (savbuf, 925) radius, iorder
                if (ibegin .le. 0) then
                  savbuf(64:65) = ',&'
                  write (isave, 930) savbuf
                  write (savbuf, 940) field(1,0), field(2,0)
                endif
 
*---- Other components.
                do 800 i = max(ibegin, 1), iend
                  if (field(1,i).ne.0.0 .or. field(2,i).ne.0.0) then
                    savbuf(64:65) = ',&'
                    write (isave, 930) savbuf
                    write (savbuf, 950) i, field(1,i), i, field(2,i)
                  endif
 800            continue
                write (isave, 930) savbuf
                nfld = nfld + 1
              endif
            endif
 900      continue
 
*---- Completion message.
          write (isave, 960)
          call flname(isave, filnam)
          call flclos(isave, error)
          if (.not. error) then
            write (msg, 970) filnam, nali, nfld
            call aainfo('ERSAVE', 3, msg)
          endif
        endif
      endif
 
  910 format(1p,'EALIGN, DX =',e15.8,', DY =',e15.8,
     +       ', DS =',e15.8,',&'/
     +       '  DPHI =',e15.8,', DTHETA =',e15.8,
     +       ', DPSI =',e15.8,:,',&'/
     +       ' MREX =',e15.8,', MREY =',e15.8)
  920 format('SELECT, FLAG=ERROR, CLEAR'/
     +       'SELECT, FLAG=ERROR, RANGE = "',a,'"[',i5,']')
  925 format('EFCOMP, RADIUS = ',1p,e16.8,', ORDER =',i2)
  930 format(a80)
  940 format(5x,'DBLNR     = ',    1p,e16.8,', DBLSR     = ',    e16.8)
  950 format(5x,'DKLNR(',i2,') = ',1p,e16.8,', DKLSR(',i2,') = ',e16.8)
  960 format('SELECT, FLAG=ERROR, CLEAR'/'RETURN')
  970 format('Error definitions saved on file: ',a/
     +       i8,' misaligned elements,'/
     +       i8,' elements with field errors.')
 
      end
