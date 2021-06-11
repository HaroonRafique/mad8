      subroutine tmfrst(lseq, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Transfer matrix w.r.t. actual orbit for one (half) superperiod.    *
*   Misalignment and field errors are considered.                      *
* Input:                                                               *
*   LSEQ(1)   (pointer) Beam line expansion bank.                      *
*   EFLAG     (logical) Overflow flag.                                 *
* Output:                                                              *
* Important common data:                                               *
*   RT(6,6)   /MAPTRN/  Transfer matrix for one (half) superperiod.    *
*   ORBIT0(6) /OPTIC0/  Initial conditions for reference orbit.        *
*   ORBIT(6)  /OPTIC1/  Final conditions for reference orbit.          *
*   SUML      /OPTIC1/  Cumulated length.                              *
*   LCELM     /REFER/   Current element bank.                          *
*   LCALI     /REFER/   Current misalignment pointer.                  *
*   LCFLD     /REFER/   Current field error pointer.                   *
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
      integer i,ibpos,icode,icount,ieflag,ienum,iflag,inbpos,iocc,ipos,
     +itp,j,jbit,jbyt,k,nunloc,iml_flag
      double precision el,ten
      integer           lseq(*)
      logical           eflag
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
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mmaxel,mwind,ndccnt,ndflag,nditer,ndocc,ndpos,ndtype,
     +nlpos
      double precision admatr,adorbt,adsuml,adtol,orbkpt,reforb,skpt
*--- common block for threader variables
      parameter (mwind = 500, mmaxel = 20000)
      common/thrcml/adthfl, adwofl, adcofl
      logical adthfl, adwofl, adcofl
      common/thrcmi/ndccnt, ndocc, nlpos, nditer,
     +ndpos(mwind), ndtype(mwind), ndflag(mmaxel)
      common/thrcmr/adtol(6), reforb(6), adsuml(mwind),
     +adorbt(6,mwind), admatr(6,6,mwind), orbkpt(6,mmaxel),
     +skpt(mmaxel)
      parameter         (ten = 10.0d0)
 
      logical           fdump, fmap
      character*(mcnam) elmnam
      character * 60 stxt
 
*---- Initialize.
      eflag = .false.
      iml_flag = 1234
      icount = 0
      ieflag = 0
      if (adthfl)  ndccnt = ndccnt + 1
    3 continue
      call vzero(ndflag, mmaxel)
      call vzero(reforb, 6*mwflt)
    5 continue
      call ucopy(orbit0, orbit, 6*mwflt)
      cplxy = .false.
      cplxt = .false.
      suml = 0.0
      call m66one(rt)
*   buffer occupation
      ndocc = 0
*   position of current element ipos in buffer
      ibpos = 0
*--- (re)start position
      ipos = iq(lseq(1)+msr1) - 1
*   last ipos with flag set
      nlpos = ipos
   20 continue
      ipos = ipos + 1
      if (adthfl)  then
        ibpos = ibpos + 1
        if (ibpos .gt. mwind) ibpos = 1
        if (ndocc .lt. mwind)  ndocc = ndocc + 1
        ndtype(ibpos) = 0
      endif
        call utelem(lseq, ipos, iflag, elmnam, iocc, ienum)
        fdump = jbit(iflag,mfrst) .ne. 0
        icode = jbyt(iflag,1,mcode)
        if (fdump) write (iqlog, 910) elmnam
 
*---- Misalignment at entrance.
        if (icode .ne. 3  .and.  lcali .ne. 0) then
          call tmali1(ipos, .false., orbit, orbit, re, te)
          if (fdump) then
            write (iqlog, 920) ((re(i,j), j=1,6), i=1,6)
            write (iqlog, 950) orbit
          endif
          call m66mpy(re, rt, rt)
        endif
 
*---- Element matrix and length.
        if (icode .eq. 1) then
          if (adthfl)  then
            itp = iq(lcelm+mbsp)
            ndtype(ibpos) = itp
            ndpos(ibpos) = ipos
          endif
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
            if (fdump) then
              write (iqlog, 930) ((re(i,j), j=1,6), i=1,6)
              write (iqlog, 950) orbit
            endif
            call m66mpy(re, rt, rt)
            suml = suml + el
          endif
        endif
        if (adthfl)  adsuml(ibpos) = suml
*---- Misalignment at exit.
        if (icode .ne. 2  .and.  lcali .ne. 0) then
          call tmali2(ipos, .false., orbit, orbit, re, te)
          if (fdump) then
            write (iqlog, 940) ((re(i,j), j=1,6), i=1,6)
            write (iqlog, 950) orbit
          endif
          call m66mpy(re, rt, rt)
        endif
        if (adthfl)  then
          call ucopy(orbit, adorbt(1,ibpos), 6 * mwflt)
          call ucopy(rt, admatr(1,1,ibpos), 36 * mwflt)
          if (ipos .eq. iq(lseq(1)+msr2))  then
*--- close end of line
            if (ieflag .eq. 2)  goto 30
            ieflag = ieflag + 1
            call ucopy(orbit0, reforb, 6*mwflt)
            call tmthrd(lseq, ipos, ibpos, ieflag, inbpos)
            goto 17
          elseif (ndflag(ipos) .eq. 0)  then
            do 15  j = 1, 4
              if (abs(orbit(j) - reforb(j)) .gt. adtol(j)) then
                if (itp .lt. 14 .or. itp .gt. 16)  then
                  call tmthrd(lseq, ipos, ibpos, 1 + (j-1)/2, inbpos)
                  goto 17
                endif
              endif
   15       continue
          endif
          goto 16
   17     continue
          icount = icount + 1
          if (icount .gt. nditer)  then
            stxt = 'over $$$$$$ restart iterations -> Threader exit'
            write(stxt(6:11), '(i6)')  nditer
            call aawarn('tmfrst', 1, stxt)
            eflag = .true.
            goto 30
          endif
          if (inbpos .eq. 0)  goto 5
*--- restart at first corrector (from orbit + matrix of prev. element)
          ibpos = inbpos
          ipos = ndpos(inbpos)
          suml = adsuml(inbpos)
          call ucopy(adorbt(1,ibpos), orbit, 6*mwflt)
          call ucopy(admatr(1,1,ibpos), rt, 36*mwflt)
          goto 20
        endif
   16   continue
        if (adthfl)  then
          call ucopy(orbit, orbkpt(1,ipos), 6*mwflt)
          skpt(ipos) = suml
        endif
*---- Test for overflow.
        do 10 j = 1, 6
          if (abs(orbit(j)) .ge. ten) then
            eflag = .true.
            go to 30
          endif
   10   continue
        if (ipos .lt. iq(lseq(1)+msr2))      goto 20
   30 continue
      if (adthfl .and. adwofl)  then
        call flopen('threader.orbit', 'SWFD', 0, 0, nunloc, error)
        if (.not. error)  then
          write(nunloc,1001)
          k = iq(lseq(1)+msr1)
          write(nunloc,1002) k, skpt(k), (orbkpt(j,k), j = 1, 6)
          do 31 k = iq(lseq(1)+msr1)+1, ipos
            if (skpt(k) .gt. skpt(k-1))
     +      write(nunloc,1002) k, skpt(k), (orbkpt(j,k), j = 1, 6)
   31     continue
          call flclos(nunloc, error)
          if (.not. error)  then
            write(iqlog, *) 'Threader orbit written into threader.orbit'
          endif
        endif
      endif
*--- use threader only in first iteration
      adthfl = .false.
 
  910 format(' '/' TMFRST.  Entering "',a,'".')
  920 format(t11,'Misalignment at entrance:'/(' ',6e16.8))
  930 format(t11,'Element transfer matrix:'/(' ',6e16.8))
  940 format(t11,'Misalignment at exit:'/(' ',6e16.8))
  950 format(t11,'Orbit:'/(' ',6e16.8))
 1001 format('# ipos', t14, 's', t28, 'x', t41, 'p_x', t56, 'y',
     +t69, 'p_y', t83, '-ct', t97, 'd_p')
 1002 format(i6, 1p, 7e14.6)
 
  999 end
