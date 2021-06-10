      subroutine survey
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Execute SURVEY command.                                            *
* Attributes, must be given in this order in the dictionary:           *
*   X0        (real)    Initial X position.                            *
*   Y0        (real)    Initial Y position.                            *
*   Z0        (real)    Initial Z position.                            *
*   THETA0    (real)    Initial azimuthal angle.                       *
*   PHI0      (real)    Initial elevation angle.                       *
*   PSI0      (real)    Initial roll angle.                            *
*   TFS       (logical) TFS option: TAPE in TFS format
*   TAPE      (logical) TAPE option: File name.                        *
*----------------------------------------------------------------------*
* Modified: 01-APR-1999, M. Woodley (SLAC)                             *
*   If we're doing tape file output and there are LCAVITY elements in  *
*   the current beamline, initialize ENER1 (in COMMON /OPTIC1/) using   *
*   ENERGY from BEAM common, and call TMLCAV for each one to update    *
*   ENERGY                                                             *
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer ietflg,ipnflg, liftbeam,currbeam
      double precision alfa,amass,arad,beta,betas,bunch,bxmax,bymax,
     +charge,cosmux,cosmuy,currnt,deltas,deltat,dtbyds,dxmax,dymax,
     +et,ex,exn,ey,eyn,freq0,gamma,gammas,gamtr,parnum,pc,pdamp,
     +qs,qx,qy,sigdx,sigdy,sige,sigt,sigx,sigxco,sigy,sigyco,sinmux,
     +sinmuy,u0,xcomax,xix,xiy,ycomax,en0,beambv,elkfact,elmfact
 
*---- Particles, emittances and sigmas.
      integer mfact, mbmult
      parameter (mfact = 50, mbmult = 20)
      common /beanam/   prtnam, bsequnam,
     +                  prtnames(mttact), bseqnames(mttact)
      common /beaflt/   amass, charge, en0, pc, gamma,
     +                  ex, exn, ey, eyn, et, sigt, sige,
     +                  bunch, parnum, currnt
      common /beaaux/   sigx, qx, xix, cosmux, sinmux, bxmax, dxmax,
     +                  xcomax, sigxco, sigdx,
     +                  sigy, qy, xiy, cosmuy, sinmuy, bymax, dymax,
     +                  ycomax, sigyco, sigdy,
     +                  qs, alfa, gamtr, deltas, dtbyds, deltat,
     +                  freq0, beta, u0, arad, beambv, pdamp(3),
     +                  gammas, betas,
     +                  elkfact(mfact), elmfact(0:mbmult)
      common /beaint/   ietflg, ipnflg, liftbeam, currbeam
      save   /beaint/
      common /bealog/   fbch, frad
      save              /beanam/, /beaflt/, /beaaux/, /bealog/
      logical           fbch, frad
      character*(mcnam) prtnam, bsequnam, prtnames, bseqnames
      integer           isp
      logical           domap, anylcav, fmap
      double precision  el, orbt(6), ek(6), re(6,6), te(6,6,6)
      integer i,icode,idisk,ienum,iflag,ilnum,iocc,ipos,isup,jbit,
     +jbyt,k,l_seq,nline,npage,lastnb
      double precision a,arc,arclen,b,circ,cm,data,dist,dphi,dpsi,
     +dtheta,elmlen,phi,phi0,proxim,psi,psi0,rmax,rmin,sig,
     +sums,t,theta,theta0,tm,twopi,utwopi,v,v0,v1,v2,ve,w,w0,w1,w2,
     +we,tx
 
      logical           eflag,  fprt, tape
      character         elmnam*(mcnam), filnam*(mcfil)
      dimension         ve(3), v(3), v0(3), v1(3), v2(3), sig(3), cm(3)
      dimension         we(3,3), w(3,3), w0(3,3), w1(3,3), w2(3,3)
*   VE, WE              Current element.
*   V, W                Global position.
*   V0, W0              Initial global position.
*   V1, W1              One (half) superperiod.
*   V2, W2              Symmetric half of superperiod.
*   SIG                 Signs for reflection.
*   CM                  Centre of machine.
      dimension         data(6)
      equivalence       (v0(1), data(1)), (theta0, data(4))
      equivalence       (phi0,  data(5)), (psi0,   data(6))
*   tm  temporary centre
*   tx  closure error
      dimension         t(3), tm(3), tx(3)
      equivalence       (t(1), theta), (t(2), phi), (t(3), psi)
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi, utwopi = 1.0d0 / twopi)
      logical tfsf
      proxim(a,b) = a + twopi * anint((b - a) * utwopi)
 
*---- Check main beam line, and retrieve its description.
      call lnchck('SURVEY', eflag)
      if (eflag) go to 9999
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Retrieve command attributes.
      do 10 i = 1, 6
        data(i) = 0.
   10 continue
      tfsf = .false.
      call utgflt(lccmd, 1, 6, data)
 
*---- Set up initial V and W.
      suml = 0.
      sums = 0.
      call sumtrx(theta0, phi0, psi0, w0)
      theta = theta0
      phi = phi0
      psi = psi0
      call sucopy(v0, w0, v, w)
 
*---- Initial disk output.
      call utglog(lccmd, 7, 7, tfsf)
      filnam = ' '
      call utgstr(lccmd, 8, 8, filnam)
      tape = .false.
      if (filnam .ne. ' ') then
        call flopen(filnam, 'SWFD', 0, 0, idisk, eflag)
        if (.not. eflag) then
          tape = .true.
          if (tfsf)  then
            call sutfsf(idisk)
          else
            call tphead(idisk, 'SURVEY')
            call tpelem('INITIAL', idisk)
            write (idisk, 810) v, suml, theta, phi, psi
          endif
          lcelm = 0
        endif
      endif
*---- If we're doing tape file output, check for presence of LCAVITY
*     elements in current beamline ... if there are any, initialize
*     ENER1 (in COMMON /OPTIC1/) using the ENERGY value from BEAM common
*     and then call TMLCAV for each one to update ENER1 and ENERGY
      domap = (tape .and. anylcav())
      if (domap) ener1 = en0
*---- Clear counts.
      npage = 0
      nline = maxlin
      l_seq = 0
*==== Print and storage loop.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        if (lcelm .ne. 0) then
          fprt = jbit(iflag,mprnt) .ne. 0  .or.
     +           ipos .eq. irg1  .or.  ipos .eq. irg2
          icode = jbyt(iflag,1,mcode)
          if (icode .eq. 1) then
            call suelem(elmlen, arclen, ve, we)
            suml = suml + elmlen
            sums = sums + arclen
            call sutrak(v, w, ve, we)
            call suangl(w, theta, phi, psi)
            if (tape) then
              if (domap) then
                isp = iq(lcelm+mbsp)
                if (isp .eq. 27) call tmlcav(.false., .false.,
     +            orbt, fmap, el, ek, re, te)
              endif
              if (tfsf)  then
                write
     +          (idisk, '(''  "'',a,''"'',t21,i5,1p,8e18.10)')
     +          elmnam(:lastnb(elmnam)), iocc,
     +                           suml, sums, v, theta, phi, psi
              else
                call tpelem(elmnam, idisk)
                write (idisk, 810) v, suml, theta, phi, psi
              endif
            endif
          endif
*---- Print, if print flag set, or if at limit of range.
          if (fprt) then
            call suhead(1, nline, npage)
            if (icode .eq. 1) then
              write (iqpr2, 910) ienum, elmnam, iocc,
     +                           suml, sums, v, theta, phi, psi
            else if (icode .eq. 2) then
              write (iqpr2, 920) elmnam, iocc,
     +                           suml, sums, v, theta, phi, psi
            else
              write (iqpr2, 930) elmnam, iocc,
     +                           suml, sums, v, theta, phi, psi
            endif
          endif
        endif
   90 continue
 
*==== Trailer record.
*     Refer (half) superperiod to initial local system.
      do 120 i = 1, 3
        cm(i) = v0(i)
        v1(i) = w0(1,i) * (v(1) - v0(1))
     +        + w0(2,i) * (v(2) - v0(2))
     +        + w0(3,i) * (v(3) - v0(3))
        do 110 k = 1, 3
          w1(i,k) = w0(1,i)*w(1,k) + w0(2,i)*w(2,k) + w0(3,i)*w(3,k)
  110   continue
  120 continue
      circ = suml
      arc = sums
      ilnum = 1
 
*---- Make symmetric half superperiod.
      if (symm) then
        sig(1) = 1.
        sig(2) = 1.
        sig(3) = - 1.
        do 140 i = 1, 3
          v2(i) = - sig(i) * (w1(1,i)*v1(1)+w1(2,i)*v1(2)+w1(3,i)*v1(3))
          do 130 k = 1, 3
            w2(i,k) = sig(k) * w1(k,i) * sig(i)
  130     continue
  140   continue
 
*---- Track to end of superperiod.
        call sutrak(v, w, v2, w2)
        call suangl(w, theta, phi, psi)
        circ = suml + suml
        arc = sums + sums
        ilnum = 2
        call suhead(1, nline, npage)
        write (iqpr2, 930) elmnam, ilnum, circ, arc, v, theta, phi, psi
      endif
 
*---- Only 1 superperiod?
      if (nsup .le. 1) then
        do i = 1, 3
          tx(i) = v(i) - v0(i)
          if (l_seq .gt. 0)  tm(i) = tm(i) / l_seq
        enddo
        dtheta = theta - proxim(theta0, theta)
        dphi = phi - proxim(phi0, phi)
        dpsi = psi - proxim(psi0, psi)
        write (iqpr2, 940)
        call suhead(5, nline, npage)
        write (iqpr2, 950) circ, arc
        write (iqpr2, 970) tx, dtheta, dphi, dpsi
        write (iqpr2, 940)
        if (tape)  then
          if (tfsf)  then
            write (idisk, '(''@ CIRCUM'',t21,''%e  '',1p,e18.10)')
     +      circ
          else
            write (idisk, 820) circ
          endif
        endif
*---- Deal with other superperiods.
      else
        do 190 isup = 2, nsup
          cm(1) = cm(1) + v(1)
          cm(2) = cm(2) + v(2)
          cm(3) = cm(3) + v(3)
          call sutrak(v, w, v1, w1)
          call suangl(w, theta, phi, psi)
          circ = circ + suml
          arc = arc + sums
          ilnum = ilnum + 1
          call suhead(1, nline, npage)
          write (iqpr2, 930) elmnam, ilnum, circ, arc, v, theta,phi,psi
 
*---- Symmetric half of superperiod.
          if (symm) then
            call sutrak(v, w, v2, w2)
            call suangl(w, theta, phi, psi)
            circ = circ + suml
            arc = arc + sums
            ilnum = ilnum + 1
            call suhead(1, nline, npage)
            write (iqpr2, 930) elmnam, ilnum, circ, arc,
     +                         v, theta, phi, psi
          endif
  190   continue
 
*---- Centre of machine.
        do i = 1, 3
          cm(i) = cm(i) / float(nsup)
          tx(i) = v(i) - v0(i)
        enddo
        dtheta = theta - proxim(theta0, theta)
        dphi = phi - proxim(phi0, phi)
        dpsi = psi - proxim(psi0, psi)
 
*---- Minimum and maximum radius.
        do 220 i = 1, 3
          v(i) = v0(i)
          do 210 k = 1, 3
            w(i,k) = w0(i,k)
  210     continue
  220   continue
        rmin = sqrt((v(1)-cm(1))**2+(v(2)-cm(2))**2+(v(3)-cm(3))**2)
        rmax = rmin
        do 300 ipos = irg1, irg2
          call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
          if (lcelm .ne. 0) then
            if (jbyt(iflag,1,mcode) .eq. 1) then
              call suelem(elmlen, arclen, ve, we)
              call sutrak(v, w, ve, we)
              dist = sqrt((v(1)-cm(1))**2+(v(2)-cm(2))**2+
     +                    (v(3)-cm(3))**2)
              rmin = min(rmin, dist)
              rmax = max(rmax, dist)
            endif
          endif
  300   continue
        write (iqpr2, 940)
        call suhead(6, nline, npage)
        write (iqpr2, 950) circ, arc
        write (iqpr2, 960) cm, rmin, rmax
        write (iqpr2, 970) tx, dtheta, dphi, dpsi
        write (iqpr2, 940)
        if (tape) then
          if (tfsf)  then
            write (idisk, '(''@ CENTRE'',t21,1p,3(''%e  '',e18.10))')
     +      cm
            write (idisk,
     +      '(''@ RMIN,RMAX,CIRC'',t21,1p,3(''%e  '',e18.10))')
     +      rmin, rmax, circ
          else
            write (idisk, 830) cm, rmin, rmax, circ
          endif
        endif
      endif
*---- Summary record on "tape".
      if (tape) then
        call flclos(idisk, error)
        if (.not. error) then
          call flname(idisk, filnam)
          msg(1) = 'Output written on file: ' // filnam
          call aainfo('SURVEY', 1, msg)
        endif
      endif
  810 format(1p,4e16.9/3e16.9)
  820 format(48x/32x,1p,e16.9)
  830 format(1p,3e16.9/3e16.9)
  910 format(' ',i5,' ',a8,i4,2f13.6,f15.6,2f14.6,f15.6,2f14.6)
  920 format(' begin ',a8,i4,2f13.6,f15.6,2f14.6,f15.6,2f14.6)
  930 format(' end   ',a8,i4,2f13.6,f15.6,2f14.6,f15.6,2f14.6)
  940 format(' ',13('----------'))
  950 format( 1x,'total length =',f18.6,10x,'arc length   =',f18.6)
  960 format( 1x,'x(centre)    =',f18.6,10x,'y(centre)    =',f18.6,
     +       10x,'z(centre)    =',f18.6/
     +        1x,'r(min)       =',f18.6,10x,'r(max)       =',f18.6)
  970 format( 1x,'error(x)     =',e22.6, 6x,'error(y)     =',e22.6,
     +        6x,'error(z)     =',e22.6/
     +        1x,'error(theta) =',e22.6, 6x,'error(phi)   =',e22.6,
     +        6x,'error(psi)   =',e22.6)
 
 9999 end
