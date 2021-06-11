      subroutine copmon(list, title, rms)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print table of all monitors in the machine.                        *
* Input:                                                               *
*   LIST      (integer) Level of output desired.                       *
*   TITLE     (char)    Table type header.                             *
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
      integer idir,iflag,iocc,ipos,istat,jbyt,lenhed,list,maxh,maxv,
     +minh,minv,nline,npage,number,numh,numv
      double precision hmax,hmin,hrms,hsum,rms,utwopi,vmax,vmin,
     +vrms,vsum
      character*(*)     title
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      parameter         (lenhed = 6)
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0 / (2.0 * pi))
 
      character*(mcnam) monnam, linnam, hmaxnm, hminnm, vmaxnm, vminnm
      character*4       effect
      logical           hor,    print,  ver
 
      if (lq(lcseq-msmon) .eq. 0) go to 9999
      hmin = + 1.e10
      hmax = - 1.e10
      hsum = 0.0
      numh = 0
      minh = 0
      maxh = 0
      hmaxnm = ' '
      hminnm = ' '
      vmin = + 1.e10
      vmax = - 1.e10
      vsum = 0.0
      numv = 0
      minv = 0
      maxv = 0
      vmaxnm = ' '
      vminnm = ' '
 
*---- Print page header.
      npage = 1
      call prpage(iqpr2)
      call uhtoc(q(lcseq+msbn), mcnam, linnam, mcnam)
      write (iqpr2, 910) title, linnam, npage
      nline = 2
 
*---- If listing, print sub-header.
      print = list .ge. 3
      if (list .gt. 1) then
        call prline(iqpr2)
        write (iqpr2, 920)
        call prline(iqpr2)
        nline = lenhed
      endif
 
*---- Loop through monitor table.
      lccom = lq(lcseq-msmon)
   10 if (lccom .ne. 0) then
        call ucopy(q(lccom+1), xcm, iq(lccom-1))
 
*---- Fetch element information.
        ipos   = iq(lccom-5)
        idir   = iq(lq(lcseq-msdir)+ipos)
        iflag  = iq(lq(lcseq-msflg)+ipos)
        number = iq(lq(lcseq-msnum)+ipos)
        iocc   = jbyt(iflag,mocc1,mocc2)
        call diname(ldbnk, idir, monnam)
 
*---- Monitor status.
        hor = .false.
        ver = .false.
        istat = jbyt(iq(lccom),3,2)
        effect = 'off'
        if (istat .eq. 1) then
          hor = .true.
          effect = 'x'
        else if (istat .eq. 2) then
          ver = .true.
          effect = 'y'
        else if (istat .eq. 3) then
          hor = .true.
          ver = .true.
          effect = 'x/y'
        endif
 
*---- Extremal and r.m.s. values.
        if (hor) then
          numh = numh + 1
          if (xcm .lt. hmin) then
            hmin = xcm
            minh = iocc
            hminnm = monnam
          endif
          if (xcm .gt. hmax) then
            hmax = xcm
            maxh = iocc
            hmaxnm = monnam
          endif
          hsum = hsum + xcm**2
        endif
        if (ver) then
          numv = numv + 1
          if (ycm .lt. vmin) then
            vmin = ycm
            minv = iocc
            vminnm = monnam
          endif
          if (ycm .gt. vmax) then
            vmax = ycm
            maxv = iocc
            vmaxnm = monnam
          endif
          vsum = vsum + ycm**2
        endif
 
*---- Partial print only?
        if (list .eq. 2) print = hor .or. ver
 
*---- Print.
        if (print) then
          if (nline .ge. maxlin) then
            npage = npage + 1
            call prpage(iqpr2)
            write (iqpr2, 910) title, linnam, npage
            call prline(iqpr2)
            write (iqpr2, 920)
            call prline(iqpr2)
            nline = lenhed
          endif
          nline = nline + 1
          write (iqpr2, 930) number, monnam, iocc, scm,
     +                       xcm*1000.0, ycm*1000.0, betxcm, betycm,
     +                       amuxcm*utwopi, amuycm*utwopi, effect
        endif
        lccom = lq(lccom-1)
        go to 10
      endif
 
*---- Summary lines.
      hmin = hmin * 1000.0
      hmax = hmax * 1000.0
      hrms = sqrt(hsum / float(max(1, numh))) * 1000.0
      vmin = vmin * 1000.0
      vmax = vmax * 1000.0
      vrms = sqrt(vsum / float(max(1, numv))) * 1000.0
      rms = max(hrms, vrms)
 
*---- Print summary.
      call prline(iqpr2)
      write (iqpr2, 940) numh, nmon(1), numv, nmon(2),
     +                   hmin, hminnm, minh, vmin, vminnm, minv,
     +                   hmax, hmaxnm, maxh, vmax, vmaxnm, maxv,
     +                   hrms, vrms
      call prline(iqpr2)
 
  910 format(' Monitor readings ',a,' for beam line ',a,
     +       t122,'page',i6)
  920 format(' Number  name   occur.      position      xread',9x,
     +       'yread',9x,'betx',10x,'bety',10x,'mux',11x,'muy',10x,
     +       'effect'/
     +       28x,'[m]',11x,2('[mm]',10x),2('[m]',11x),2('[2*pi]',8x))
  930 format(' ',i6,2x,a8,i5,7f14.6,5x,a4)
  940 format(t34,'horizontal',t84,'vertical'/
     +       ' Monitors used:',
     +       t25,i8,' (of ',i8,')',t75,i8,' (of ',i8,')'/
     +       ' Minimum readings:',
     +       t25,f15.6,' mm at ',a,' [',i5,']',
     +       t75,f15.6,' mm at ',a,' [',i5,']'/
     +       ' Maximum readings:',
     +       t25,f15.6,' mm at ',a,' [',i5,']',
     +       t75,f15.6,' mm at ',a,' [',i5,']'/
     +       ' R.m.s. readings:',t25,f15.6,' mm',t75,f15.6,' mm')
 
 9999 end
