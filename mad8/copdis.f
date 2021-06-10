      subroutine copdis(list, title, rrms, drms)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print table of dispersion and orbit at all monitors.               *
* Input:                                                               *
*   LIST      (integer) Level of output desired.                       *
*   TITLE     (char)    Table type header.                             *
* Output:                                                              *
*   RRMS:     (real)    R.m.s. orbit error.                            *
*   DRMS:     (real)    R.m.s. dispersion error.                       *
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
      integer idir,iflag,ihdcnt,ihdmax,ihdmin,ihrcnt,ihrmax,ihrmin,iocc,
     +ipos,istat,ivdcnt,ivdmax,ivdmin,ivrcnt,ivrmax,ivrmin,jbyt,lenhed,
     +list,nline,npage,number
      double precision drms,hdmax,hdmin,hdsum,hrmax,hrmin,hrsum,
     +rrms,utwopi,vdmax,vdmin,vdsum,vrmax,vrmin,vrsum
      character*(*)     title
      dimension         rrms(2), drms(2)
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
 
      character*(mcnam) monnam, linnam
      character*(mcnam) ahdmax, ahdmin, ahrmax, ahrmin
      character*(mcnam) avdmax, avdmin, avrmax, avrmin
      character*4       orbeff, diseff
      logical           dishor, disver, orbhor, orbver, print
 
      if (lq(lcseq-msmon) .eq. 0) go to 9999
      ihdcnt = 0
      ihrcnt = 0
      ivdcnt = 0
      ivrcnt = 0
 
      hdmin = + 1.e10
      hdmax = - 1.e10
      hdsum = 0.0
      ihdmin = 0
      ihdmax = 0
      ahdmax = ' '
      ahdmin = ' '
 
      vdmin = + 1.e10
      vdmax = - 1.e10
      vdsum = 0.0
      ivdmin = 0
      ivdmax = 0
      avdmax = ' '
      avdmin = ' '
 
      hrmin = + 1.e10
      hrmax = - 1.e10
      hrsum = 0.0
      ihrmin = 0
      ihrmax = 0
      ahrmax = ' '
      ahrmin = ' '
 
      vrmin = + 1.e10
      vrmax = - 1.e10
      vrsum = 0.0
      ivrmin = 0
      ivrmax = 0
      avrmax = ' '
      avrmin = ' '
 
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
        orbhor = .false.
        orbver = .false.
        orbeff = 'off'
        dishor = .false.
        disver = .false.
        diseff = 'off'
        istat = jbyt(iq(lccom),3,2)
        if (istat .eq. 1) then
          orbhor = .true.
          orbeff = 'x'
        else if (istat .eq. 2) then
          orbver = .true.
          orbeff = 'y'
        else if (istat .eq. 3) then
          orbhor = .true.
          orbver = .true.
          orbeff = 'x/y'
        endif
        istat = jbyt(iq(lccom),5,2)
        if (istat .eq. 1) then
          dishor = .true.
          diseff = 'x'
        else if (istat .eq. 2) then
          disver = .true.
          diseff = 'y'
        else if (istat .eq. 3) then
          dishor = .true.
          disver = .true.
          diseff = 'x/y'
        endif
 
*---- Extremal and r.m.s. values.
        if (dishor) then
          ihdcnt = ihdcnt + 1
          if (dxcm .lt. hdmin) then
            hdmin = dxcm
            ihdmin = iocc
            ahdmin = monnam
          endif
          if (dxcm .gt. hdmax) then
            hdmax = dxcm
            ihdmax = iocc
            ahdmax = monnam
          endif
          hdsum = hdsum + dxcm**2
        endif
        if (orbhor) then
          ihrcnt = ihrcnt + 1
          if (xcm .lt. hrmin) then
            hrmin = xcm
            ihrmin = iocc
            ahrmin = monnam
          endif
          if (xcm .gt. hrmax) then
            hrmax = xcm
            ihrmax = iocc
            ahrmax = monnam
          endif
          hrsum = hrsum + xcm**2
        endif
        if (disver) then
          ivdcnt = ivdcnt + 1
          if (dycm .lt. vdmin) then
            vdmin = dycm
            ivdmin = iocc
            avdmin = monnam
          endif
          if (dycm .gt. vdmax) then
            vdmax = dycm
            ivdmax = iocc
            avdmax = monnam
          endif
          vdsum = vdsum + dycm**2
        endif
        if (orbver) then
          ivrcnt = ivrcnt + 1
          if (ycm .lt. vrmin) then
            vrmin = ycm
            ivrmin = iocc
            avrmin = monnam
          endif
          if (ycm .gt. vrmax) then
            vrmax = ycm
            ivrmax = iocc
            avrmax = monnam
          endif
          vrsum = vrsum + ycm**2
        endif
 
*---- Partial print only?
        if (list .eq. 2) then
          print = dishor .or. disver .or. orbhor .or. orbver
        endif
 
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
     +      xcm*1000.0, dxcm, betxcm, amuxcm*utwopi, orbeff,
     +      ycm*1000.0, dycm, betycm, amuycm*utwopi, diseff
        endif
        lccom = lq(lccom-1)
        go to 10
      endif
 
*---- Summary lines.
      drms(1) = sqrt(hdsum / float(max(1, ihdcnt)))
      drms(2) = sqrt(vdsum / float(max(1, ivdcnt)))
      hrmin = hrmin * 1000.0
      hrmax = hrmax * 1000.0
      rrms(1) = sqrt(hrsum / float(max(1, ihrcnt))) * 1000.0
      vrmin = vrmin * 1000.0
      vrmax = vrmax * 1000.0
      rrms(2) = sqrt(vrsum / float(max(1, ivrcnt))) * 1000.0
 
*---- Print summary.
      call prline(iqpr2)
      write (iqpr2, 940) ihrcnt, ivrcnt, ihdcnt, ivdcnt, nmon,
     +                   hdmin, ahdmin, ihdmin, vdmin, avdmin, ivdmin,
     +                   hdmax, ahdmax, ihdmax, vdmax, avdmax, ivdmax,
     +                   drms,
     +                   hrmin, ahrmin, ihrmin, vrmin, avrmin, ivrmin,
     +                   hrmax, ahrmax, ihrmax, vrmax, avrmax, ivrmax,
     +                   rrms
      call prline(iqpr2)
 
  910 format(' Orbit and dispersion at monitors ',a,' for beam line ',
     +       a,t122,'page',i6)
  920 format(' Number  name   occur.      position',
     +       6x,'xread',9x,'Dx',12x,'betx',10x,'mux',11x,'effect'/
     +       42x,'yread',9x,'Dy',12x,'bety',10x,'muy'/
     +       28x,'[m]',11x,'[mm]',10x,2('[m]',11x),'[2*pi]')
  930 format(' ',i6,2x,a8,i5,5f14.6,6x,'orbit:      ',a4/
     +       36x,4f14.6,6x,'dispersion: ',a4)
  940 format(t34,'horizontal',t84,'vertical'/
     +       ' Orbit monitors used:',t25,i8,t75,i8/
     +       ' Dispersion monitors used:',t25,i8,t75,i8/
     +       ' Total monitors:',t25,i8,t75,i8/
     +       ' Minimum dispersion:',
     +       t25,f15.6,' m  at ',a,' [',i5,']',
     +       t75,f15.6,' m  at ',a,' [',i5,']'/
     +       ' Maximum dispersion:',
     +       t25,f15.6,' m  at ',a,' [',i5,']',
     +       t75,f15.6,' m  at ',a,' [',i5,']'/
     +       ' R.m.s. dispersion:',t25,f15.6,' m',t75,f15.6,' m'/
     +       ' Minimum readings:',
     +       t25,f15.6,' mm at ',a,' [',i5,']',
     +       t75,f15.6,' mm at ',a,' [',i5,']'/
     +       ' Maximum readings:',
     +       t25,f15.6,' mm at ',a,' [',i5,']',
     +       t75,f15.6,' mm at ',a,' [',i5,']'/
     +       ' R.m.s. readings:',t25,f15.6,' mm',t75,f15.6,' mm')
 
 9999 end
