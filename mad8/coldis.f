      subroutine coldis(nkick, jpl, ok)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Perform least squares correction on closed orbit for one plane.    *
* Input:                                                               *
*   NKICK     (integer) Number of kickers to be used at most (0 = all).*
*   JPL       (integer) Number of plane; 1: x, 2: y.                   *
* Output:                                                              *
*   OK        (logical) Success flag.                                  *
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
      integer ia,ib,ibc,ibm,imc,imm,ip,ir,isave,iter,iw1,iw2,ix,jm,jpl,
     +nc,ncd,nkick,nm,nmd
      double precision drms,rms,rrms
      logical           ok
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      logical           symm
 
*---- If no correctors or no monitors for this plane, skip.
      ncd = ncor(jpl)
      nmd = nmon(jpl)
      ok = .false.
      if (ncd * nmd .eq. 0) then
        ok = .true.
 
*---- Allocate working space.
      else
        isave = iwork
        ia    = isave + 1
        ib    = ia  + 2 * nmd * ncd
        ibm   = ib  + 2 * nmd
        imm   = ibm + 2 * nmd
        ix    = imm + nmd
        ibc   = ix  + ncd
        imc   = ibc + ncd
        ip    = imc + ncd
        iwork = ip  + ncd - 1
        ir    = ibm
        iw1   = ibc
        iw2   = imc
        if (iwork .gt. nwork) then
          call mzwork(0, dq(1), dq(iwork+1), 2)
          nwork = iwork
        endif
 
*---- Get monitor readings.
        symm = iq(lcseq+msym) .ne. 0
        call cogdis(jpl, dq(ib), dq(ib+nmd), dq(ibm), dq(imm), nm)
 
*---- Errors before correction.
        drms = 0.0
        rrms = 0.0
        do 10 jm = 1, nm
          drms = drms + dq(ib+nmd+jm-1)**2
          rrms = rrms + dq(ib+jm-1)**2
          dq(ib+nm+jm-1) = dq(ib+nmd+jm-1) * weight(jpl)
   10   continue
        rms  = sqrt((rrms+drms*weight(jpl)**2)/float(max(nm,1)))
        drms = sqrt(drms/float(max(nm,1)))
        rrms = sqrt(rrms/float(max(nm,1)))
        write (iqlog, 910) drms, rrms*1000.0
 
*---- Check need for correction.
        if (rms .le. qual) then
          write (iqlog, 920)
          ok = .true.
 
*---- Get corrector settings.
        else
          call cogkik(jpl, dq(ix), dq(ibc), dq(imc), nc)
 
*---- Set up influence matrix.
          call comdis(symm, jpl, dq(ia), nm, nc, dq(ibm),
     +                dq(imm), dq(ibc), dq(imc))
 
*---- Solve for this plane.
          iter = nkick
          call htlsq(dq(ia), dq(ib), 2*nm, nc, qual, iter,
     +               dq(ix), dq(ip), dq(ir), dq(iw1), dq(iw2))
 
*---- Modify correctors.
          call coskik(jpl, dq(ix))
 
*---- Expected remaining r.m.s. error.
          drms = 0.0
          rrms = 0.0
          do 30 jm = 1, nm
            drms = drms + dq(ir+nm+jm-1)**2
            rrms = rrms + dq(ir+jm-1)**2
   30     continue
          drms = sqrt(drms/float(max(nm,1))) / weight(jpl)
          rrms = sqrt(rrms/float(max(nm,1)))
          write (iqlog, 930) drms, rrms * 1000.0
        endif
 
*---- Release working space.
        iwork = isave
      endif
 
  910 format(t45,'dispersion',t68,'orbit'/
     +       t11,'R.m.s. errors before MICADO:',t41,f12.6,' m',
     +       t61,f12.6,' mm')
  920 format(t11,'No correction made.'/' ')
  930 format(t11,'Expected r.m.s. errors after:',t41,f12.6,' m',
     +       t61,f12.6,' mm'/' ')
 
      end
