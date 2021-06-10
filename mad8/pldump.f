      subroutine pldump
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Dump the plot bank structure (partially)                           *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer ivcurv,ivvar,j,l,nipri
      character         sanno*(mxlabl)
      character         sname*(mcnam), stitl*(mtitl), symb*1
      nipri = ivpar(8)
      write (iqlog, 10000)
      l = lpmain
      if (l .eq. 0)  then
        write (iqlog, 10010)
      else
        write (iqlog, 10020) iq(l-1)
        write (iqlog, 10030) iq(l + mpfont), q(l + mpfelm)
        write (iqlog, 10040) q(l + mpxsiz), q(l + mpysiz), q
     +  (l + mpascl), q(l + mplscw), q(l + mplscl), q(l + mpsscl), q
     +  (l + mptscl)
        lm1 = lq(l - mpfram - 1)
        lm2 = lq(l - mpfram - 2)
        if (lm1 .eq. 0)  then
          write (iqlog, 10050)
        else
          write (iqlog, 10060) iq(lm1 - 1), iq(lm2 - 1)
        endif
        do 30 iframe = 1, mpfram
          lframe = lq(lpmain - iframe)
          if (lframe .ne. 0) then
            write (iqlog, 10070) iframe
            l = lq(lframe - 2)
            if (l .ne. 0) then
              write (iqlog, 10090) iq(l - 1)
              write (iqlog, 10080) nipri,
     +        (q(l + j), j = 1, min (nipri, iq(l - 1)))
              write (iqlog, * ) ' '
            endif
            l = lq(lframe - 3)
            if (l .ne. 0) then
              write (iqlog, 10100) iq(l - 1)
            endif
            l = lframe
            write (iqlog, 10110) q(l + mpmin), q(l + mpmax),
     +      iq(l + mpsclf)
            call uhtoc (q(l + mpname), mcnam, sname, mcnam)
            write (iqlog, 10120) sname
            call uhtoc (q(l + mpttit), mtitl, stitl, mtitl)
            write (iqlog, 10130) stitl
            call uhtoc (q(l + mpbtit), mtitl, stitl, mtitl)
            write (iqlog, 10140) stitl
            lvvar = lq(lframe - 1)
            if (lvvar .eq. 0) then
              write (iqlog, 10150)
              goto 999
            endif
            ivvar = 0
   10       continue
            ivvar = ivvar + 1
            write (iqlog, 10160) ivvar
            l = lvvar
            write (iqlog, 10170) q(l + mpmin), q(l + mpmax), iq
     +      (l + mpsclf)
            call uhtoc (q(l + mpname), mxlabl, sanno, mxlabl)
            write (iqlog, 10180) sanno
            lvcurv = lq(lvvar - 1)
            if (lvcurv .eq. 0) then
              write (iqlog, 10190)
              goto 999
            endif
            ivcurv = 0
   20       continue
            ivcurv = ivcurv + 1
            write (iqlog, 10200) ivcurv
            l = lvcurv
            write (iqlog, 10210) iq(l + mpstyl), iq(l + mpcolr),
     +      iq(l + mpspli), iq(l + mpbars), iq(l + mpsymf)
            if (iq(l + mpsymf) .eq. 200) then
              call uhtoc (q(l + mpsymb), 1, symb, 1)
              write (iqlog, 10220) symb
            endif
            call uhtoc (q(l + mpanno), mcnam, sname, mcnam)
            write (iqlog, 10230) sname
            lvval = lq(lvcurv - 1)
            if (lvval .eq. 0) then
              write (iqlog, 10240)
              goto 999
            endif
            l = lvval
            write (iqlog, 10250) iq(l - 1)
            write (iqlog, 10080) nipri,
     +      (q(l + j), j = 1, min (nipri, iq(l - 1)))
            write (iqlog, * ) ' '
            lhval = lq(lvcurv - 3)
            if (lhval .eq. 0) then
              write (iqlog, 10260)
              goto 999
            endif
            l = lhval
            write (iqlog, 10270) iq(l - 1)
            write (iqlog, 10080) nipri,
     +      (q(l + j), j = 1, min (nipri, iq(l - 1)))
            write (iqlog, * ) ' '
            lindx = lq(lvcurv - 4)
            if (lindx .ne. 0) then
              l = lindx
              write (iqlog, 10280) iq(l - 1)
              write (iqlog, 10290) nipri,
     +        (iq(l + j), j = 1, min (nipri, iq(l - 1)))
              write (iqlog, * ) ' '
            endif
            lvcurv = lq(lvcurv)
            if (lvcurv .ne. 0) goto 20
            lvvar = lq(lvvar)
            if (lvvar .ne. 0) goto 10
          endif
   30   continue
      endif
10000 format(//' ',15('++++') // t20,'dump LPMAIN plot bank' //' ',15(
     +'++++')//)
10010 format(' WARNING: LPMAIN pointer = 0, no bank.'/)
10020 format( ' LPmain number of data words   =',i8/)
10030 format( ' font                          =',i8/
     +' start of machine plot s value =',g14.6/)
10040 format( ' x size (default = 0.)         =',g14.6/
     +' y size (ditto)                =',g14.6/
     +' annotation size               =',g14.6/
     +' line width scale factor       =',g14.6/
     +' label scale factor            =',g14.6/
     +' symbol scale factor           =',g14.6/
     +' text scale factor             =',g14.6/)
10050 format(' No machine plot banks given.'/)
10060 format( ' lengths of machine plot banks =',2i8/)
10070 format(' ',15('****') // t20,'start frame no. ',i1 //)
10080 format( ' first up to ',i3,' values       ='/(1x,5g14.6))
10090 format( ' length of hor. bank           =',i8/)
10100 format( ' length of sequence bank       =',i8/)
10110 format( ' hor. minimum                  =',g14.6/
     +' hor. maximum                  =',g14.6/
     +' scaling request (0,1,2)       =',i8/)
10120 format( ' hor. var. name                =',a/)
10130 format( ' top title                     =',a/)
10140 format( ' bottom title                  =',a/)
10150 format(' WARNING: no vert. axis banks, quit.'/)
10160 format(' ',15('+-+-') // t20,'start variable no. ',i2 //)
10170 format( ' vert. minimum                 =',g14.6/
     +' vert. maximum                 =',g14.6/
     +' scaling request (0,1,2)       =',i8/)
10180 format( ' vert. var. name               =',a/)
10190 format(' WARNING: no vert. var. banks, quit.'/)
10200 format(' ',15('----') // t20,'start curve no. ',i2 //)
10210 format( ' line style                    =',i8/
     +' colour                        =',i8/
     +' spline flag                   =',i8/
     +' bars flag                     =',i8/
     +' symbol value                  =',i8/)
10220 format( ' plot symbol                   =',a/)
10230 format( ' curve annotation              =',a/)
10240 format(' WARNING: no vert. value bank, quit.'/)
10250 format(' no. of vertical values =',i8/)
10260 format(' WARNING: no hor. value bank, quit.'/)
10270 format(' no. of hor. values     =',i8/)
10280 format( ' no. of index values           =',i8/)
10290 format( ' first up to ',i3,' values       ='/(1x,10i8))
  999 end
