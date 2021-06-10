      subroutine erlist(ipos, iopt, idum, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print errors for one single element.                               *
* Input:                                                               *
*   LCSEQ     /REFER/   Current beam line sequence.                    *
*   IPOS      (integer) Current position in sequence.                  *
*   IOPT      (integer) Option flag: 1=Initialize, 2=Print, 3=End.     *
*   IDUM      (integer) Unused.                                        *
*   EFLAG     (logical) Error flag.                                    *
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
      integer idum,ienum,iflag,iocc,iopt,ipos,j,jbyt,kline,lvect,mhead,
     +nd,nline,npage
      double precision vect,zero
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
      integer maxmul
 
*---- Maximum order of multipoles.
      parameter         (maxmul = 20)
      parameter         (mhead = 5)
 
      character         elmnam*(mcnam), title*(*)
      dimension         vect(2*maxmul+8)
 
      save              npage, nline
 
      parameter         (zero = 0.0d0)
      parameter         (title = 'Imperfections.')
 
*---- Initialize printing.
      eflag = .false.
      if (iopt .eq. 1) then
        call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
        npage = 0
        nline = maxlin
 
*---- Print errors for one element.
      else if (iopt .eq. 2) then
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        if (lcali .ne. 0  .or.  lcfld .ne. 0) then
 
*---- Extract misalignment errors.
          if (lcali .eq. 0) then
            call uzero(vect, 1, 6*mwflt)
          else
            call ucopy(q(lcali+1), vect, 6*mwflt)
          endif
          lvect = 6
 
*---- Extract field errors.
          if (lcfld .ne. 0) then
            nd = iq(lcfld-1)
            call ucopy(q(lcfld+1), vect(7), nd)
            lvect = lvect + nd / mwflt
          endif
 
*---- If NLINE too big, start a new page.
          kline = (lvect + 9) / 10
          if (nline + kline .ge. maxlin) then
            npage = npage + 1
            call prhead('EPRINT', title, zero, -1, nline, npage)
            nline = nline + mhead
            write (iqpr2, 920)
            call prline(iqpr2)
          endif
          nline = nline + kline
 
*---- Print error vector.
          if (jbyt(iq(lsflg+ipos),1,mcode) .eq. 1) then
            write (iqpr2, 930) ienum, elmnam, iocc,
     +        (1000.0 * vect(j), j = 1, lvect)
          else if (jbyt(iq(lsflg+ipos),1,mcode) .eq. 2) then
            write (iqpr2, 940) elmnam, iocc,
     +        (1000.0 * vect(j), j = 1, lvect)
          else if (jbyt(iq(lsflg+ipos),1,mcode) .eq. 3) then
            write (iqpr2, 950) elmnam, iocc,
     +        (1000.0 * vect(j), j = 1, lvect)
          endif
        endif
      else
        nline = nline + 1
        call prline(iqpr2)
      endif
 
  910 format(' Imperfections.',t31,a,'line: ',a,t51,'Range: ',a,
     +       t122,'page',i6)
  920 format(' Element sequence',t23,'Displacements in [mm]',
     +       t65,'Rotations in [mrad]',t107,'Field errors in 1.0E-3'/
     +       ' pos.  element occ.',t23,'DX',t37,'DY',t51,'DS',
     +       t65,'DPHI',t79,'DTHETA',t93,'DPSI',t107,'Re(K0L)',
     +       t121,'Im(K0L)'/
     +       ' no.   name    no.',t23,'Re(K1L)',t37,'Im(K1L)',
     +       t51, 'Re(K2L)',t65, 'Im(K2L)',t79,'Re(K3L)',t93,'Im(K3L)',
     +       t107,'Re(K4L)',t121,'Im(K4L)'/t23,'Re(K5L)',t37,'Im(K5L)',
     +       t51, 'Re(K6L)',t65, 'Im(K6L)',t79,'Re(K7L)',t93,'Im(K7L)',
     +       t107,'etc.')
  930 format(' ',i5,1x,a8,i4,8g14.5:/(t20,8g14.5:))
  940 format(' begin',1x,a8,i4,8g14.5:/(t20,8g14.5:))
  950 format(' end  ',1x,a8,i4,8g14.5:/(t20,8g14.5:))
 
      end
