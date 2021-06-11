      subroutine lalump(nord, lseq, fp, fm)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find closed orbit and build Lie-algebraic map along the orbit.     *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   LSEQ(*)   (pointer) Expansion of the relevant beam line sequence.  *
* Output:                                                              *
*   FP, FM    (map)     Resulting map.                                 *
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
      integer icode,ienum,iflag,ihm,ihp,iocc,ipos,isave,itm,itp,jbit,
     +jbyt,nord
      double precision displ,el,fm,fp
      integer           lseq(*)
      dimension         fp(*), fm(6,6)
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
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
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
 
      dimension         displ(6)
      character*(mcnam) elmnam
      logical           fprt
 
*---- Allocate working space.
      isave = iwork
      itm   = iwork
      itp   = itm + 36
      ihm   = itp + itop6(nord)
      ihp   = ihm + 36
      iwork = ihp + itop6(nord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Initialize.
      call ucopy(orbit0, orbit, 6*mwflt)
      call lmone(nord, fp, fm)
      lsali = lq(lseq(1)-msali)
      lsfld = lq(lseq(1)-msfld)
      lscom = lq(lseq(1)-mscom)
      lcali = 0
      lcfld = 0
      lccom = 0
 
*---- Loop on range.
      do 90 ipos = iq(lseq(1)+msr1), iq(lseq(1)+msr2)
        call utelem(lseq, ipos, iflag, elmnam, iocc, ienum)
        icode = jbyt(iflag, 1, mcode)
        if (lsali .ne. 0) lcali = lq(lsali-ipos)
        if (lsfld .ne. 0) lcfld = lq(lsfld-ipos)
        if (lscom .ne. 0) lccom = lq(lscom-ipos)
        fprt = jbit(iflag,mlump) .ne. 0
        if (fprt) write (iqlog, 910) elmnam
 
*---- Misalignment at entrance.
        if (icode .ne. 3  .and.  lcali .ne. 0) then
          call ucopy(q(lcali+1), displ, 6 * mwflt)
          call lmdsp1(ipos, nord, displ, dq(itp+1), dq(itm+1))
          call lamove(nord, dq(itp+1), dq(itm+1), orbit,
     +                dq(ihp+1), dq(ihm+1))
          if (fprt) then
            write (iqlog, 920) 'misalignment at entrance'
            call lmprnt(iqlog, nord, dq(ihp+1), dq(ihm+1))
          endif
          call lmcat(nord, fp, fm, dq(ihp+1), dq(ihm+1), fp, fm)
        endif
 
*---- Compute element map.
        if (icode .eq. 1) then
          call lmmap(nord, el, dq(itp+1), dq(itm+1))
          call lamove(nord, dq(itp+1), dq(itm+1), orbit,
     +                dq(ihp+1), dq(ihm+1))
          if (fprt) then
            write (iqlog, 920) 'element'
            call lmprnt(iqlog, nord, dq(ihp+1), dq(ihm+1))
          endif
          call lmcat(nord, fp, fm, dq(ihp+1), dq(ihm+1), fp, fm)
        endif
 
*---- Misalignment at exit.
        if (icode .ne. 2  .and.  lcali .ne. 0) then
          call ucopy(q(lcali+1), displ, 6 * mwflt)
          call lmdsp2(ipos, nord, displ, dq(itp+1), dq(itm+1))
          call lamove(nord, dq(itp+1), dq(itm+1), orbit,
     +                dq(ihp+1), dq(ihm+1))
          if (fprt) then
            write (iqlog, 920) 'misalignment at exit'
            call lmprnt(iqlog, nord, dq(ihp+1), dq(ihm+1))
          endif
          call lmcat(nord, fp, fm, dq(ihp+1), dq(ihm+1), fp, fm)
        endif
   90 continue
 
*---- Drop working storage.
      iwork = isave
 
  910 format(' '/' LALUMP.  Entering object: ',a)
  920 format(' Transfer map for ',a,':')
 
      end
