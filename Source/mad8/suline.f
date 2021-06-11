      subroutine suline(ipos2, elmlen, arclen, vl, wl)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute Displacement and rotation for one element.                 *
* Input:                                                               *
*   IPOS2     (integer) Position of END in main beam line.             *
* Output:                                                              *
*   ELMLEN    (real)    Sum of nominal element lengths.                *
*   ARCLEN    (real)    Line length along design orbit.                *
*   VL(3)     (real)    Displacement of exit w.r.t. entry.             *
*   WL(3,3)   (real)    Rotation of exit w.r.t. entry.                 *
* Reference pointer used:                                              *
*   LCELM     /REFER/   Current line bank, or current element.         *
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
      integer idir,ipos,ipos2,jdir,k
      double precision arc,arclen,el,elmlen,ve,vl,vt1,vt2,vt3,we,wl,
     +wt1k,wt2k,wt3k
      dimension         vl(3), wl(3,3)
 
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
 
      dimension         ve(3), we(3,3)
 
*---- Initialize.
      elmlen = 0.0
      arclen = 0.0
      call suiden(vl, wl)
      idir = iq(lq(lcseq-msdir)+ipos2)
 
*---- Loop backwards over the line until its beginning is found.
      do 80 ipos = ipos2 - 1, 1, - 1
        jdir = iq(lq(lcseq-msdir)+ipos)
        if (jdir .eq. idir) go to 90
        lcelm = lq(ldbnk(3)-jdir)
        if (iq(lcelm+mbpr) .eq. mpelm) then
          call suelem(el, arc, ve, we)
          vt1 = ve(1) + we(1,1)*vl(1) + we(1,2)*vl(2) + we(1,3)*vl(3)
          vt2 = ve(2) + we(2,1)*vl(1) + we(2,2)*vl(2) + we(2,3)*vl(3)
          vt3 = ve(3) + we(3,1)*vl(1) + we(3,2)*vl(2) + we(3,3)*vl(3)
          vl(1) = vt1
          vl(2) = vt2
          vl(3) = vt3
          do 10 k = 1, 3
            wt1k = we(1,1)*wl(1,k) + we(1,2)*wl(2,k) + we(1,3)*wl(3,k)
            wt2k = we(2,1)*wl(1,k) + we(2,2)*wl(2,k) + we(2,3)*wl(3,k)
            wt3k = we(3,1)*wl(1,k) + we(3,2)*wl(2,k) + we(3,3)*wl(3,k)
            wl(1,k) = wt1k
            wl(2,k) = wt2k
            wl(3,k) = wt3k
   10     continue
          elmlen = elmlen + el
          arclen = arclen + arc
        endif
   80 continue
 
*---- Restore element pointer.
   90 continue
      lcelm = lq(ldbnk(3)-idir)
 
      end
