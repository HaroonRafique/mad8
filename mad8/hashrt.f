      subroutine hashrt
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set up "short" table of linear lattice functions for HARMON.       *
*   This routine assumes that the temporary links for HARMON are       *
*   already active; and that the "long" table has been set up.         *
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
      double precision ensige,ensigx,ensigy
 
*---- Communication area for HARMON module.
      common /harchr/   lngnam, shtnam
      common /harflt/   ensigx, ensigy, ensige
      save              /harchr/, /harflt/
      character*(mcnam) lngnam, shtnam
      double precision amux1,amux2,amuy1,amuy2,ax1,ax2,ay1,ay2,bx1,bx2,
     +by1,by2,ct1,ct2,delta1,delta2,dpx1,dpx2,dpy1,dpy2,dx1,dx2,dy1,dy2,
     +px1,px2,py1,py2,s1,s2,x1,x2,y1,y2
 
*---- Buffer for "long" HARMON table: Values at both ends of an element.
      common /halbuf/   bx1, ax1, amux1, by1, ay1, amuy1,
     +                  x1, px1, y1, py1, ct1, delta1,
     +                  dx1, dpx1, dy1, dpy1, s1,
     +                  bx2, ax2, amux2, by2, ay2, amuy2,
     +                  x2, px2, y2, py2, ct2, delta2,
     +                  dx2, dpx2, dy2, dpy2, s2
      save   /halbuf/
      integer lhalbf,lhaltb,lhasbf,lhastb,mlcol,mscol
 
*---- Link area for HARMON module.
      parameter         (mlcol = 17, mscol = 12)
      common /halink/   lhaltb, lhalbf, lhastb, lhasbf
      save              /halink/
      double precision amuxb,amuyb,bxb,bxbp,byb,bybp,dxb,dxbp,dxbpp,dyb,
     +dybp,dybpp
 
*---- Buffer for "short" HARMON table: Values averaged over an element.
      common /hasbuf/   bxb, dxb, amuxb, byb, dyb, amuyb,
     +                  bxbp, dxbp, dxbpp, bybp, dybp, dybpp
      save   /hasbuf/
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
      integer ienum,iflag,iocc,ipos,nrow
      double precision el,twopi
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0 * pi)
      character*(mcnam) colnam(mscol), elmnam
      integer           icform(mscol)
 
      data colnam
     +  / 'BXBAR',  'BYBAR',  'DXBAR',  'DYBAR',   'MUXBAR', 'MUYBAR',
     +    'BXBARP', 'BYBARP', 'DXBARP', 'DXBARPP', 'DYBARP', 'DYBARPP' /
      data icform       / mscol * mreal /
 
*---- Set up table buffer for averaged optical functions.
*     "long" table is already open.
      nrow = iq(lq(lcseq-msdir)-1)
      call tbcrea(shtnam, 1, nrow, mscol, colnam, icform, 1, lhastb)
      if (error) go to 9999
 
*---- Loop on beam range.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
 
*---- Fetch lattice parameters for both ends.
        if (iq(lcelm+mbpr) .eq. mpelm) then
          call tbset(lhaltb, ipos - 1, 1, lhalbf)
          call ucopy(q(lhalbf+1), bx1, iq(lhalbf-1))
          call tbset(lhaltb, ipos, 1, lhalbf)
          call ucopy(q(lhalbf+1), bx2, iq(lhalbf-1))
 
*---- Calculate averaged lattice parameters.
          el = s2 - s1
          bxb = (bx1 + bx2) / 2.0 + (ax2 - ax1) * el/6.0
          byb = (by1 + by2) / 2.0 + (ay2 - ay1) * el/6.0
          dxb = (dx1 + dx2) / 2.0 - (dpx2 - dpx1) * el/12.0
          dyb = (dy1 + dy2) / 2.0 - (dpy2 - dpy1) * el/12.0
          amuxb = (amux1 + amux2) / 2.0 - (el / bx2 - el / bx1) / 12.0
          amuyb = (amuy1 + amuy2) / 2.0 - (el / by2 - el / by1) / 12.0
 
*---- Save averaged lattice parameters.
          call tbset(lhastb, ipos, 3, lhasbf)
          if (lhasbf .ne. 0) then
            call ucopy(bxb, q(lhasbf+1), iq(lhasbf-1))
          endif
        endif
   90 continue
 
*---- Tables remain open.
 
 9999 end
