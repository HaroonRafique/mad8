      subroutine comdis(symm, jpl, a, nm, nc, betm, amum, betc, amuc)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set up influence matrix for dispersion and orbit correction.       *
*   Uses thin lens approximation for all elements.                     *
* Input:                                                               *
*   SYMM      (logical) Symmetry flag.                                 *
*   JPL       (integer) 1: horizontal, 2: vertical.                    *
*   NM        (integer) Number of monitors.                            *
*   NC        (integer) Number of correctors.                          *
*   BETM(*)   (real)    Beta functions at monitors.                    *
*   AMUM(*)   (real)    Monitor phases.                                *
*   BETC(*)   (real)    Beta functions at correctors.                  *
*   AMUC(*)   (real)    Corrector phases.                              *
* Output:                                                              *
*   A(NDIM,*) (real)    Influence matrix.                              *
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
      integer ic,im,isave,isp,jc,jm,jpl,nc,nm
      double precision a,amuc,amue,amum,betc,betm,cosecq,data,facce,
     +faccm,facem,force,half,halfq
      logical           symm
      dimension         a(2*nm,nc), betm(*), amum(*), betc(*), amuc(*)
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
 
      parameter         (half = 0.5d0)
 
      dimension         data(4)
 
      if (jpl .eq. 1) then
        halfq = halfqx
      else
        halfq = halfqy
      endif
      cosecq = half / sin(halfq)
 
*---- Fill block for influence on orbit, clear block for dispersion.
      do 10 jc = 1, nc
      do 10 jm = 1, nm
        faccm = cos(abs(amum(jm)-amuc(jc))-halfq)
        if (symm) then
          faccm = faccm + cos(amum(jm)+amuc(jc)-halfq)
        endif
        a(jm,jc) = faccm * sqrt(betm(jm)*betc(jc)) * cosecq
        a(nm+jm,jc) = 0.0
   10 continue
 
*---- Allocate working space.
      isave = iwork
      ic    = isave
      im    = ic + nc
      iwork = im + nm
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Loop over elements affecting dispersion.
      lccom = lq(lcseq-mselm)
   20 if (lccom .ne. 0) then
        call ucopy(q(lccom+1), xcm, iq(lccom-1))
 
*---- Extract element data.
        lcelm = lq(lccom-2)
        isp   = iq(lcelm+mbsp)
        akl   = 0.0
        data(1) = 0.0
        data(2) = 0.0
        data(3) = 0.0
        data(4) = 0.0
 
*---- Quadrupole or thin multipole.
        if (isp .eq. 5) then
          call utgflt(lcelm, 2, 4, data)
          if (data(3) .eq. 0.0) akl = data(1) * data(2)
 
*---- Sextupole.
        else if (isp .eq. 6) then
          call utgflt(lcelm, 2, 4, data)
          if (data(3) .eq. 0.0) akl = - dxcm * data(1) * data(2)
 
*---- Multipole.
        else if (isp .eq. 8) then
          call utgflt(lcelm, 4, 7, data)
          if (data(2) .eq. 0.0) akl = data(1)
          if (data(4) .eq. 0.0) akl = akl - dxcm * data(3)
        endif
        if (jpl .eq. 1) then
          force = + betxcm * akl
          amue  =   amuxcm
        else
          force = - betycm * akl
          amue  =   amuycm
        endif
 
*---- Influence of correctors on elements.
*     omitting the factor sqrt(beta(elm)).
        do 30 jc = 1, nc
          facce = cos(abs(amuc(jc)-amue)-halfq)
          if (symm) then
            facce = facce + cos(amuc(jc)+amue-halfq)
          endif
          dq(ic+jc) = facce * cosecq * force
   30   continue
 
*---- Influence of elements on monitors,
*     omitting the factor sqrt(beta(elm)).
        do 40 jm = 1, nm
          facem = cos(abs(amue-amum(jm))-halfq)
          if (symm) then
            facem = facem + cos(amue+amum(jm)-halfq)
          endif
          dq(im+jm) = facem * cosecq
   40   continue
 
*---- Add to block for influence on dispersion.
        do 60 jc = 1, nc
          do 50 jm = 1, nm
            a(nm+jm,jc) = a(nm+jm,jc) + dq(im+jm) * dq(ic+jc)
   50     continue
   60   continue
 
*---- Next element.
        lccom = lq(lccom-1)
        go to 20
      endif
 
*---- Multiply by scale factors.
      do 70 jc = 1, nc
      do 70 jm = 1, nm
        a(nm+jm,jc) = weight(jpl) * (a(nm+jm,jc) *
     +                sqrt(betm(jm) * betc(jc)) - a(jm,jc))
   70 continue
 
*---- Release working store.
      iwork = isave
 
      end
