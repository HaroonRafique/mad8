      subroutine habegn
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize HARMON mode; HARMON command.                            *
* Attributes:                                                          *
*   FX        (real)    Number of horizontal sigmas.                   *
*   FY        (real)    Number of vertical sigmas.                     *
*   FE        (real)    Number of longitudinal sigmas.                 *
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
      double precision ensige,ensigx,ensigy
 
*---- Communication area for HARMON module.
      common /harchr/   lngnam, shtnam
      common /harflt/   ensigx, ensigy, ensige
      save              /harchr/, /harflt/
      character*(mcnam) lngnam, shtnam
      integer mhfun
      double precision hdes,hfac,hfun,hwei
 
*---- Data for minimization in HARMON.
      parameter         (mhfun = 21)
      common /hafbad/   hdes(mhfun), hfun(mhfun), hwei(mhfun),
     +                  hfac(mhfun)
      save              /hafbad/
 
*---- Flags for matching.
      common /mtcflg/   flbeta, florb, flrmat, fltmat, flchrm
      save              /mtcflg/
      logical           flbeta, florb, flrmat, fltmat, flchrm
      character *(mcnam)  sequd, betnm
      common / dmatchc / sequd(2), betnm(2)
      integer mtdbfl, imsequ
      common / dmatchi / mtdbfl, imsequ
      logical bdtflg
      common / dmatchl / bdtflg(2)
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
      integer i
 
*---- Is main beam line set?
      call lnchck('HARMON', error)
      if (error) go to 9999
 
*---- Initialize beam data.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Fetch emittance factors.
      ensigx = 0.0
      ensigy = 0.0
      ensige = 0.0
      call utgflt(lccmd, 1, 3, ensigx)
      if (ensigx .eq. 0.) ensigx = 1.0
      if (ensigy .eq. 0.) ensigy = 1.0
      if (ensige .eq. 0.) ensige = 1.0
 
*---- Set up internal lattice function tables.
      lngnam = 'HARM-LNG'
      call halong
      if (error) go to 9999
      shtnam = 'HARM-SHT'
      call hashrt
 
*---- Initialize matching module.
      mtdbfl = 0
      call mtinit
 
*---- Initialize weights and desired values for penalty functions.
      do 90 i = 1, mhfun
        hwei(i) = 0.0
        hdes(i) = 0.0
   90 continue
      hwei(1) = 1.0
      hwei(2) = 1.0
 
 9999 end
