      subroutine haweig
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set the weights for minimization. HWEIGHT command.                 *
* Weights for penalty functions:                                       *
*   QX'',  QY''              Second momentum derivatives of tunes.     *
*   QX''', QY'''             Third momentum derivatives of tunes.      *
*   DQXDEX, DQYDEY, DQYDEX   Anharmonicities.                          *
*   DX'I,  DX''I             Derivatives of dispersion (int. point).   *
*   BX'I,  BY'I              Derivatives of beta's     (int. point).   *
*   RXI,   RYI               Resonances                (int. point).   *
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
      integer mhfun
      double precision hdes,hfac,hfun,hwei
 
*---- Data for minimization in HARMON.
      parameter         (mhfun = 21)
      common /hafbad/   hdes(mhfun), hfun(mhfun), hwei(mhfun),
     +                  hfac(mhfun)
      save              /hafbad/
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
      integer i
 
      hwei(1) = 1.0
      hwei(2) = 1.0
      do 10 i = 3, mhfun
        hwei(i) = 0.0
   10 continue
      call utgflt(lccmd, 1, mhfun, hwei)
 
      end
