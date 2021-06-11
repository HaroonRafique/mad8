      subroutine mtgwgt
      implicit none
*----------------------------------------------------------------------*
* Attributes:                                                          *
*   q1       (const.)   weight on tune of mode 1                       *
*   q2       (const.)   weight on tune of mode 2                       *
*   q1'      (const.)   weight on chromaticity of mode 1               *
*   q2'      (const.)   weight on chromaticity of mode 2               *
*   q1''     (const.)   weight on nonlinear chromaticity of mode 1     *
*   q2''     (const.)   weight on nonlinear chromaticity of mode 2     *
*   dq1de1   (const.)   weight on anharmonicity of mode 1              *
*   dq1de2   (const.)   weight on cross anharmonicity                  *
*   dq2de2   (const.)   weight on anharmonicity of mode 2              *
*   dtune    (const.)   weight on fast detuning.                       *
*   tunx     (const.)   weight on fast tune of mode x.                 *
*   tuny     (const.)   weight on fast tune of mode y.                 *
*   dynapfrac(const.)   weight on fractional dynamic aperture.         *
*   smear    (const.)   weight on smear.                               *
*   turns    (const.)   weight on number of survival turns.            *
*   lyapunov (const.)   weight on Lyapunov exponent                    *
*   xend     (const.)   weight on xend                                 *
*   pxend    (const.)   weight on pxend                                *
*   yend     (const.)   weight on yend                                 *
*   pyend    (const.)   weight on pyend                                *
*   tend     (const.)   weight on tend                                 *
*   ptend    (const.)   weight on ptend                                *
*   wxmin    (const.)   weight on wxmin                                *
*   wxmax    (const.)   weight on wxmax                                *
*   wymin    (const.)   weight on wymin                                *
*   wymax    (const.)   weight on wymax                                *
*   wxymin   (const.)   weight on wxymin                               *
*   wxymax   (const.)   weight on wxymax                               *
*   formula  (const.)   weight on user-defined formula                 *
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
      integer iformula,igflag,nuglob,nuloc
      double precision gpesi,gtarget
 
*---- Communication area for GLOBAL constraints.
      common /mtfrgo/   gtarget(29,2), gpesi(29)
      common /mtirgo/   nuglob, nuloc, igflag(32,2), iformula
      common /mtlrgo/   dynapflag, fixpointfl, statflag
      logical           dynapflag, fixpointfl, statflag
 
*---- Copy data to local storage.
      call utgflt(lccmd, 1, 29, gpesi)
 
      end
