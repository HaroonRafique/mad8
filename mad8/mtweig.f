      subroutine mtweig
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   WEIGHT command.                                                    *
* Attributes:                                                          *
*   BETX     (real)    Horizontal beta.                                *
*   BETY     (real)    Vertical beta.                                  *
*   ALFX     (real)    Horizontal alpha.                               *
*   ALFY     (real)    Vertical alpha.                                 *
*   X        (real)    Horizontal position for closed orbit.           *
*   PX       (real)    Horizontal momentum for closed orbit.           *
*   Y        (real)    Vertical position for closed orbit.             *
*   PY       (real)    Vertical momentum for closed orbit.             *
*   T        (real)    Longitudinal position for closed orbit.         *
*   PT       (real)    Momentum error for closed orbit.                *
*   DX       (real)    Horizontal dispersion.                          *
*   DY       (real)    Vertical dispersion.                            *
*   DPX      (real)    Horizontal dispersion slope.                    *
*   DPY      (real)    Vertical dispersion slope.                      *
*   MUX      (real)    Horizontal phase.                               *
*   MUY      (real)    Vertical phase.                                 *
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
      integer maxlin,maxval,mconf1,mconf2,mconmn,mconmx,mcontp,mconvl,
     +mconwt
 
*---- Parameters for matching module.
      parameter         (maxlin = 16, maxval = 36)
      parameter         (mconf1 = 1, mcontp = 2, mconf2 = maxval + 2)
      parameter         (mconmn = mconf2 + 1)
      parameter         (mconmx = mconmn + maxval * mwflt)
      parameter         (mconvl = mconmx + maxval * mwflt)
      parameter         (mconwt = mconvl + maxval * mwflt)
      integer energy_val, chrom_val
      parameter         (energy_val = 27, chrom_val = 26)
      double precision wgt
 
*---- Information for matching module.
      common /mtcwgt/   wgt(maxval)
      save              /mtcwgt/
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
 
*---- Store given weights.
      call utgflt(lccmd, 1, maxval, wgt)
 
      end
