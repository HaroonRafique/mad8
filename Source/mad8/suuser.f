      subroutine suuser(elmlen, arclen, ve, we)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   This routine must be replaced to handle survey calculations for    *
*   user-defined elements (example see routine SUELEM).                *
*   It can safely assume that the survey data are set to identity.     *
* Output:                                                              *
*   ELMLEN    (real)    Nominal element length.                        *
*   ARCLEN    (real)    Element length along design orbit.             *
*   VE(3)     (real)    Displacement of exit w.r.t. entry.             *
*   WE(3,3)   (real)    Rotation of exit w.r.t. entry.                 *
* Reference pointer used:                                              *
*   LCELM               Current element bank.                          *
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
      double precision arclen,elmlen,ve,we
 
      end
