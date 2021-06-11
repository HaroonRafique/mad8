      subroutine enarb(el, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save TRANSPORT map for a MAKELUMP.                                 *
* Input:                                                               *
*   EK(6)     (real)    Kick due to element.                           *
*   RE(6,6)   (real)    Transfer matrix.                               *
*   TE(6,6,6) (real)    Second-order terms.                            *
* Important common data:                                               *
*   LCELM     /REFER/   Current element bank.                          *
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
      integer i1,i2
      double precision ek,el,re,te
      dimension         ek(6), re(6,6), te(6,6,6)
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
 
*---- Length of element.
      i2 = 2
      call utpflt(lcelm, i2, i2, el)
 
*---- Element kick.
      i1 = i2 + 1
      i2 = i2 + 6
      call utpflt(lcelm, i1, i2, ek)
 
*---- Transfer matrix.
      i1 = i2 + 1
      i2 = i2 + 36
      call utpflt(lcelm, i1, i2, re)
 
*---- Second order terms.
      i1 = i2 + 1
      i2 = i2 + 216
      call utpflt(lcelm, i1, i2, te)
 
      end
