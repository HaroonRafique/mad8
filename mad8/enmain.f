      subroutine enmain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Switch routine (subprocess code) for environment section.          *
* Input:                                                               *
*   IPR       (integer) Process code.                                  *
*   ISP       (integer) Subprocess code.                               *
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
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
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
      integer ipr,isp
 
*---- User-defined services.
      if (isp .le. 0  .or.  isp .gt. 20) then
        call usercm(ipr, isp)
 
*---- BEAM.
      else if (isp .eq. 1) then
        call enbeam
 
*---- BETA0: No action required (data already stored).
      else if (isp .eq. 2) then
        continue
 
*---- PRINT.
      else if (isp .eq. 3) then
        call enprnt
 
*---- SAVEBETA.
      else if (isp .eq. 4) then
        call ensbet
 
*---- TITLE.
      else if (isp .eq. 5) then
        call utgstr(lccmd, 1, 1, ctitle)
*---- USE.
      else if (isp .eq. 6) then
        call enuse
 
*---- SELECT.
      else if (isp .eq. 7) then
        call endump
 
*---- SPLIT.
      else if (isp .eq. 8) then
        call ensplt
 
*---- SAVESIGMA.
      else if (isp .eq. 9) then
        call enssig
 
*---- SIGMA0: No action required (data already stored).
      else if (isp .eq. 10) then
        continue
 
*---- MAKLEUMP.
      else if (isp .eq. 11) then
        call enlump
      endif
 
      end
