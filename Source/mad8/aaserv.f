      subroutine aaserv(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Switch routine (subprocess code) for services.                     *
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
      integer ipr,isp
 
*---- ISP = 1, PACKMEM. Wipe out division 1, compact division 2.
      if (isp .eq. 1) then
        call mzgarb(2, 1)
 
*---- ISP = 2, OPTION.
      else if (isp .eq. 2) then
        call aaopts
 
*---- ISP = 3, STOP. This command is handled in AAMAIN.
      else if (isp .eq. 3) then
        continue
 
*---- ISP = 4, SET.
      else if (isp .eq. 4) then
        call aaset
 
*---- ISP = 5, VALUE.
      else if (isp .eq. 5) then
        call aavalu
 
*---- ISP = 6, END_FILE.
      else if (isp .eq. 6) then
 
*---- ISP = 7, PUSH.
      else if (isp .eq. 7) then
        call aapush
 
*---- ISP = 8, ENDPUSH.
      else if (isp .eq. 8) then
        call aaepush
 
*---- ISP = 9, reserved.
      else if (isp .eq. 9) then
 
*---- ISP = 10, reserved.
      else if (isp .eq. 10) then
 
*---- User-defined services.
      else
        call usercm(ipr, isp)
      endif
 
      end
