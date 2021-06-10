      subroutine flmain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Switch routine (subprocess code) for I/O services section.         *
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
      integer lflbuf,lfltab
 
*---- Links for closed orbit correction module.
      common /fllink/   lfltab, lflbuf
      save              /fllink/
      integer iflflg,ipr,isp
 
      data iflflg       / 0 /
 
*---- Initialize link area.
      if (iflflg .eq. 0) then
        call mzlink(0, '/FLLINK/', lfltab, lfltab, lflbuf)
        iflflg = 1
      endif
 
*---- User-defined services.
      if (isp .le. 0  .or.  isp .gt. 20) then
        call usercm(ipr, isp)
 
*---- ARCHIVE.
      else if (isp .eq. 1) then
        call flwtfs
 
*---- RETRIEVE.
      else if (isp .eq. 2) then
        call flrtfs
 
*---- ASSIGN.
      else if (isp .eq. 3) then
        call flassi
 
*---- CALL.
      else if (isp .eq. 4) then
        call flcall(1)
 
*---- RETURN.
      else if (isp .eq. 5) then
        call flcall(2)
 
*---- EXCITE.
      else if (isp .eq. 6) then
        call flxcit(1)
 
*---- INCREMENT.
      else if (isp .eq. 7) then
        call flxcit(2)
 
*---- POOLDUMP.
      else if (isp .eq. 8) then
        call fldump
 
*---- POOLLOAD.
      else if (isp .eq. 9) then
        call flload
 
*---- SAVE.
      else if (isp .eq. 10) then
        call svmain
 
*---- STATUS.
      else if (isp .eq. 11) then
        call fltell
 
*---- SYSTEM.
      else if (isp .eq. 12) then
        call flcsys
 
*---- HELP.
      else if (isp .eq. 13) then
        call svhelp
 
*---- SYSTEM.
      else if (isp .eq. 14) then
        call svshow
 
*---- TABLE.
      else if (isp .eq. 15) then
        call tblist
 
*---- SAVELINE.
      else if (isp .eq. 16) then
        call svslin
 
*---- STRUCTURE.
      else if (isp .eq. 17) then
        call flstrc
 
*---- MAKESEQUENCE.
      else if (isp .eq. 18) then
        call flseq
      endif
 
      end
