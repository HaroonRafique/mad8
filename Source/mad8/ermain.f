      subroutine ermain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Switch routine (subprocess code) for errors section.               *
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
      integer ipr,isp
 
*---- User-defined services.
      if (isp .le. 0  .or.  isp .gt. 10) then
        call usercm(ipr, isp)
 
*---- ISP = 1, EALIGN.
      else if (isp .eq. 1) then
        call eralig
 
*---- ISP = 2, EFIELD.
      else if (isp .eq. 2) then
        call erfiel
 
*---- ISP = 3, EOPT.
      else if (isp .eq. 3) then
        call eropt
 
*---- ISP = 4, EPRINT.
      else if (isp .eq. 4) then
        call erprnt
 
*---- ISP = 5, ESAVE.
      else if (isp .eq. 5) then
        call ersave
 
*---- ISP = 6, EFCOMP.
      else if (isp .eq. 6) then
        call erfcom
      endif
 
      end
