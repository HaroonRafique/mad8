      logical function bmusrg(iel, ilo, iup, iflag)
      implicit none
************************************************************************
*
*     Returns .TRUE. if element is selected for printing.
*
*--- input
*  IEL       BMPM element number
*  ILO       BMPM lower range for print
*  ILU       BMPM upper range for print
*  IFLAG     flag from UTELEM with element print flags
*
************************************************************************
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer iel,iflag,ilo,iup,jbit
      bmusrg = iel .ge. ilo .and. iel .le. iup
     +         .or. jbit(iflag, mprnt) .ne. 0
      end
