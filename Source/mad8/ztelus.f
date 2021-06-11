      subroutine ztelus
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Handle errors considered as non-serious by ZEBRA.                  *
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
      integer id,mode
 
      common /ztellc/ id, mode
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      if (id .eq. 99) then
        call aafail('ZTELUS', 1, 'Dynamic store full --- terminated.')
      else
        call aafail('ZTELUS', 1, 'ZEBRA call failed --- terminated.')
      endif
      mode = 3
 
      end
