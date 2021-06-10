      subroutine zend
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Close down all activities (called by MZEND).                       *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer nline
      integer idate(2), itime(2)
 
 
*---- Close plot metafile.
      if (iplflg .ne. 0) call gxterm
 
*---- Enter termination phase.
      call zphase(-1)
 
*---- Print error counts and ending time.
      call datimh(idate, itime)
      call uhtoc(idate, mcwrd, cdate, 8)
      call uhtoc(itime, mcwrd, ctime, 8)
      if (nwarn .ne. 0  .or.  nfail .ne. 0) then
        write (msg, 910) nwarn, nfail
        nline = 3
      else
        nline = 1
      endif
      write (msg(nline), 920) cdate, ctime
      call aainfo('ZEND', nline, msg)
 
*---- Terminate ZEBRA.
      call mzend
 
  910 format(i5,' Warning messages,'/i5,' Error messages.')
  920 format(6x,'MAD terminated on ',a8,' at ',a8)
 
      end
