      subroutine flrtfs
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read an internal MAD table on a TFS file. RETRIEVE command.        *
* Attributes:                                                          *
*   TABLE     (name)    Name to be given to the table.                 *
*   FILENAME  (string)  Name of TFS file to be read.                   *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer ileng,iunit
 
      character         tabnam*(mcnam), strnam*(mcfil), filnam*(mcfil)
 
*---- Get attributes.
      tabnam = 'TWISS'
      call utgnam(lccmd, 1, 1, tabnam)
      strnam = tabnam
      call utgstr(lccmd, 2, 2, strnam)
 
*---- Open disk file.
      call flopen(strnam, 'SRFD', 0, 0, iunit, error)
      if (.not. error) then
        call flname(iunit, filnam)
 
*---- Read table on file.
        call tbrtfs(tabnam, iunit)
        if (.not. error) then
          call utleng(tabnam, ileng)
          msg(1) = 'Table "' // tabnam(1:ileng)
     +    // '" read on file: ' // filnam
          call aainfo('FLWTFS', 1, msg)
        endif
 
*---- Close disk file.
        call flclos(iunit, error)
      endif
 
      end
