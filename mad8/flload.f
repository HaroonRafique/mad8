      subroutine flload
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read memory dump file (as written by FLDUMP).                      *
*   POOLLOAD command.                                                  *
* Attribute:                                                           *
*   FILENAME  (string)  File name for load.                            *
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
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
      double precision coest,cotol
 
*---- Estimate for closed orbit search.
      common /coesti/ coest(6), cotol
      save            /coesti/
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer iquest
 
*---- ZEBRA system block: Returns system information.
      common /quest/    iquest(100)
      save              /quest/
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
      integer iunit,mfintg,mfname,mfreal
 
      parameter         (mfname = 1,
     +                   mfreal = mfname + mwnam + 1,
     +                   mfintg = mfreal + 16 * mwflt + 1)
      character*(mcfil) filnam, strnam
      character * 8      tmpc
 
*---- Name of POOLDUMP file to be read.
      strnam = 'pooldump'
      call utgstr(lccmd, 1, 1, strnam)
 
*---- Get an index for pooldump file.
      call flopen(strnam, 'SRUD', 0, 0, iunit, error)
      if (.not. error) then
        call flname(iunit, filnam)
        call mzwipe(2)
        call fzfile(iunit, 0, 'IOR')
        call fzin(iunit, 2, lroot, 1, '.', 0, 0)
        if (iquest(1) .ne. 0) then
          msg(1) = 'Unable to read stream: ' // strnam
          msg(2) = 'File name: ' // filnam
          call aafail('FLLOAD', 2, msg)
        else
          call fzendi(iunit, 'T')
          call flclos(iunit, error)
 
*--- perform floating format check, version number check,
*    reload closed orbit estimate, and closed orbit fit tolerance
        if (q(lroot+2) .ne. 100.)  then
          call aafail('FLLOAD', 1,
     +    'pooldump file has wrong floating point format')
        else
          call uhtoc(q(lroot+3), 4, tmpc, 8)
          if (tmpc(:4) .ne. nvers(:4)) then
            msg(1) = 'pooldump from version             ' // tmpc
            msg(2) = 'incompatible with current version ' // nvers
            call aafail('FLLOAD', 2, msg)
          else if (tmpc(5:) .ne. nvers(5:)) then
            msg(1) = 'pooldump from version             ' // tmpc
            msg(2) = 'being read with current version   ' // nvers
            call aawarn('FLLOAD', 2, msg)
          endif
        endif
        call ucopy(q(lroot+5), coest, 7 * mwflt)
 
*---- Re-initialize table manager.
          call tbinit(0)
 
*---- Copy some structural links to reference area for efficiency.
          ldkey(1) = lq(lroot-mdkey)
          ldkey(2) = lq(lroot-mdkey-1)
          ldkey(3) = lq(lroot-mdkey-2)
          ldkey(4) = lq(lroot-mdkey-3)
          ldbnk(1) = lq(lroot-mdbnk)
          ldbnk(2) = lq(lroot-mdbnk-1)
          ldbnk(3) = lq(lroot-mdbnk-2)
          ldbnk(4) = lq(lroot-mdbnk-3)
          lcseq = lq(lq(lroot-mcseq)-1)
 
*---- Success.
          if (.not. error) then
            msg(1) = 'Memory dump read on stream: ' // strnam
            msg(2) = 'File name: ' // filnam
            call aainfo('FLLOAD', 2, msg)
          endif
        endif
      endif
 
      end
