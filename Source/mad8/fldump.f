      subroutine fldump
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Write memory dump file (to be reloaded with FLLOAD).               *
*   POOLDUMP command.                                                  *
* Attribute:                                                           *
*   FILENAME  (string)  File name for dump.                            *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer iunit,mfintg,mfname,mfreal,msize
      parameter         (mfname = 1,
     +                   mfreal = mfname + mwnam + 1,
     +                   mfintg = mfreal + 16 * mwflt + 1,
     +                   msize = mfintg + 3 + 1)
      character*(mcfil) filnam, strnam
 
*---- Name of POOLDUMP file to be written.
      strnam = 'pooldump'
      call utgstr(lccmd, 1, 1, strnam)
 
*---- Get an index for pooldump file.
      call flopen(strnam, 'SWUD', 0, 0, iunit, error)
      if (.not. error) then
*--- add floating format check, version number,
*    closed orbit estimate, and closed orbit fit tolerance
        q(lroot+2) = 100.
        call uctoh(nvers, iq(lroot+3), mcwrd, 8)
        call ucopy(coest, q(lroot+5), 7 * mwflt)
*---- Write pooldump file.
        call flname(iunit, filnam)
        call fzfile(iunit, 0, 'IOR')
        call fzout(iunit, 2, lroot, 1, '.', 2, 0, 0)
        if (iquest(1) .ne. 0) then
          msg(1) = 'Unable to write stream: ' // strnam
          msg(2) = 'File name: ' // filnam
          call aafail('FLDUMP', 2, msg)
        else
          call fzendo(iunit, 'T')
          call flclos(iunit, error)
 
*---- Success.
          if (.not. error) then
            msg(1) = 'Memory dump written on stream: ' // strnam
            msg(2) = 'File name: ' // filnam
            call aainfo('FLDUMP', 2, msg)
          endif
        endif
      endif
 
      end
