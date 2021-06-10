      subroutine svmain
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save selected part(s) of structure; SAVE command.                  *
* Attribute:                                                           *
*   FILENAME  (string)  Name of file to be written.                    *
*   PATTERN   (char)    Pattern to be matched (default all).           *
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
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
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer idir,ipr,isp,jdir
 
      logical           dosave, eflag
      character*(mcfil) filnam*(mcfil), label*(mcnam), patt*(mcstr)
 
*---- File name.
      filnam = 'save'
      call utgstr(lccmd, 1, 1, filnam)
 
*---- Fetch patterns.
      patt = ' '
      call utgstr(lccmd, 2, 2, patt)
      if (patt .eq. ' ') then
        lref1 = 0
      else
        call utpatt(patt, lref1)
      endif
 
*---- Open SAVE file.
      call flopen(filnam, 'SWFD', 0, 0, isave, eflag)
      if (eflag) go to 9999
 
*---- Title.
      call flname(isave, filnam)
      if (ctitle .ne. ' ') write (isave, 910) ctitle
      write (isave, 920) cdate, ctime, filnam
 
*==== Save all definitions which are not part of the dictionary.
      dosave = .true.
 
*---- Pass 1: Save element definitions.
      do 10 idir = iq(ldbnk(3)+3) + 1, iq(ldbnk(3)+1)
        lccls = lq(ldbnk(3)-idir)
        if (lccls .ne. 0) then
          ipr   = iq(lccls+mbpr)
          jdir  = iq(lccls+mbnam)
          if (ipr .eq. mpelm  .and.  jdir .eq. idir  .and.
     +        lccls .ne. lq(lq(lccls+1)-3)) then
            call diname(ldbnk, idir, label)
            if (lref1 .ne. 0) call utmtpt(lref1, label, dosave)
            if (dosave) call svbank(lccls)
          endif
        endif
   10 continue
 
*---- Pass 2: Save beam lines and sequences.
      do 20 idir = iq(ldbnk(3)+3) + 1, iq(ldbnk(3)+1)
        lccls = lq(ldbnk(3)-idir)
        if (lccls .ne. 0) then
          ipr = iq(lccls+mbpr)
          isp = iq(lccls+mbsp)
          if (ipr .eq. mplin) then
            call diname(ldbnk, idir, label)
            if (lref1 .ne. 0) call utmtpt(lref1, label, dosave)
            if (dosave) call svline(lccls)
          endif
        endif
   20 continue
 
*---- Pass 3: Save parameter definitions.
      do 30 idir = iq(ldbnk(3)+3) + 1, iq(ldbnk(3)+1)
        lccls = lq(ldbnk(3)-idir)
        if (lccls .ne. 0) then
          ipr = iq(lccls+mbpr)
          if (ipr .eq. mppar) then
            call diname(ldbnk, idir, label)
            if (lref1 .ne. 0) call utmtpt(lref1, label, dosave)
            if (dosave) call svparm(lccls)
          endif
        endif
   30 continue
 
*---- End of save.
      write (isave, 930)
      call flclos(isave, eflag)
      if (.not. eflag) then
        msg(1) = 'Definitions saved on file: ' // filnam
        call aainfo('SVMAIN', 1, msg)
      endif
 
*---- Drop the pattern.
      if (lref1 .ne. 0) then
        call mzdrop(0, lref1, 'L')
      endif
 
  910 format('TITLE &'/'"',a77,'"'/'!')
  920 format('! DATE AND TIME: ',t21,a,2x,a/'!'/'! FILE:',t21,a/'!')
  930 format('RETURN !')
 
 9999 end
