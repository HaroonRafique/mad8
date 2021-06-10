      subroutine didefi(ldir, label, lbank)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Add LABEL to directory structure LDIR and store bank pointer.      *
*   The full length of LABEL is considered for searches.               *
*   Call relevant routines to drop invalidated data.                   *
* Input:                                                               *
*   LDIR(4)  (pointer)  Directory links.                               *
*   LABEL    (char)     Label to be found.                             *
*   LBANK(1) (pointer)  Pointer to bank to be defined.                 *
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
      integer idir,ileng,iln,index,ipr,jbyt,jpr
      integer           ldir(4), lbank(*)
      character*(mcnam) label
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ldinew,ldiold
 
*---- Local links for decoder.
      common /dilink/   ldinew, ldiold
      save              /dilink/
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
 
      character*(mcnam) oldkey, newkey
 
*---- Search directory.
      ldinew = lbank(1)
      call dilook(ldir, label, index, idir)
 
*---- If valid entry found or created, test for possible redefinition.
      if (idir .ne. 0) then
        ldiold = lq(ldir(3)-idir)
        if (ldiold .ne. 0) then
          ipr = iq(ldiold+mbpr)
          iln = iq(ldiold+mbln)
          jpr = iq(ldinew+mbpr)
 
*---- Keyword commands must not be redefined.
          call utleng(label, ileng)
          if (ipr .eq. mpkey) then
            write (msg, 910) label(1:ileng), iln
  910       format('Trying to redefine master keyword "',a,'",'/
     +             'old version (defined in line ',i6,') kept.')
            call aafail('DIDEFI', 2, msg)
            call aadrop(ldinew)
            go to 9999
 
*==== Replacement of a protected name is not allowed.
          else if (jbyt(iq(ldiold),mxcls,2) .ne. 0  .and.
     +             iq(ldiold+mbnam) .eq. idir) then
            write (msg, 920) label(1:ileng), iq(ldiold+mbln)
  920       format('Trying to redefine protected name "',a,'",'/
     +             'old version (defined in line ',i6,') kept.')
            call aafail('DIDEFI', 2, msg)
            call aadrop(ldinew)
            go to 9999
 
*---- Replacing a parameter definition.
          else if (ipr .eq. mppar  .and.  jpr .eq. mppar) then
            if (iln .ne. 0) then
              write (msg, 930) 'parameter', label(1:ileng), iln
  930         format('Redefining ',a,' value "',a,'",'/
     +               'previous definition occurred in line',i6,'.')
              call aawarn('DIDEFI', 2, msg)
            endif
 
*---- Replacing a string definition.
          else if (ipr .eq. mpstr  .and.  jpr .eq. mpstr) then
            if (iln .ne. 0) then
              write (msg, 930) 'string', label(1:ileng), iln
              call aawarn('DIDEFI', 2, msg)
            endif
 
*---- Replacing beam line/element. If structure changes, drop expansion.
*     Messages are generated in AASMOD.
          else if ((ipr .eq. mpelm  .or.  ipr .eq. mplin)  .and.
     +             (jpr .eq. mpelm  .or.  jpr .eq. mplin)) then
            call aasmod(idir, ldiold, ldinew)
            if (error) go to 9999
 
*---- Replacing command or subroutine.
          else if (ipr .ge. mpsub  .and.  jpr .ge. mpsub) then
            if (label .ne. ' ') then
              write (msg, 940) label(1:ileng), iln
  940         format('Redefining subroutine or command "',a,'",'/
     +               'previous definition occurred in line',i6,'.')
              call aawarn('DIDEFI', 2, msg)
            endif
 
*---- Replacement changes category of item.
          else
            call diname(ldkey, iq(lq(ldiold+1)+mbnam), oldkey)
            call diname(ldkey, iq(lq(ldinew+1)+mbnam), newkey)
            write (msg, 950) label(1:ileng), oldkey, newkey, iln
  950       format('Trying to change category of label "',a,'";'/
     +             'old definition was: ',a/
     +             'new definition is:  ',a/
     +             'old version (defined in line',i6,') kept.')
            call aafail('DIDEFI', 4, msg)
            go to 9999
          endif
 
*---- Drop old data bank, unless it represents an element class.
          if (ldiold .ne. ldinew) then
            if (jbyt(iq(ldiold),mxcls,2) .eq. 0) call aadrop(ldiold)
          endif
        endif
 
*---- If not yet defined, add new entry.
      else
        call diadd(ldir, label, index, idir)
      endif
 
*---- Link bank to directory entry.
      lq(ldir(3)-idir) = ldinew
      iq(ldinew+mbnam) = idir
 
 9999 end
