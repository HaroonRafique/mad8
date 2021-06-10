      subroutine exstrg(name, ltab, lbnk, ilink, rval, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode expression stored in a string.                              *
* Input:                                                               *
*   NAME      (name)    Table column or string name.                   *
*   LTAB(1)   (pointer) Pointer to table header bank.                  *
*   LBNK(1)   (pointer) Supporting bank for expression.                *
*   ILINK     (integer) Supporting link for expression (see MZBOOK).   *
* Output:                                                              *
*   RVAL      (real)    Value returned for constant expression.        *
*   EFLAG     (logical) .TRUE. if error (NAME not found)               *
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
      integer ibias,icat1,icat2,ichar,idata,idir,iform,ikat,ileng,ilink,
     +iln,ipr,iseen,isp,ival,j,nkat
      double precision rval
      character*(mcnam) name
      integer           ltab(1), lbnk(1)
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
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
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
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
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*(mcstr) string(3)
      character*(mcnam) sval
      logical           eflag
 
*---- First test: table column name.
      eflag = .true.
      string(1) = name
      string(2) = ' '
      string(3) = name
      call utleng(name, ileng)
      if (ileng .gt. 0) then
        call tbcol(ltab, name(1:ileng), iform, ibias)
        if (iform .ge. 2  .and.  iform .le. 4) then
          eflag = .false.
 
*---- Second test: descriptor name.
        else
          call tbqdsc(ltab, name(1:ileng), iform)
          if (iform .ge. 2  .and.  iform .le. 4) then
            eflag = .false.
 
*---- Third test: expression stored in global string.
          else
            call difind(ldbnk, name(1:ileng), idir, lcsrc)
            if (lcsrc .ne. 0  .and.
     +        iq(lcsrc+mbpr) .eq. mpstr  .and.
     +        iq(lcsrc+mbsp) .eq. 1) then
              call utgstr(lcsrc, 1, 3, string)
              ileng = iq(lcsrc+mbat+mcval)
              eflag = .false.
 
*---- Fourth test: expression restricted to specific table.
            else
              call tbgdsc(ltab, 'TYPE', iform, ival, rval, sval)
              if (iform .eq. 5) then
                call difind(ldbnk, 'T_' // sval, idir, lcsrc)
                if (lcsrc .ne. 0  .and.
     +            iq(lcsrc+mbpr) .eq. mpstr  .and.
     +            iq(lcsrc+mbsp) .ne. 1) then
                  call kwget(lq(lcsrc+1), iln, ipr, isp, nkat)
                  call utlook(name(1:ileng), katnam, nkat, ikat)
                  if (ikat .ne. 0) then
                    icat1 = 3 * ikat - 2
                    icat2 = 3 * ikat
                    call utgstr(lcsrc, icat1, icat2, string)
                    idata = mbat + 3 * mcsiz * (ikat - 1) + mcval
                    ileng = iq(lcsrc+idata)
                    eflag = .false.
                  endif
                endif
              endif
            endif
          endif
        endif
      endif
 
*---- Test for valid use.
      if (eflag) then
        call aawarn('EXSTRG', 1, 'Name "' // name(1:ileng) //
     +              '" is not table column or string.')
 
*---- Remove blanks and pack to statement buffer.
      else
        ntok = 0
        do 10 j = 1, ileng
          if (string(1)(j:j) .ne. ' ') then
            ntok = ntok + 1
            token(ntok) = ch2upp(ichar(string(1)(j:j)))
          endif
   10   continue
 
*---- Append semicolon.
        if (ntok .gt. 0) then
          jtok = 1
          ntok = ntok + 1
          token(ntok) = ';'
 
*---- Decode expression and store in /EXPRESS/.
          call exread(2, rval, iseen)
 
*---- Store constant value or build expression bank.
          if (iseen .eq. 2) then
            call exmak1(ltab, lbnk, ilink)
          endif
        endif
      endif
 
      end
