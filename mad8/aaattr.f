      subroutine aaattr(ldef, lbank, nnkat, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode attributes for a command or an element definition.          *
* Input:                                                               *
*   LDEF(1)  (pointer)  Pointer to data bank containing defaults.      *
*   LBANK(1) (pointer)  Pointer to data bank to be filled.             *
*   NKAT     (integer)  Number of attributes for keyword.              *
* Output:                                                              *
*   EFLAG    (logical)  Error flag.                                    *
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
      integer ic,icase,ichar,ichr,idata,ifrst,ikat,ilast,ind1,ind2,ind3,
     +index,itype,ival,j,jcomma,jfrst,jind,jkat,jlast,jtmp,jtype,leng,
     +nkat,nnkat
      integer           ldef(*), lbank(*)
      logical           eflag
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
      integer laabnk,laacur,laadef,laakey,laanew,laaold,laaprc,laasrc,
     +laastk,laatar
 
*---- Local links for control module.
      common /aalink/   laabnk, laadef, laasrc, laatar, laakey, laacur,
     +                  laaprc, laastk, laanew, laaold
      save              /aalink/
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*(mcnam) atrnam
      logical           sflag,  sign
 
*---- Initialize position counters for first attribute.
*     Assuming that at least one attribute is modified.
      laadef = ldef(1)
      laabnk = lbank(1)
      eflag = .false.
      ikat = 0
      nkat = abs(nnkat)
      ifrst = 1
      ilast = 0
      call aamark('AAATTR', laabnk)
 
*---- While current token is comma ...
  100 if (token(jtok) .eq. ',') then
        jcomma = jtok
        jtok = jtok + 1
 
*---- Setup for next attribute. Will be used, if no attribute name seen.
        if (ifrst .gt. ilast) then
          ikat = ikat + 1
          if (ikat .le. nkat) then
            itype = iatype(ikat)
            ilast = ilast + iadim1(ikat) * iadim2(ikat) * iadim3(ikat)
          endif
        endif
 
*---- Empty field: Update attribute pointer and go to next field.
        if (token(jtok) .eq. ',') then
          ifrst = ifrst + 1
          go to 100
        else if (token(jtok) .eq. ';') then
          go to 9999
        endif
 
*---- Sign may be part of a flag name.
        sflag = .false.
        sign = token(jtok) .eq. '-'  .or.  token(jtok) .eq. '+'
        if (sign) jtok = jtok + 1
 
*---- Read attribute name.
        icase = 1
        call rdword(atrnam, leng)
 
*---- If there is no name, the field must contain data.
        if (leng .eq. 0) then
          jtok = jcomma + 1
 
*---- Name found. Look for a subscript list.
        else
          if (token(jtok) .eq. '(') then
            jind = jtok
  120       continue
              jtok = jtok + 1
              ic = ichtyp(ichar(token(jtok)))
            if (ic .le. 9  .or.  token(jtok) .eq. ',') go to 120
            if (token(jtok) .eq. ')') then
              jtok = jtok + 1
            else
              jtok = jind
              jind = 0
            endif
          else
            jind = 0
          endif
 
*---- If next character is not relational or end of field,
*     the field must contain data (probably an expression).
          ichr = index('=><,;', token(jtok))
          if (ichr .eq. 0) then
            jtok = jcomma + 1
 
*---- We have a name followed by relational or end of field.
*     Test if known attribute name.
          else
            call utlook(atrnam(1:leng), katnam, nkat, jkat)
            if (jkat .eq. 0) then
 
*---- Name alone is data.
              if (ichr .gt. 3) then
                jtok = jcomma + 1
 
*---- Exit for special attribute list; resume at comma.
              else if (nnkat .lt. 0) then
                jtok = jcomma
                go to 9999
 
*---- Unknown name followed by relational is not allowed.
              else
                call rdfail('AAATTR', 1, 'Unknown attribute name.')
                sflag = .true.
              endif
 
*---- Known attribute name. Decode subscript list, if any.
            else
              if (jind .ne. 0) then
                jtmp = jtok
                jtok = jind
                call dcindx(ind1, ind2, ind3, sflag)
                if (sflag) then
                  sflag = .true.
                else if (ind1 .gt. iadim1(jkat)  .or.
     +                   ind2 .gt. iadim2(jkat)  .or.
     +                   ind3 .gt. iadim3(jkat)) then
                  call rdfail('AAATTR', 1, 'Index out of range.')
                  sflag = .true.
                endif
                jtok = jtmp
              else
                ind1 = 1
                ind2 = 1
                ind3 = 1
              endif
 
*---- Compute position in bank.
              jlast = 0
              do 130 j = 1, jkat
                jlast = jlast + iadim1(j) * iadim2(j) * iadim3(j)
  130         continue
              jfrst = jlast + (ind1 - iadim1(jkat))
     +          + (ind2 - iadim2(jkat)) * iadim1(jkat)
     +          + (ind3 - iadim3(jkat)) * iadim1(jkat) * iadim2(jkat)
 
*---- If relational is present, use name and index.
              if (ichr .le. 3) then
                if (sign) then
                  call rdfail('AAATTR', 1, 'Invalid attribute phrase.')
                  sflag = .true.
                endif
                ikat = jkat
                ifrst = jfrst
                ilast = jlast
                itype = iatype(jkat)
                jtok = jtok + 1
 
*---- Deal with logical type.
              else if (iatype(jkat) .eq. mtlog) then
                ikat = jkat
                ifrst = jfrst
                ilast = jlast
                itype = iatype(jkat)
                icase = 3
 
*---- Name (possibly subscripted). Look for default.
              else
                idata = mbat + mcsiz * (jfrst - 1)
                jtype = iq(laadef+idata+mctyp)
                if (mod(jtype,10) .ne. 0) then
                  if (sign) then
                    call rdfail('AAATTR',1,'Invalid attribute phrase.')
                    sflag = .true.
                  endif
                  ikat = jkat
                  ifrst = jfrst
                  ilast = jlast
                  itype = iatype(jkat)
                  icase = 2
 
*---- Anything else must be data.
                else
                  jtok = jcomma + 1
                endif
              endif
            endif
          endif
        endif
 
*---- ICASE = 1. Following field must be data.
        if (.not. sflag) then
          if (icase .eq. 1) then
            if (ikat .le. nkat) then
              call dcattr(itype, laabnk, ifrst, ilast, sflag)
 
*---- Exit for special attribute list; resume at comma.
            else if (nnkat .lt. 0) then
              jtok = jcomma
              go to 9999
 
*---- Storing beyond end of bank.
            else
              call rdfail('AAATTR', 1,
     +        'Attempt to store beyond last attribute.')
              sflag = .true.
            endif
 
*---- ICASE = 2. Copy default value for this field.
          else if (icase .eq. 2) then
            call aacopy(laadef, ifrst, laabnk)
            ifrst = ifrst + 1
 
*---- ICASE = 3. Decode logical flag.
          else if (icase .eq. 3) then
            idata = mbat + mcsiz * (ifrst - 1)
            ival = 0
            if (token(jcomma+1) .ne. '-') ival = 1
            iq(laabnk+idata+mctyp) = 10 * mtlog + 1
            iq(laabnk+idata+mcval) = ival
            ifrst = ifrst + 1
          endif
        endif
 
*---- After an error try to skip to next field.
        if (sflag) then
          eflag = .true.
          call rdfind(',;')
        endif
        go to 100
      endif
 
*---- End of attribute list.
      if (token(jtok) .ne. ';') then
        call rdfail('AAATTR', 1, 'Semicolon ";" expected.')
        eflag = .true.
      endif
 
 9999 end
