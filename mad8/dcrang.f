      subroutine dcrang(ilink, idata, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode observation point(s) or range.                              *
* Input:                                                               *
*   LDCBNK   /DCLINK/   Data bank pointer.                             *
*   ILINK    (integer)  Bias for pointer to sub-bank.                  *
*   IDATA    (integer)  Bias for data block.                           *
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
      integer ichar,icode1,icode2,idata,idir1,idir2,ilink,index1,index2,
     +leng1,leng2
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*(mcnam) elnam1, elnam2
      logical           dvflag, hash,   select, sflag
 
*---- Drop any previous 'range' bank.
      eflag = .false.
      ldcatt = lq(ldcbnk-ilink)
      if (ldcatt .ne. 0) call aadrop(ldcatt)
 
*---- Initialize.
      dvflag = .false.
      hash = .false.
      select = .false.
      index1 = 0
      index2 = 0
      icode2 = 0
      idir1 = 0
      idir2 = 0
 
*==== Start of range.
*---- Hash sign "#", followed by "S", "E", "F", "L", or integer.
      if (token(jtok) .eq. '#') then
        jtok = jtok + 1
        hash = .true.
        if (token(jtok) .eq. 'S') then
          jtok = jtok + 1
          icode1 = 1
        else if (token(jtok) .eq. 'E') then
          jtok = jtok + 1
          icode1 = 2
        else if (token(jtok) .eq. 'F') then
          jtok = jtok + 1
          icode1 = 3
        else if (token(jtok) .eq. 'L') then
          jtok = jtok + 1
          icode1 = 4
        else
          call rdint(index1, sflag)
          if (sflag) go to 890
          icode1 = 5
        endif
 
*---- Integer alone.
      else if (ichtyp(ichar(token(jtok))) .le. 9) then
        call rdint(index1, sflag)
        if (sflag) go to 890
        icode1 = 5
 
*---- Name, optionally followed by square bracket.
      else
        call rdword(elnam1, leng1)
        if (leng1 .eq. 0) go to 810
        icode1 = 6
        call direfe(ldbnk, elnam1(1:leng1), idir1)
 
*---- May be "select" type range.
        select = .true.
        if (token(jtok) .eq. '[') then
          jtok = jtok + 1
          call rdint(index1, sflag)
          if (sflag) go to 890
          index2 = index1
          if (token(jtok) .eq. '/') then
            jtok = jtok + 1
            call rdint(index2, sflag)
            if (sflag) go to 890
            if (index1 .gt. index2) go to 820
            dvflag = .true.
          endif
          if (token(jtok) .ne. ']') go to 830
          jtok = jtok + 1
        endif
      endif
 
*==== End of range.
      if (token(jtok) .eq. '/') then
        if (dvflag) go to 840
        jtok = jtok + 1
        dvflag = .true.
 
*---- Cannot be "select" type range.
        select = .false.
 
*---- Hash sign "#", followed by "S", "E", "F", "L", or integer.
        if (token(jtok) .eq. '#') then
          jtok = jtok + 1
          if (token(jtok) .eq. 'S') then
            jtok = jtok + 1
            icode2 = 1
          else if (token(jtok) .eq. 'E') then
            jtok = jtok + 1
            icode2 = 2
          else if (token(jtok) .eq. 'F') then
            jtok = jtok + 1
            icode2 = 3
          else if (token(jtok) .eq. 'L') then
            jtok = jtok + 1
            icode2 = 4
          else
            call rdint(index2, sflag)
            if (sflag) go to 890
            icode2 = 5
          endif
 
*---- Integer alone.
        else if (ichtyp(ichar(token(jtok))) .le. 9) then
          call rdint(index2, sflag)
          if (sflag) go to 890
          icode2 = 5
 
*---- Name, or "#?/S", "#?/E", "#?/F", or "#?/L".
        else
          call rdword(elnam2, leng2)
          if (leng2 .le. 0) go to 810
          if (hash  .and.  elnam2 .eq. 'S') then
            icode2 = 1
          else if (hash  .and.  elnam2 .eq. 'E') then
            icode2 = 2
          else if (hash  .and.  elnam2 .eq. 'F') then
            icode2 = 3
          else if (hash  .and.  elnam2 .eq. 'L') then
            icode2 = 4
          else
            icode2 = 6
            call direfe(ldbnk, elnam2(1:leng2), idir2)
            if (token(jtok) .eq. '[') then
              jtok = jtok + 1
              call rdint(index2, sflag)
              if (sflag) go to 890
              if (token(jtok) .ne. ']') go to 830
              jtok = jtok + 1
            endif
          endif
        endif
      endif
 
*---- Set code for "select" range.
      if (select) then
        icode1 = 7
        icode2 = 7
        idir2 = idir1
 
*---- Fill in second group, if no slash seen.
      else if (.not. dvflag) then
        index2 = index1
        icode2 = icode1
        idir2 = idir1
      endif
 
*---- Lift 'range' bank.
      call mzbook(2, ldcatt, ldcbnk, -ilink, 'RANG', 0, 0, 6, 2, 0)
      iq(ldcatt+1) = icode1
      iq(ldcatt+2) = idir1
      iq(ldcatt+3) = index1
      iq(ldcatt+4) = icode2
      iq(ldcatt+5) = idir2
      iq(ldcatt+6) = index2
      iq(ldcbnk+idata+mctyp) = 10 * mtrng + 1
      go to 9999
 
*---- Name expected.
  810 continue
      call rdfail('DCRANG', 1, '"#" or name expected.')
      go to 890
 
*---- Indices out of order.
  820 continue
      write (msg, 910) index1, index2
  910 format('Bad index order ',i8,'/',i8)
      call rdfail('DCRANG', 1, msg)
      go to 890
 
*---- Missing "]".
  830 continue
      call rdfail('DCRANG', 1, 'Missing closing bracket "]".')
      go to 890
 
*---- Invalid "/".
  840 continue
      call rdfail('DCRANG', 1, '"/" not allowed here.')
 
*---- Return error flag set.
  890 continue
      iq(ldcbnk+idata+mctyp) = 10 * mtrng
 
 9999 end
