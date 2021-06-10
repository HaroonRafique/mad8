      subroutine kwgrp(ikat, icat, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode keyword attribute group.                                    *
* Input:                                                               *
*   IKAT     (integer)  Attribute number (in keyword).                 *
*   ICAT     (integer)  Attribute number (in command).                 *
*   LCCMD    /REFER/    Pointer to keyword being decoded.              *
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
      integer i1,i2,icat,ikat,index,itype,jcat,k,kcode,lcat,nsize
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
 
      logical           sflag
 
      eflag = .false.
      kcode = 0
      nsize = iadim1(ikat) * iadim2(ikat) * iadim3(ikat)
      lcat = icat + nsize - 1
      jcat = icat
 
*---- Skip opening parenthesis "(" or comma ",".
  100 continue
        jtok = jtok + 1
        sflag = .false.
 
*---- Skip data type and dimensions.
        k = index('NIRDLSBPCV', token(jtok))
        if (k .ne. 0  .and.  index('(*,', token(jtok+1)) .ne. 0) then
          jtok = jtok + 1
          if (token(jtok) .eq. '(') then
            call rdform(i1, i2, sflag)
            if (sflag) go to 200
          endif
          call rdfind(',);')
 
*---- Skip attribute name.
        else if (token(jtok) .eq. ':') then
          call rdfind(',);')
 
*---- Anything else must be data.
        else
          if (token(jtok) .eq. '=') then
            jtok = jtok + 1
            kcode = 1
          endif
          if (kcode .ne. 0) then
            itype = iatype(ikat)
            call dcattr(itype, lq(lccmd-2), jcat, lcat, sflag)
            if (sflag) go to 200
          endif
        endif
        if (token(jtok) .eq. ',') go to 100
        if (token(jtok) .eq. ')') go to 300
        msg(1) =
     +  'Character "' // token(jtok) // '" is not allowed here.'
        call rdfail('KWGRP', 1, msg)
 
*---- Error recovery.
  200   continue
        call rdfind(',;)')
        eflag = .true.
      go to 100
 
*---- Skip closing parenthesis ")".
  300 continue
      jtok = jtok + 1
 
      end
