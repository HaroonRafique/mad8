      subroutine enrang(lrng, rngnam)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Encode the name of a range.                                        *
* Input:                                                               *
*   LRNG(1)   (pointer) Range reference bank.                          *
* Output:                                                              *
*   RNGNAM*(*)(char)    Range name.                                    *
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
      integer i,icode1,icode2,idir1,idir2,index1,index2,j,len,leng1,
     +leng2,mclab
      integer           lrng(*)
      character*(mcrng) rngnam
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
 
      character*(mcnam) elnam1, elnam2
      parameter         (mclab = mcnam + 8)
      character*20      label1, label2
      character*(mcrng) tmpnam
 
*---- Fetch data.
      icode1 = iq(lrng(1)+1)
      idir1  = iq(lrng(1)+2)
      index1 = iq(lrng(1)+3)
      icode2 = iq(lrng(1)+4)
      idir2  = iq(lrng(1)+5)
      index2 = iq(lrng(1)+6)
      call diname(ldbnk, idir1, elnam1)
      call diname(ldbnk, idir2, elnam2)
 
*---- Ordinary range.
      if (icode1 .lt. 7) then
        if (icode1 .eq. 1) then
          label1 = '#S'
          leng1 = 2
        else if (icode1 .eq. 2) then
          label1 = '#E'
          leng1 = 2
        else if (icode1 .eq. 3) then
          label1 = '#F'
          leng1 = 2
        else if (icode1 .eq. 4) then
          label1 = '#L'
          leng1 = 2
        else if (icode1 .eq. 5) then
          write (label1, '(''#'',I7)') index1
          leng1 = 8
        else
          write (label1, '(A12,''['',I6,'']'')') elnam1, index1
          leng1 = 12
          if (index1 .ne. 0) leng1 = 20
        endif
        if (icode2 .eq. 1) then
          label2 = '#S'
          leng2 = 2
        else if (icode2 .eq. 2) then
          label2 = '#E'
          leng2 = 2
        else if (icode2 .eq. 3) then
          label2 = '#F'
          leng2 = 2
        else if (icode2 .eq. 4) then
          label2 = '#L'
          leng2 = 2
        else if (icode2 .eq. 5) then
          write (label2, '(''#'',I7)') index2
          leng2 = 8
        else
          write (label2, '(A12,''['',I6,'']'')') elnam2, index2
          leng2 = 12
          if (index2 .ne. 0) leng2 = 20
        endif
 
*---- Pack non-blank characters to range name.
        j = 0
        rngnam = ' '
        do 10 i = 1, leng1
          if (label1(i:i) .ne. ' ') then
            j = j + 1
            rngnam(j:j) = label1(i:i)
          endif
   10   continue
        j = j + 1
        rngnam(j:j) = '/'
        do 20 i = 1, leng2
          if (label2(i:i) .ne. ' ') then
            j = j + 1
            if (j .gt. len(rngnam)) go to 30
            rngnam(j:j) = label2(i:i)
          endif
   20   continue
   30   continue
 
*---- "Selection" range.
      else if (index1 .eq. 0) then
        rngnam = elnam1
      else
        write (tmpnam, '(A12,''['',I6,''/'',I6,'']'')')
     +  elnam1, index1, index2
        j = 0
        rngnam = ' '
        do 40 i = 1, len(tmpnam)
          if (tmpnam(i:i) .ne. ' ') then
            j = j + 1
            rngnam(j:j) = tmpnam(i:i)
          endif
   40   continue
      endif
 
      end
