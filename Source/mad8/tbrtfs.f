      subroutine tbrtfs(tnam, iunit)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read a table in TFS coded format (ASCII or EBCDIC).                *
*   This routine uses a local link L. Be careful with Zebra calls.     *
* Input:                                                               *
*   TNAM      (char)    Name for stored table.                         *
*   IUNIT     (integer) Logical unit number to be read.                *
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
      integer iform,ileng,ipos,iunit,ival,j,jcol,jrow,jseg,l,linsav,
     +lzlong,mrow,mseg,nd,nfrm,nnam,nr,nrow,ns,nseg,
     +nwid
      double precision rval
      character*(mcnam) tnam
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
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      character*1024    text
      equivalence       (text, token(1))
      integer icurr,itabun,itbbuf,itbfil,itbspc,koff,ltable,ltbbuf,
     +ltbcol,ltbcur,ltbdsc,ltbsav,ltbspc,ltbsum,ltbtab,ltbtmp,nblock,
     +nbout,ncmax,nrbmod
 
*---- Communication area for table manager routines.
      integer mleng,mnblck,mstep
      parameter         (mnblck=10, mleng=512*mnblck, mstep=100)
      common /tbcomm/   ltable, ltbcol, ltbsum,
     +                  ltbbuf, ltbspc, ltbdsc, ltbtab, ltbcur, ltbsav,
     +                  ltbtmp,
     +                  nblock, nbout, nrbmod, icurr, ncmax, itbspc,
     +                  koff, itbfil, itabun, itbbuf(mleng,2)
      save              /tbcomm/
      integer icfrm,iclen,maxcol
 
*---- Table header information.
      parameter         (maxcol = 100)
      common /tbhedc/   cname(maxcol), cform(maxcol)
      common /tbhedf/   icfrm(maxcol), iclen(maxcol)
      save              /tbhedc/, /tbhedf/
      character*20      cname, cform
      integer mtbact,mtbbky,mtbbuf,mtbcfm,mtbcnm,mtbcol,mtbcps,mtbdsc,
     +mtbf1,mtbf2,mtbfst,mtblst,mtbmod,mtbnam,mtbrow,mtbseg,mtbsiz,
     +mtbsky,mtbwid
 
*---- Parameters for table manager bank structure.
      parameter         (mtbact = 1, mtbbuf = 2, mtbmod = 1)
      parameter         (mtbf1  = 1,
     +                   mtbseg = 2, mtbrow = 3, mtbcol = 4, mtbwid = 5,
     +                   mtbf2  = 6,
     +                   mtbnam = 7, mtbsiz = mtbnam + mwnam - 1)
      parameter         (mtbsky = 2, mtbbky = 3, mtbcnm = 4,
     +                   mtbcfm = 5, mtbcps = 6, mtbdsc = 7,
     +                   mtbfst = 8, mtblst = 9)
 
      character         sval*99, tname*20
      integer           iname(mwnam)
 
*---- Look for previous definition.
      call uctoh(tnam, iname, mcwrd, mcnam)
      ltbsav = lzlong(0, ltable, mwnam, iname, mtbnam)
      if (ltbsav .ne. 0) then
        call tbdrop(ltbsav)
        call utleng(tnam, ileng)
        msg(1) = 'Table buffer already exists "' // tnam(1:ileng)
     +  // '" ---- previous version deleted.'
        call aawarn('TBRTFS', 1, msg)
      endif
 
*---- Lift main table bank.
      nr = mtblst
      ns = mtbfst
      call mzbook(2,ltbsav,ltable,1,'TAB ',nr,ns,mtbsiz,7,0)
      iq(ltbsav+mtbf1) = 16 * 4 + 2
      iq(ltbsav+mtbf2) = 16 * mwnam + 5
      call ncopy(iname, iq(ltbsav+mtbnam), mwnam)
      nfrm = 0
      nnam = 0
      nrow = 0
      nseg = 0
      call sbit1(iq(ltbsav), mtbact)
 
*---- Read one line.
      linsav = lintok
      lintok = 0
   10 continue
        jtok = 1
        read (iunit, '(A)', end = 90) text
        lintok = lintok + 1
        call utleng(text, ntok)
        token(ntok+1) = ' '
 
*---- Header line containing names.
        if (token(jtok) .eq. '*') then
          if (nrow .ne. 0  .or.  nseg .ne. 0) then
            call rdfail('TBRTFS', 1, 'Header line out of order.')
          else if (nnam .ne. 0) then
            call rdfail('TBRTFS', 1, 'Redundant header line.')
          else
            jtok = jtok + 1
   20       if (jtok .le. ntok) then
              call tbname(tname)
              if (tname .eq. ' ') go to 30
              if (nnam .lt. maxcol) then
                nnam = nnam + 1
                cname(nnam) = tname
                go to 20
              else
                write (msg, 910) maxcol
  910           format('Number of columns exceeds limit of ',i3,'.')
                call rdfail('TBRTFS', 1, msg)
              endif
            endif
   30       continue
          endif
          nd = mwnam * nnam
          call mzbook(2,l,ltbsav,-mtbcnm,'CNAM',0,0,nd,5,0)
          do 40 j = 1, nnam
            call uctoh(cname(j), iq(l+1), mcwrd, mcnam)
            l = l + mwnam
   40     continue
          iq(ltbsav+mtbcol) = nnam
 
*---- Header line containing data formats.
        else if (token(jtok) .eq. '$') then
          if (nrow .ne. 0  .or.  nseg .ne. 0) then
            call rdfail('TBRTFS', 1, 'Header line out of order.')
          else if (nfrm .ne. 0) then
            call rdfail('TBRTFS', 1, 'Redundant header line.')
          else
            jtok = jtok + 1
   50       if (jtok .le. ntok) then
              call tbform(iform, ileng)
              if (iform .eq. 0) go to 60
              if (nfrm .lt. maxcol) then
                nfrm = nfrm + 1
                icfrm(nfrm) = iform
                iclen(nfrm) = ileng
                go to 50
              else
                write (msg, 920) maxcol
  920           format('Number of formats exceeds limit of ',i3,'.')
                call rdfail('TBRTFS', 1, msg)
              endif
            endif
   60       continue
          endif
          call mzbook(2,l,ltbsav,-mtbcfm,'CFRM',0,0,nfrm,2,0)
          call ncopy(icfrm, iq(l+1), nfrm)
          nd = nfrm + 1
          call mzbook(2,l,ltbsav,-mtbcps,'CPOS',0,0,nd,2,0)
          nwid = 0
          do 70 jcol = 1, nfrm
            if (icfrm(jcol) .le. 3) then
              nwid = nwid + 1
            else if (icfrm(jcol) .eq. 4) then
              nwid = nwid + 2
            else if (icfrm(jcol) .eq. 5) then
              nwid = nwid + mwnam
            endif
            iq(l+jcol+1) = nwid
   70     continue
          iq(ltbsav+mtbwid) = nwid
 
*---- Descriptor line.
        else if (token(jtok) .eq. '@') then
          jtok = jtok + 1
          call tbname(tname)
          call tbform(iform, ileng)
          call tbdata(iform, ival, rval, sval)
          call tbpdsc(ltbsav, tname, iform, ival, rval, sval)
 
*---- Segment descriptor line.
        else if (token(jtok) .eq. '!') then
          if (text(2:8) .eq. 'Segment') then
            if (nseg .eq. 0) then
              if (nrow .ne. 0  .or.  nwid .eq. 0) then
                call rdfail('TBRTFS', 1, 'Header line out of order.')
              else
                read (text, '(8X,3I8)') jseg, nseg, nrow
                call mzbook(2,l,ltbsav,-mtbsky,'SKEY',0,0,nseg,2,0)
                iq(ltbsav+mtbseg) = nseg
                iq(ltbsav+mtbrow) = nrow
                call tbbuff(nrow)
              endif
            else
              read (text, '(8X,3I8)') jseg, mseg, mrow
              call tbseg(ltbsav, jseg, error)
            endif
            nrow = 0
          endif
 
*---- Data line.
        else if (token(jtok) .ne. '%'  .and.
     +           token(jtok) .ne. '#') then
          if (nnam .eq. 0  .or.  nfrm .eq. 0) then
            call rdfail('TBRTFS', 1, 'Table header line missing.')
          else if (nnam .ne. nfrm) then
            call rdfail('TBRTFS', 1,
     +      'Numbers of names and formats are not the same.')
          else
            nrow = nrow + 1
            if (nseg .ne. 0) then
              call tbset(ltbsav, nrow, 3, ltbbuf)
              if (ltbbuf .eq. 0) go to 10
            else
              call mzbook(2,ltbbuf,ltbsav,-mtbfst,'BUFF',0,0,nwid,0,0)
              iq(ltbbuf-5) = nrow
              call sbit1(iq(ltbbuf), mtbmod)
            endif
 
*---- Decode data line.
            do 80 jcol = 1, nnam
              iform = icfrm(jcol)
              ileng = iclen(jcol)
              ipos = iq(lq(ltbsav-mtbcps)+jcol)
              call tbdata(iform, ival, rval, sval)
              if (iform .le. 2) then
                iq(ltbbuf+ipos+1) = ival
              else if (iform .eq. 3) then
                q(ltbbuf+ipos+1) = rval
              else if (iform .eq. 4) then
                call ucopy(rval, q(ltbbuf+ipos+1), 2)
              else if (iform .eq. 5) then
                call uctoh(sval, iq(ltbbuf+ipos+1), mcwrd, mcnam)
              endif
   80       continue
          endif
        endif
      go to 10
 
*---- End of table reached. Test for error.
   90 continue
      lintok = linsav
      if (error) then
        call mzdrop(0, ltbsav, '.')
 
*---- Complete buffer structure, if not segmented.
      else
        call ztopsy(0, lq(ltbsav-mtbdsc))
        if (nseg .eq. 0) then
          call mzbook(2, l,ltbsav,-mtbsky,'SKEY',0,0,1,2,0)
          iq(ltbsav+mtbseg) = 1
          iq(ltbsav+mtbrow) = nrow
          lq(ltbsav-mtblst) = lq(ltbsav-mtbfst)
          call ztopsy(0, lq(ltbsav-mtbfst))
          call mzbook(2,l,ltbsav,-mtbbky,'BKEY',nrow,0,nrow,2,0)
          iq(l-5) = 1
          ltbtmp = lq(ltbsav-mtbfst)
          do 110 jrow = 1, nrow
            lq(l-jrow) = ltbtmp
            ltbtmp = lq(ltbtmp)
  110     continue
          call sbit1(iq(ltbsav), mtbbuf)
        endif
      endif
 
      end
