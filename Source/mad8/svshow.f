      subroutine svshow
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Show definitions.                                                  *
* Attribute:                                                           *
*   COMMAND   (name)    Command to be shown (blank: list all commands).*
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
      integer i,icmd,j,l,lcmd,n,ncmd
 
      character*(mcnam) label, name(8)
 
*---- Retrieve command name.
      label = ' '
      call utgnam(lccmd, 1, 1, label)
 
*---- Blank command: list all known commands in alphabetical order.
      if (label .eq. ' ') then
        write (iqlog, 910)
        n = 0
        do 10 i = 1, iq(ldbnk(3)+1)
          j = iq(ldbnk(1)+i)
          l = lq(ldbnk(3)-j)
          call diname(ldbnk, j, label)
          if (l .ne. 0  .and.  label .ne. ' ') then
            if (iq(l+mbpr) .ne. mpelm  .or. iq(l+mbsp) .ne. 1  .or.
     +          label(1:1) .ne. '['  .or.  label(8:) .ne. ']') then
              n = n + 1
              name(n) = label
              if (n .ge. 8) then
                write (iqlog, 920) name
                n = 0
              endif
            endif
          endif
   10   continue
        if (n .ne. 0) write (iqlog, 920) (name(i), i = 1, n)
        write (iqlog, 930)
 
*---- Otherwise find command and show its attributes.
      else
        call utleng(label, ncmd)
        call difind(ldbnk, label(1:ncmd), icmd, lcmd)
        if (lcmd .eq. 0) then
          write (iqlog, 940) label
        else
          isave = iqlog
          if (iq(lcmd+mbpr) .eq. mppar) then
            write (iqlog, 950) label
            call svparm(lcmd)
          else if (iq(lcmd+mbpr) .eq. mpelm) then
            write (iqlog, 950) label
            call svbank(lcmd)
          else if (iq(lcmd+mbpr) .eq. mplin) then
            write (iqlog, 950) label
            call svline(lcmd)
          else if (iq(lcmd+mbpr) .eq. mpsub) then
            write (iqlog, 950) label
            call svsubr(lcmd)
          else
            write (iqlog, 960) label
            call svbank(lcmd)
          endif
        endif
      endif
 
  910 format(' '/' Known commands and definitions:')
  920 format(4('  ',a16))
  930 format(' Type "SHOW, name" for information about "name".')
  940 format(' '/' Name "',a,'" is unknown.'/
     +       ' Type "SHOW" to list known names.'/' ')
  950 format(' '/' Definition: ',a)
  960 format(' '/' Command: ',a)
 
      end
