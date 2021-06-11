      subroutine utelem(lseq, ipos, iflag, elmnam, iocc, ienum)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fetch data for current element in current beam line sequence.      *
* Input:                                                               *
*   LSEQ(1)   (pointer) Pointer to working beam line.                  *
*   IPOS      (integer) Current element position in working line.      *
* Output:                                                              *
*   IFLAG     (integer) Current position flag word.                    *
*   ELMNAM    (char)    Current element name.                          *
*   IOCC      (integer) Occurrence number.                             *
*   LCELM     /REFER/   Current element bank.                          *
*   LCALI     /REFER/   Current alignment error pointer.               *
*   LCFLD     /REFER/   Current field error pointer.                   *
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
      integer idir,ienum,iflag,ileng,iname,iocc,ipos,jbyt
      integer           lseq(*)
      character*(mcnam) elmnam
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
 
      idir = iq(lq(lseq(1)-msdir)+ipos)
      lcelm = lq(ldbnk(3)-idir)
      ienum = iq(lq(lseq(1)-msnum)+ipos)
      iname = (idir - 1) * mwnam + 1
      call uhtoc(q(ldbnk(2)+iname), mcwrd, elmnam, mcnam)
      iflag = iq(lq(lseq(1)-msflg)+ipos)
      iocc = jbyt(iflag,mocc1,mocc2)
      if (lcelm .eq. 0) then
        call utleng(elmnam, ileng)
        msg(1) = 'Unknown element name "' // elmnam(1:ileng)
     +  // '" occurs in working line.'
        call aafail('UTELEM', 1, msg)
      endif
      lcali = 0
      lcfld = 0
      lccom = 0
      if (doali  .and.  lq(lseq(1)-msali) .ne. 0) then
        lcali = lq(lq(lseq(1)-msali)-ipos)
      endif
      if (dofld  .and.  lq(lseq(1)-msfld) .ne. 0) then
        lcfld = lq(lq(lseq(1)-msfld)-ipos)
      endif
      if (dokick  .and.  lq(lseq(1)-mscom) .ne. 0) then
        lccom = lq(lq(lseq(1)-mscom)-ipos)
      endif
 
      end
