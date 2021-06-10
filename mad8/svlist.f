      subroutine svlist(llist)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save beam line list of replacement list.                           *
* Input:                                                               *
*   LLIST(1)  (pointer) Pointer to list bank.                          *
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
      integer iact,icell,idir,iform,ihead,irep
      integer           llist(1)
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
      integer mlact,mlf1,mlf2,mlfm,mlfree,mlhd,mlnxt,mlprv,mlref,mlrep,
     +mlsiz,mltyp
 
*---- Bias for beam line list information.
      parameter         (mlfm = mbat + 1, mlhd = mbat + 2,
     +                   mlf1 = mbat + 3, mlf2 = mbat + 4,
     +                   mlfree = mbat + 4)
 
*---- Bias for beam line list cells.
      parameter         (mltyp = 1, mlprv = 2, mlnxt = 3, mlrep = 4,
     +                   mlref = 5, mlact = 6, mlsiz = 6)
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
 
      character*(mcnam) name
 
      icell = 0
      ihead = iq(llist(1)+mlhd)
 
*---- Procedure "save list".
  100 continue
      iq(llist(1)+ihead+mlref) = icell
      icell = iq(llist(1)+ihead+mlnxt)
      call svlitt('(')
 
*---- Repetition count.
  200 continue
      irep = iq(llist(1)+icell+mlrep)
      if (irep .lt. 0) then
        call svlitt('-')
        irep = - irep
      endif
      if (irep .ne. 1) then
        call svint(irep)
        call svlitt('*')
      endif
 
*---- Sublist reference.
      if (iq(llist(1)+icell+mltyp) .eq. 4) then
        ihead = iq(llist(1)+icell+mlref)
        if (ihead .gt. iq(llist(1)+mlf2)) go to 100
        iform = ((ihead - iq(llist(1)+mlf1)) / (2 * mlsiz)) * mwnam
        call uhtoc(q(lq(llist(1)-1)+iform+1), mcwrd, name, mcnam)
        call svname(name)
 
*---- Name reference.
      else
        idir = iq(llist(1)+icell+mlref)
        call diname(ldbnk, idir, name)
        call svname(name)
 
*---- Actual arguments, if present.
        iact = iq(llist(1)+icell+mlact)
        if (iact .ne. 0) then
          ihead = iact
          go to 100
        endif
      endif
 
*---- Go to next list member.
  300 continue
      icell = iq(llist(1)+icell+mlnxt)
      if (iq(llist(1)+icell+mltyp) .ge. 4) then
        call svlitt(',')
        go to 200
      endif
      call svlitt(')')
 
*---- End of procedure "save list".
      ihead = icell
      icell = iq(llist(1)+ihead+mlref)
      iq(llist(1)+ihead+mlref) = 0
      if (icell .ne. 0) go to 300
 
      end
