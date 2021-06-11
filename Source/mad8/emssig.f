      subroutine emssig(ipos, orbit, disp, sigma)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save beam envelope for subsequent use in ENVELOPE command.         *
* Input:                                                               *
*   IPOS      (integer) Position where SIGMA0 bank is linked.          *
*   ORBIT(6)  (real)    Orbit position.                                *
*   SIGMA(6,6)(real)    Beam matrix in internal form.                  *
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
      integer ikey,ipos
      double precision corr,disp,orbit,sigma
      dimension         orbit(6), disp(6), sigma(6,6)
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
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
 
      dimension         corr(6,6)
 
*---- Search for desired SIGMA0 bank chain.
      call difind(ldkey, 'SIGMA0', ikey, lckey)
      ltwbet = lq(lckey-1)
      call emci2t(sigma, corr)
 
*---- Loop to store in proper bank(s).
  100 if (ltwbet .ne. 0) then
        if (iq(ltwbet-5) .eq. ipos) then
          call utpflt(ltwbet,  1,  6, orbit)
          call utpflt(ltwbet,  7, 12, disp)
          call utpflt(ltwbet, 13, 18, corr(1,1))
          call utpflt(ltwbet, 19, 23, corr(2,2))
          call utpflt(ltwbet, 24, 27, corr(3,3))
          call utpflt(ltwbet, 28, 30, corr(4,4))
          call utpflt(ltwbet, 31, 32, corr(5,5))
          call utpflt(ltwbet, 33, 33, corr(6,6))
        endif
        ltwbet = lq(ltwbet)
        go to 100
      endif
 
      end
