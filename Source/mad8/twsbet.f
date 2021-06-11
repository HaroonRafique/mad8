      subroutine twsbet(ipos, fsec)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save lattice functions for subsequent TWISS, in a BETA0 bank       *
* Input:                                                               *
*   IPOS      (integer) Position where BETA0 bank is linked.           *
*   FSEC      (logical) If true, store also chromatic functions.       *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Added energy to BETA0 bank; added warning if chromatic functions   *
*   are not set                                                        *
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
      double precision twopi
      logical           fsec
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
 
*---- Search for desired BETA0 bank chain.
      call difind(ldkey, 'BETA0', ikey, lckey)
      ltwbet = lq(lckey-1)
*---- Loop to store in proper bank(s).
   10 if (ltwbet .ne. 0) then
        if (iq(ltwbet-5) .eq. ipos) then
 
*---- Store lattice functions.
          amux = amux / twopi
          amuy = amuy / twopi
          call utpflt(ltwbet, 1, 6, betx)
          amux = amux * twopi
          amuy = amuy * twopi
          call utpflt(ltwbet, 7, 12, orbit)
          call utpflt(ltwbet, 13, 16, disp)
          if (fsec) then
            call utpflt(ltwbet, 17, 22, wx)
            call utpflt(ltwbet, 23, 26, ddisp)
          else
            msg(1) = 'Chromatic functions in BETA0 are zero.'
            msg(2) = 'Use CHROM option to get non-zero values.'
            call aawarn ('TWSBET', 2, msg)
          endif
          call utpflt(ltwbet, 27, 27, ener1)
 
*---- Go to next bank.
        endif
        ltwbet = lq(ltwbet)
        go to 10
      endif
 
      end
