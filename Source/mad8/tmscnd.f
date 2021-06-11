      subroutine tmscnd(lseq)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map w.r.t. actual orbit for one (half) superperiod.      *
*   Misalignment and field errors are considered.                      *
* Input:                                                               *
*   LSEQ(1)   (pointer) Beam line expansion bank.                      *
* Output:                                                              *
* Important common data:                                               *
*   RT(6,6)   /MAPTRN/  Transfer matrix for one (half) superperiod.    *
*   TT(6,6,6) /MAPTRN/  Second order terms.                            *
*   ORBIT0(6) /OPTIC0/  Initial conditions for reference orbit.        *
*   ORBIT(6)  /OPTIC1/  Final conditions for reference orbit.          *
*   SUML      /OPTIC1/  Cumulated length.                              *
*   LCELM     /REFER/   Current element bank.                          *
*   LCALI     /REFER/   Current misalignment pointer.                  *
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
      integer i,icode,ienum,iflag,iocc,ipos,j,jbit,jbyt,k
      double precision el
      integer           lseq(*)
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
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      logical           fdump, fmap
      character*(mcnam) elmnam
 
*---- Initialize.
      call ucopy(orbit0, orbit, 6*mwflt)
      cplxy = .false.
      cplxt = .false.
      suml = 0.0
      call m66one(rt)
      call uzero(tt, 1, 216*mwflt)
 
      do 90 ipos = iq(lseq(1)+msr1), iq(lseq(1)+msr2)
        call utelem(lseq, ipos, iflag, elmnam, iocc, ienum)
        fdump = jbit(iflag,mscnd) .ne. 0
        icode = jbyt(iflag,1,mcode)
        if (fdump) write (iqlog, 910) elmnam
 
*---- Misalignment at entrance.
        if (icode .ne. 3  .and.  lcali .ne. 0) then
          call tmali1(ipos, .true., orbit, orbit, re, te)
          if (fdump) then
            write (iqlog, 920) ((re(i,j), j=1,6), i=1,6)
            write (iqlog, 960) orbit
            do 20 i = 1, 6
              write (iqlog, 930) i, ((te(i,j,k), k=1,6), j=1,6)
   20       continue
          endif
          call tmcat(.true., re, te, rt, tt, rt, tt)
        endif
 
*---- Element matrix and length.
        if (icode .eq. 1) then
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) then
            if (fdump) then
              write (iqlog, 940) ((re(i,j), j=1,6), i=1,6)
              write (iqlog, 960) orbit
              do 30 i = 1, 6
                write (iqlog, 930) i, ((te(i,j,k), k=1,6), j=1,6)
   30         continue
            endif
            call tmcat(.true., re, te, rt, tt, rt, tt)
            suml = suml + el
          endif
        endif
 
*---- Misalignment at exit.
        if (icode .ne. 2  .and.  lcali .ne. 0) then
          call tmali2(ipos, .true., orbit, orbit, re, te)
          if (fdump) then
            write (iqlog, 950) ((re(i,j), j=1,6), i=1,6)
            write (iqlog, 960) orbit
            do 40 i = 1, 6
              write (iqlog, 930) i, ((te(i,j,k), k=1,6), j=1,6)
   40       continue
          endif
          call tmcat(.true., re, te, rt, tt, rt, tt)
        endif
   90 continue
 
  910 format(' '/' TMSCND.  Entering element "',a,'".')
  920 format(t11,'Misalignment at entrance:'/(' ',6e16.8))
  930 format(t11,'Second order terms, I = ',i6/(' ',6e16.8))
  940 format(t11,'Element transfer matrix:'/(' ',6e16.8))
  950 format(t11,'Misalignment at exit:'/(' ',6e16.8))
  960 format(t11,'Orbit:'/(' ',6e16.8))
 
      end
