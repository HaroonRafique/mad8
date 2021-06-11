      subroutine aaruns
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Execute a procedure: DO or SUBROUTINE.                             *
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer laabnk,laacur,laadef,laakey,laanew,laaold,laaprc,laasrc,
     +laastk,laatar
 
*---- Local links for control module.
      common /aalink/   laabnk, laadef, laasrc, laatar, laakey, laacur,
     +                  laaprc, laastk, laanew, laaold
      save              /aalink/
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
      integer ileng
      double precision condif,valif
 
      character*(mcnam) label, key, opif
      logical           iftest
 
*---- Enter procedure: Link procedure to stack and set repetition.
  100 continue
        lq(lq(lccmd-1)-1) = laastk
        laastk = lq(lccmd-1)
 
*---- Variable evaluation for IF condition, similar to AAEXEC.
          if (iq(lccmd+mbsp) .eq. 8) then
            call exfill
            if (.not. error) then
              call exordr
              if (.not. error) then
                call exupdt
              endif
            endif
 
*---- Propagate change flags in data structure.
            call aapmod
 
*---- Set repetition to 1 or 0 according to COND, OP and VALUE.
            condif = 0.
            valif  = 0.
            opif   = '>'
            call utgflt(lccmd,2,2,condif)
            call utgnam(lccmd,3,3,opif)
            call utgflt(lccmd,4,4,valif)
            if       (opif .eq. '>') then
              iftest = (condif .gt. valif)
            else if  (opif .eq. '<') then
              iftest = (condif .lt. valif)
            else if  (opif .eq. '=') then
              iftest = (condif .eq. valif)
            else if (opif .eq. '>=') then
              iftest = (condif .ge. valif)
            else if (opif .eq. '<=') then
              iftest = (condif .le. valif)
            else if (opif .eq. '<>') then
              iftest = (condif .ne. valif)
            else
              iftest = .false.
            endif
            if (iftest) then
              iq(lccmd+mbat+mcval) = 1
            else
              iq(lccmd+mbat+mcval) = 0
            endif
          endif
 
*---- End of evaluation of number of repetitions.
        iq(laastk+2) = iq(lccmd+mbat+mcval)
 
*---- Skip procedure if counter is less or equal to 0.
        if (iq(laastk+2) .le. 0) go to 400
 
*---- Begin repetition: Skip one pointer for stack pointer.
  200   continue
          iq(laastk+1) = 1
 
*---- Execute one pass through procedure.
  300     continue
          iq(laastk+1) = iq(laastk+1) + 1
          if (iq(laastk+1) .le. iq(laastk-3)) then
            lcsrc = 0
            lccmd = lq(laastk-iq(laastk+1))
            if (iq(lccmd+mbpr) .eq. mpsub) then
              if (iq(lq(lccmd-1)+1) .ne. 0) then
                call diname(ldbnk, iq(lccmd+mbnam), label)
                call utleng(label, ileng)
                msg(1) = 'Recursive call to "' // label(1:ileng)
     +          // '" --- call skipped.'
                call aawarn('AARUNS', 1, msg)
              else
                go to 100
              endif
            endif
            lckey = lq(lccmd+1)
            call diname(ldbnk, iq(lccmd+mbnam), label)
            call diname(ldkey, iq(lckey+mbnam), key)
            call aaexec(label, key)
            go to 300
          endif
 
*---- More repetions.
          iq(laastk+2) = iq(laastk+2) - 1
        if (iq(laastk+2) .gt. 0) go to 200
 
*---- Resume outer procedure, if any.
400     iq(laastk+1) = 0
        laastk = lq(laastk-1)
      if (laastk .ne. 0) go to 300
 
      end
