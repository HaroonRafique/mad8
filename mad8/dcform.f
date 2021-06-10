      subroutine dcform(lline, if1, if2, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode formal argument list.                                       *
*   The formals bank is linked to link 1 of the current command.       *
* Input:                                                               *
*   LLINE(1) (pointer)  Beam line list bank.                           *
*   IF1, IF2 (integer)  First and last character of formal list.       *
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
      integer if1,if2,ileng,jform,jmark,jt,leng,nd,nform
      integer           lline(1)
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
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*(mcnam) frmnam, oldnam
      logical           sflag
 
*---- Any argument list?
      ldclin = lline(1)
      eflag = .false.
      if (if1 .ne. 0  .and.  token(if1) .eq. '(') then
 
*---- Count formals names.
        nform = 1
        do 10 jt = if1, if2
          if (token(jt) .eq. ',') nform = nform + 1
   10   continue
 
*---- Lift bank for formals names.
        nd = nform * mwnam
        call mzbook(2, ldcfrm, ldclin, -1, 'FORM', 0, 0, nd, 5, 0)
        nform = 0
        jmark = jtok
        jtok = if1
 
*---- Argument name.
  100   continue
          jtok = jtok + 1
          sflag = .false.
          call rdword(frmnam, leng)
          if (leng .eq. 0) then
            call rdfail('DCFORM', 1, 'Formal argument name expected.')
            sflag = .true.
 
*---- Test for correct delimiter.
          else if (token(jtok).ne.',' .and. token(jtok).ne.')') then
            call rdfail('DCFORM', 1, '"," or ")" expected.')
            sflag = .true.
 
*---- Reject duplicate arguments.
          else
            do 110 jform = 1, nform, mwnam
              call uhtoc(q(ldcfrm+jform), mcwrd, oldnam, mcnam)
              if (oldnam .eq. frmnam) then
                call utleng(frmnam, ileng)
                msg(1) = 'Duplicate formal argument "'// frmnam(1:ileng)
     +          // '" encountered.'
                call rdfail('DCFORM', 1, msg)
                sflag = .true.
                go to 120
              endif
  110       continue
 
*---- Add formal name to list.
            call uctoh(frmnam, iq(ldcfrm+nform+1), mcwrd, mcnam)
            nform = nform + mwnam
  120       continue
          endif
 
*---- Error recovery.
          if (sflag) then
            call rdfind(',);')
            eflag = .true.
          endif
        if (token(jtok) .eq. ',') go to 100
 
*---- Restore place in input statement.
        jtok = jmark
      endif
 
      end
