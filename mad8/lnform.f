      subroutine lnform(lline, label, lact, iahed)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fill in actual arguments for a beam line.                          *
* Input:                                                               *
*   LLINE(1)  (pointer) Beam line bank.                                *
*   LABEL     (char)    Name of called beam line.                      *
*   LACT(1)   (pointer) Bank containing actual arguments.              *
*   IAHED     (integer) Header of actual argument list.                *
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
      integer iact,iahed,icell,iform1,iform2,ileng,jform
      integer           lline(*), lact(*)
      character*(mcnam) label
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
 
*---- Formal argument list.
      iform1 = iq(lline(1)+mlf1)
      iform2 = iq(lline(1)+mlf2)
      if (iform1 .eq. 0  .and.  lact(1) .eq. 0) go to 9999
 
*---- Redundant actual argument list.
      call utleng(label, ileng)
      msg(1)(33:80) = 'entering "' // label(1:ileng) // '".'
      if (iform1 .eq. 0) then
        msg(1)(1:32) = 'Redundant actual argument list,'
        call aafail('LNFORM', 1, msg)
 
*---- Missing actual argument list.
      else if (lact(1) .eq. 0) then
        msg(1)(1:32) = 'Missing actual argument list,'
        call aafail('LNFORM', 1, msg)
 
*---- Header of actual argument list.
      else
        iact = iahed
 
*---- Loop for formals.
        do 90 jform = iform1, iform2, 2*mlsiz
          icell = jform + mlsiz
          iact = iq(lact(1)+iact+mlnxt)
 
*---- Header of actual argument list reached before formal list ends.
          if (iq(lact(1)+iact+mltyp) .eq. 1) then
            msg(1)(1:32) = 'Too few actual arguments seen,'
            call aafail('LNFORM', 1, msg)
            go to 9999
 
*---- Copy actual argument to formal argument.
*     If a sublist replaces a formal argument, mark its header.
          else if (iq(lact(1)+iact+mltyp) .eq. 4) then
            iq(lline(1)+icell+mltyp) = 5
            iq(lact(1)+iq(lact(1)+iact+mlref)+mltyp) = 3
          else
            iq(lline(1)+icell+mltyp) = iq(lact(1)+iact+mltyp)
          endif
          iq(lline(1)+icell+mlrep) = iq(lact(1)+iact+mlrep)
          iq(lline(1)+icell+mlref) = iq(lact(1)+iact+mlref)
          iq(lline(1)+icell+mlact) = iq(lact(1)+iact+mlact)
   90   continue
 
*---- Header of actual argument list not reached at end of formal list.
        iact = iq(lact(1)+iact+mlnxt)
        if (iq(lact(1)+iact+mltyp) .eq. 1) go to 9999
        msg(1)(1:32) = 'Too many actual arguments seen,'
        call aafail('LNFORM', 1, msg)
      endif
 
 9999 end
