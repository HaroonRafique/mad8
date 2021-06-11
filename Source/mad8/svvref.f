      subroutine svvref(lvar)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save a variable reference.                                         *
* Input:                                                               *
*   LVAR(1)   (pointer) Pointer to the variable reference bank.        *
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
      integer ind1,ind2,ind3,ndim
      integer           lvar(1)
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
      integer mvattr,mvbank,mvbias,mvf1,mvf2,mvind1,mvind2,mvind3,
     +mvseen,mvsiz
 
*---- Bias for variable reference group.
      parameter         (mvf1   = 1,
     +                   mvbank = 2,
     +                   mvattr = mvbank + mwnam,
     +                   mvf2   = mvattr + mwnam,
     +                   mvseen = mvf2 + 1,
     +                   mvind1 = mvseen + 1,
     +                   mvind2 = mvind1 + 1,
     +                   mvind3 = mvind2 + 1,
     +                   mvbias = mvind3 + 1,
     +                   mvsiz = mvbias)
 
      character*(mcnam) name
 
*---- Bank name.
      call uhtoc(q(lvar(1)+mvbank), mcwrd, name, mcnam)
      call svname(name)
 
*---- Attribute name.
      if (iq(lvar(1)+mvseen) .eq. 2) then
        call svlitt('[')
        call uhtoc(q(lvar(1)+mvattr), mcwrd, name, mcnam)
        call svname(name)
 
*---- Set dimension count.
        ndim = 0
        ind1 = iq(lvar(1)+mvind1)
        ind2 = iq(lvar(1)+mvind2)
        ind3 = iq(lvar(1)+mvind3)
        if (ind1 .gt. 1) ndim = 1
        if (ind2 .gt. 1) ndim = 2
        if (ind3 .gt. 1) ndim = 3
 
*---- Indices, if present.
        if (ndim .ge. 1) then
          call svlitt('(')
          call svint(ind1)
          if (ndim .ge. 2) then
            call svlitt(',')
            call svint(ind2)
            if (ndim .ge. 3) then
              call svlitt(',')
              call svint(ind3)
            endif
          endif
          call svlitt(')')
        endif
        call svlitt(']')
      endif
 
      end
