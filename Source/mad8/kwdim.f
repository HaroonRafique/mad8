      subroutine kwdim(ikat, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode keyword attribute types and dimensions.                     *
* Input:                                                               *
*   IKAT     (integer)  Attribute number.                              *
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
      integer ikat,index,k,leng
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
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*(mcnam) atrnam
 
      eflag = .false.
 
*---- Skip open parenthesis "(" or comma ",".
  100 continue
        jtok = jtok + 1
 
*---- Data type.
        k = index('NIRDLSBPCV', token(jtok))
        if (k .ne. 0  .and.  index('(*,', token(jtok+1)) .ne. 0) then
          iatype(ikat) = k
          jtok = jtok + 1
 
*---- Dimensions.
          iadim1(ikat) = 1
          iadim2(ikat) = 1
          iadim3(ikat) = 1
          if (token(jtok) .eq. '*') then
            jtok = jtok + 1
            call rdint(iadim1(ikat), eflag)
          else if (token(jtok) .eq. '(') then
            call dcindx(iadim1(ikat), iadim2(ikat), iadim3(ikat), eflag)
          endif
          if (eflag) go to 200
 
*---- New attribute name.
        else if (token(jtok) .eq. ':') then
          jtok = jtok + 1
          call rdword(atrnam, leng)
          if (leng .eq. 0) then
            call rdfail('KWDIM', 1,
     +      'New attribute name expected after ":".')
            go to 200
          else
            katnam(ikat) = atrnam
          endif
 
*---- Anything else is assumed to be data.
        else if (token(jtok).ne.')' .and. token(jtok).ne.';') then
          call rdfind(',);')
        endif
        if (token(jtok) .eq. ',') go to 100
        if (token(jtok) .eq. ')') go to 300
        msg(1)='Character "' // token(jtok) // '" is not allowed here.'
        call rdfail('KWDIM', 1, msg)
 
*---- Error recovery.
  200   continue
        call rdfind(',;)')
        eflag = .true.
      go to 100
 
*---- Skip closing parenthesis ")".
  300 continue
      jtok = jtok + 1
 
      end
