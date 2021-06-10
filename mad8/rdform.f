      subroutine rdform(ip1, ip2, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Mark positions of balanced parentheses.                            *
* Output:                                                              *
*   IP1      (integer)  position of open parenthesis.                  *
*   IP2      (integer)  position of closing parenthesis.               *
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
      integer ip1,ip2,level
      logical           eflag
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*1       c1, c2
 
      eflag = .false.
      ip1 = 0
 
*---- Care about ( ), [ ], or ;.
      if (token(jtok) .eq. '(') then
        c1 = '('
        c2 = ')'
      else if (token(jtok) .eq. '[') then
        c1 = '['
        c2 = ']'
      else
        go to 9999
      endif
      level = 1
      ip1 = jtok
 
*---- Loop until closing bracket found.
  100 continue
        jtok = jtok + 1
        if (token(jtok) .eq. c2) then
          level = level - 1
          if (level .le. 0) go to 200
        else if (token(jtok) .eq. c1) then
          level = level + 1
        else if (token(jtok) .eq. ';') then
          go to 200
        endif
      if (token(jtok) .ne. ';') go to 100
 
*---- Check for balanced brackets.
  200 continue
      if (level .ne. 0) then
        call rdfail('RDFORM', 1, 'Unbalanced parentheses or brackets.')
        eflag = .true.
      else
        ip2 = jtok
        jtok = jtok + 1
        eflag = .false.
      endif
 
 9999 end
