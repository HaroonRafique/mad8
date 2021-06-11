      subroutine svname(name)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save a name for SAVE or VIEW command.                              *
*   Also treats labels with special characters.                        *
* Input:                                                               *
*   NAME      (char)    Name to be written.                            *
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
      integer ichar,index,js,ks,ls,mc,ns
      character*(mcnam) name
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
 
      logical           quotes
      character*1       c1
      parameter         (mc = 2 * mcnam)
      character*(mc)    string
 
*---- Normal name, or special character at start?
      quotes = ichtyp(ichar(name(1:1))) .ne. 10
      string(1:1) = name(1:1)
 
*---- Loop through characters of name, inserting escapes as needed.
      ls = 1
      ns = 1
      call utleng(name, ks)
      do 10 js = 2, ks
        c1 = name(js:js)
        if (ichtyp(ichar(c1)).gt.10 .and. index('.''_', c1).eq.0) then
          quotes = .true.
        endif
        ls = ls + 1
        string(ls:ls) = c1
        if (c1 .eq. '"') then
          ls = ls + 1
          string(ls:ls) = c1
        endif
        if (c1 .ne. ' ') ns = ls
   10 continue
 
*---- New line if near end - don't break within quotes.
      if (isvbuf + ns .gt. 76) call svcont
      if (quotes) then
        isvbuf = isvbuf + 1
        savbuf(isvbuf:isvbuf) = '"'
      endif
      savbuf(isvbuf+1:isvbuf+ns) = string(1:ns)
      isvbuf = isvbuf + ns
      if (quotes) then
        isvbuf = isvbuf + 1
        savbuf(isvbuf:isvbuf) = '"'
      endif
 
      end
