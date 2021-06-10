      subroutine rdstat(eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read a statement into the statement buffer, skip emtpy statements. *
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
      integer ichar,index
      logical           eflag
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
      integer jtext,lintxt,ltext,ntext
 
*---- Input line buffer.
      parameter         (ltext = 80)
      common /lnbufc/   text(ltext)
      common /lnbufi/   lintxt, jtext, ntext
      save              /lnbufc/, /lnbufi/
      character*1       text
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*1       c, cp, q
      logical           ovrun
      character*1       bslash
      parameter         (bslash = '\')
 
*---- Read next line if finished with last one.
      eflag = .false.
      if (jtext .gt. ntext) call rdline
 
*---- Note starting line number.
      lintok = lintxt
      jtok = 1
      ntok = 0
      cp = ';'
 
*---- Copy statement to statement buffer.
  100 continue
 
*---- Get next character on current line.
        if (jtext .le. ntext) then
          c = ch2upp(ichar(text(jtext)))
          jtext = jtext + 1
        else
          c = ';'
        endif
 
*---- End of statement.
        if (c .eq. ';'  .or.  c .eq. '!') then
          if (ntok .ge. lentok) go to 800
          ntok = ntok + 1
          token(ntok) = ';'
          if (c .eq. '!') jtext = ntext + 1
          eflag = .false.
          go to 9999
 
*---- Continue on next line.
        else if (c .eq. '&') then
          call rdline
          go to 100
 
*---- Single or double quote.
        else if (c .eq. '"'  .or.  c .eq. '''') then
          if (ntok .ge. lentok) go to 800
 
*---- Copy single quote (prime) to name.
          if ((c .eq. '''')  .and.
     +        (ichtyp(ichar(cp)) .le. 10 .or.
     +         index('.''_', cp) .ne. 0)) then
            ntok = ntok + 1
            token(ntok) = c
            cp = c
 
*---- Copy opening quote to string.
          else
            ovrun = .false.
            q = c
            ntok = ntok + 1
            token(ntok) = c
 
*---- Copy string to statement buffer.
  200       continue
 
*---- Get next character.
              if (jtext .gt. ntext) then
                if (.not. ovrun) then
                  call rdwarn('RDSTAT', 1,
     +            'String overruns end of line.')
                  ovrun = .true.
                endif
                call rdline
              endif
              c = text(jtext)
              jtext = jtext + 1
 
*---- If quote, look for a second quote to continue string.
              if (c .eq. q) then
                if (ntok .ge. lentok) go to 800
                ntok = ntok + 1
                token(ntok) = c
                cp = c
                if (text(jtext) .ne. q) go to 100
                jtext = jtext + 1
              endif
 
*---- Move character to string.
              if (ntok .ge. lentok) go to 800
              ntok = ntok + 1
              token(ntok) = c
            go to 200
          endif
 
*---- Remove string of blanks.
        else if (c .eq. ' ') then
  300     if (jtext .le. ntext) then
            c = text(jtext)
            jtext = jtext + 1
            if (c .eq. ' ') go to 300
            if (c .eq. '&') then
              call rdline
              go to 300
            endif
            jtext = jtext - 1
          endif
 
*---- Replace blank string by comma.
          if ((ichtyp(ichar(cp)) .le. 10 .or.
     +         index('.''"@$)]',cp) .ne. 0)  .and.
     +        (ichtyp(ichar(c)) .le. 10 .or.
     +         index('.''"@$([#',c) .ne. 0)) then
            if (ntok .ge. lentok) go to 800
            ntok = ntok + 1
            cp = ','
            token(ntok) = ','
          else
            cp = ' '
          endif
 
*---- Any other character goes to statement buffer.
        else
          if (ntok .ge. lentok) go to 800
          ntok = ntok + 1
          token(ntok) = c
          cp = c
        endif
      go to 100
 
*---- Statement buffer overflow.
  800 continue
      write (msg, 910) lentok
  910 format('Statement too long for input buffer, available length = ',
     +       i5)
      call rdfail('RDSTAT', 1, msg)
      token(ntok) = ';'
 
 9999 end
