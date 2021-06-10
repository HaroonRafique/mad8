      subroutine svhelp
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Help on command keywords.                                          *
* Attribute:                                                           *
*   KEYWORD   (name)    Keyword to be shown (blank: list all keywords).*
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
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
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
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer i,ikey,iln,ipr,isp,j,lbank,lkey,n,nkat,nkey
 
      character*(mcnam) key, name(8)
      character*12      kattyp(10)
 
      data kattyp
     +  / 'name',       'integer',    'real',       'deferred',
     +    'logical',    'string',     'beamline',   'range',
     +    'constraint', 'variable' /
 
*---- Retrieve keyword name.
      key = ' '
      call utgnam(lccmd, 1, 1, key)
 
*---- Blank keyword: list all known keywords in alphabetical order.
      if (key .eq. ' ') then
        write (iqlog, 910)
        n = 0
        do 10 i = 1, iq(ldkey(3)+1)
          j = iq(ldkey(1)+i)
          n = n + 1
          call diname(ldkey, j, name(n))
          if (n .ge. 8) then
            write (iqlog, 920) name
            n = 0
          endif
   10   continue
        write (iqlog, 920) (name(i), i = 1, n)
        write (iqlog, 930)
 
*---- Otherwise find keyword and show its attributes.
      else
        call utleng(key, nkey)
        call difind(ldkey, key(1:nkey), ikey, lkey)
        if (ikey .eq. 0) then
          write (iqlog, 940) key
        else
          call kwget(lkey, iln, ipr, isp, nkat)
          write (iqlog, 950) key
          write (iqlog, 960) (katnam(j), kattyp(iatype(j)),
     +      iadim1(j), iadim2(j), iadim3(j), j = 1, nkat)
          isave = iqlog
          write (iqlog, 970)
          lbank = lq(lkey-2)
          call svbank(lbank)
        endif
      endif
 
  910 format(' '/' Known commands:')
  920 format(4('  ',a16))
  930 format(' Type "HELP, command" for help about "command".'/' ')
  940 format(' '/' Keyword "',a,'" is unknown.'/
     +       ' Type "HELP" to list known commands.'/' ')
  950 format(' '/' Command: ',a/
     +       ' Attribute        type      dimension ',4x,
     +       ' Attribute        type      dimension ')
  960 format(' ',a16,' ',a10,'(',i2,',',i2,',',i2,')',
     +        5x,a16,' ',a10,'(',i2,',',i2,',',i2,')')
  970 format(' Default values used for attribute name without value:')
 
      end
