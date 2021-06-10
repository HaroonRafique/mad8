      subroutine sutfsf(idisk)
      implicit none
*----------------------------------------------------------------------*
* Purpose:
*     writes survey TFS out file header
* Input:  idisk (integer)  unit number
*----------------------------------------------------------------------*
      integer k, idisk, lastnb
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
      character * (68) form11, form12, form21, form22
      character * (160) form
      character * 8 tmp
      data form11, form12 /
     +'(''* NAME'',t21,''OCC'',t26,''S'',t44,''S_ARC'',t62,''X'',t80,',
     +'''Y'',t98,''Z'',t116,''THETA'',t134,''PHI'',t152,''PSI'')' /
      data form21, form22 /
     +'(''$ %16s'',t21,''%hd'',t26,''%e'',t44,''%e'',t62,''%e'',t80,',
     +'''%e'',t98,''%e'',t116,''%e'',t134,''%e'',t152,''%e'')' /
      form = form11 // form12
      write (idisk, form)
      form = form21 // form22
      write (idisk, form)
      write (idisk, '(''@ TYPE'',t21,''%08s "SURVEY"'')')
      write (idisk, '(''@ ORIGIN'',t21,''%07s "MAD-x"'')')
      write (idisk, '(''@ DATE'',t21,''%10s  "'',a,''"'')') cdate
      write (idisk, '(''@ TIME'',t21,''%10s  "'',a,''"'')') ctime
      tmp = '%  s'
      k = min(lastnb(ctitle)+2,80)
      write (tmp(2:3), '(i2.2)') k
      write (idisk, '(''@ COMMENT'',t21,a4, '' "'',a,''"'')')
     +tmp(:4), ctitle(:k)
      end
