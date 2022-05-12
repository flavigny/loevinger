c 2 do apersl
c voir mermeture txt
c gaffe a dualite irepcol et ivalq
c 20190203 ok code 10 en csv 
c verifier q x bug pas controle mq pour seconde csv ???
c dict sans label lisgte ligne 1
c petit bégayement sur log ouvert et estla?
c voir comment et finish
c voir testun invite bof
c 20180124 le debordement de 80 n'est détecté qu si non vide, passer en 5000!
c protection debordement
c repren dre par (1) idiot si pas démarr&é
c ligne 2804 san s doute imp inter de trop
c SKIP EN FAUTE
c numéroter le s .txt.txt
c reprise tenter d'ab lect binaire
c bug col 1 de traditi
c ajouter len a accent
c double inter impertinent
c si multiple nom mqu mx
c indiquer ligne en =erreur
c voir interligne
c on d&m&rre en MQMT
c mot vide!
c compte colofa8nnes identiques
c ti 0
c imp 66 seulement si ouvert
c ê ˆ
c é ‚‚
c à 
c 87 imp
c si echec ouverture dsnlog ne pas dire trace
c vide fichier .txt.txt
c simple read fait erreur
c si so k loupé ne pas dire reprendre!
c VOIR NOM A 1 LETRE ?????
c GAFFE: si on repren un echele dasn rien changer termine par un reprendre impertinent
c affectation desz untées:
c  0 dirige vers stderr
c  * (ou 6) dirige vers stdout
c  actuellement sortie sur * (quand nulo=0) ou fichier 
c nom de fichier  >>* à voir
C     Last change:  JC   13 Jul 2012   10:29 am
c si batch ecrit frappes
c reponse = si pas propose bug
      subroutine goodbye(nap)
	  character*80 ti
	  
	  common /io/ mae,mal,lca,imp
       character*4 byedsn
	   common /bye/ byedsn
	
    	character*80 ctitre
	   character*8 cdate,cche
	   logical fsaved
	   character*96 dsnlog
      common /chetit/ ctitre,cche,cdate,fsaved,dsnlog
	
    	character*8 ici
	  integer *4 la(2)
      equivalence(ici,la)
	  character*1 acmd
      write(*,*) 'fin de travail: '
	  write(*,*)ctitre
	  la(1)=0
	  la(2)=0
	  ici(1:1)=cche(1:1)
c	  write(*,*) 'cche ',la(1),' ',la(2)
c plyus de mesage reprendrepar
       if(.false.)      then
	  if (fsaved .and. la(1).ne.0 .and. nap.eq.0) then
c		  if (byedsn(1:1).eq.' ') byedsn(1:1)='.'
	      write(*,*) 'reprendre par:'
	      write(imp,*) 'reprendre par:'
		  idep=1
76		  continue
          ici(1:1)=cche(idep:idep)
		  if (la(1).ne.32) goto 77
		  idep=idep+1
		  if (idep.lt.9) goto 76
77	      continue
      write(*,*)'loevingr:chemin=',cche(idep:8),',rappel=',byedsn,';'
	  endif
	  endif
c si dessus ensdif du .false.
	  if (dsnlog.ne.'') then
	  ici(1:1)=dsnlog(1:1)
      if (la(1).ne.0) write(*,*) 'trace dans le fichier: "',
     1 trim(dsnlog),'"'
	  endif
	  call stamp(0,0)
      if (imp.ne.0) then	  
        write(imp,*) 'fin de travail: '
		write(imp,*) ctitre
	    ici(1:1)=cche(1:1) 
c		write(*,*) la(1),' ' , la(2)
         
	    if (fsaved ) then
		  write(imp,*) 'reprendre (1) par: '
      n=imp		  
	  if (byedsn.eq.'') then
	    write(*,*) 'pas de abc, forcer .'
	    byedsn='.'
		write(*,*)'. force'
	   endif
	  write(n,*)'loevingr:chemin=',cche(idep:8),',rappel=',byedsn,';'
      else
	   if (n.eq.66)write(n,*)'dernier état non sauvegardé'
	   write(*,*)'dernier etat non sauvegard3e'
c 
	  endif	  
        write(imp,*) 'trace enregistrée dans le fichier: ',dsnlog
        call stamp(imp,0)
	  endif
	  if (nap.eq.0)then
      stop 
      else  
	  stop 1
      endif
      
	  end
	   subroutine pose(nappel)
      common /io/ mae,mal,lca,imp
      if (nappel.ne.0) then
	    write(*,*)'Fin brutale au point:',nappel
	   if (imp.eq.66)write(imp,*)'Fin brutale au point:',nappel
	   endif
c	  read(*,*)
	  call goodbye (nappel)
      end
      
      
      subroutine upcas1(c1)
      common /io/ mae,mal,lca,imp
      character * 1 c1
      character * 3 c3
      if (c1 .ge. 'a' .and. c1 .le. 'z')
     1 c1=char ( ichar(c1) - ichar('a') + ichar('A') )
      return
      end

      function aplmn(vf)
      character * 1 aplmn
      logical vf
      aplmn='+'
      if (.not. vf) aplmn='-'
      return
      end


      SUBROUTINE IMPTAB(nbq,nitp,NBIT,PP,LUIT,IAIT,IPOP,NBI,nmi,iqorit)
	  common /controlist/ partil,absents,listqabsentes,listqpresentes
	

    	logical partil,absents,listqabsentes,listqpresentes
      common /presents/  qq
      integer*4 qq(100)	 
      
      logical fsaved
      character*4 byedsn
	  	   common /bye/ byedsn
      logical inter2,col5,large
	  common /inter2/ inter2,col5,large,ls

	   character*96 dsnlog
      common /chetit/ ctitre,cche,cdate,fsaved,dsnlog
      character*4 chr12 
	  integer *4 int12 /12/
	  equivalence (int12,chr12)
      character * 80 ctitre
      character * 8 cdate,cche
      LOGICAL  IAIT (1) 
	  DIMENSION ILIGNE(50)  ,PP(1) ,iqorit(100)
      INTEGER * 2 PP
      CHARACTER * 4 ITEXT
      DIMENSION ITEXT(11)
      dimension nmi(nbit)
      dimension ncopr(100)
      common /io/ mae,mal,lca,imp
	  character * 80 f50,f9
	  character*1 slash	 
	  DATA ITEXT/'N IJ','C IJ','W IJ','H IJ','++  ',
     1 'I 01','I 05','I 10','S 01','S 05', 'S 10'/
      NUIT=MOD(LUIT,100)
c nombre de colonnes de la matrice calculé plus bas
c      write(*,*)'imptab',luit,ctitre
      IP=0
c                                 boucle sur les l‚s

c formats d'écriute
      if (inter2)then
	   slash='/'
	   else
	   slash=' '
	   endif
      if (col5) then
	  nbco=(ls-5)/5
      f50='('//slash//'I3,'':'',40I5)'
	  f9='(/''    '',40(I4,a1))'

	  else 
	   f50='('//slash//'I3,'':'',50I4)'
	  f9='(/''    '',50(I3,a1))'
	  nbco=(ls-5)/4

      endif
c      write(*,*)'f50=',f50	  
c      write(*,*)'f9 =',f9
	  if (nbco.gt.50.or.nbco.lt.0) then
	   write(*,*)'trop de colonnes, mis 50 ',ls, nbco
	    nbco=50
		endif
c       write(*,*)ls,nbco
	  if (inter2) then
	    nbli=25
		else
		nbli=50
	 endif	
	 
c     si partil compter les colonnes presentes
      nbitpr=0
      do 7 ibit=1,nbit
c contrute le vecteur des numéros itemps présents	  
c ABSENTS
c      write(*,*)'(1)appel ilfaut nbq=',nbq
	  if (ilfaut(nbq,ibit,nbit,iait,iqorit,qq,numeit).eq.0)goto 7
      nbitpr=nbitpr+1
      ncopr(nbitpr)=ibit
7     continue

c      write(*,*)'nbitpr=',nbitpr,' tab=',nuit,' nbco=',nbco
	  if (nbitpr.le.1) then
	    write(*,*)'moins de 2 items dans le tableau, rien à voir'
		return
		endif
      DO 1 JCOG=1,NBITpr,NBCO

c                                 boucle sur les colonnes
      DO 2 JLiiH=JCOG,NBITpr,NBLI
c ruse pour redefinit jlih plus bas!
	   jlih=jliih

      
c                                  JLIH numero ligne haut de le
      JLIB=MIN0(NBITpr,JLIH+NBLI-1)
c                                  JLIB numero bas de le
      JCOE=MIN0(NBITpr,JCOG+NBCO-1)
c                                   JCOE nu colonnne end (droite)
      IP=IP+1
c                                   Ip numero de page
c	   write(*,*)' dans imptab titre=',titre
c	   write(*,*)' dans imptab idd=',idd
c       write(*,*)' dans imptab cdate=',cdate

       CALL STPA(4,IPA,IDD,imp)
c      DATA ITEXT/'N IJ','C IJ','W IJ','H IJ','++  ',
c     1 'I 01','I 05','I 10','S 01','S 05', 'S 10'/

      write(*, 8) ' ',ITEXT(NUIT) ,IP,nmi(ncopr(JCOG)),
     1 nmi(ncopr(JCOE)),
     1 nmi(ncopr(JLIH)),nmi(ncopr(JLIB))
      write(imp, 8) ' ',ITEXT(NUIT) ,IP,nmi(ncopr(JCOG)),
     1 nmi(ncopr(JCOE)),
     1 nmi(ncopr(JLIH)),nmi(ncopr(JLIB))
8     FORMAT(A,'TABLEAU ',A8,24x,'PAGE ',I3,I9,' A ',I3,'  PAR  ' ,I3,' A
     1 ',I3)
      write(*,*) ctitre
      write(imp,*) ctitre
	  call stamp(imp,0)
      call direqui(nbit,nitp)
	      write(*,*) 'chemin=',cche,'   date=',cdate
c      write(imp,*) 'chemin=',cche,'   date=',cdate
      write(*, f9) (nmi(ncopr(JJ)),':',JJ=JCOG,JCOE)
      write(imp, f9) (nmi(ncopr(JJ)),':',JJ=JCOG,JCOE)
c9     FORMAT(//'     ',25(I4,a1))
c             pour N on a la diagonale 1,1 sinon on party a 2
      IF(JLIH.EQ.1.AND.NUIT.NE.1)JLIH=2

      DO 3 JLI=JLIH,JLIB
c	  write(*,*)'do 3 jli=',jli
      JCOD= MIN0(JCOE,JLI)
      IF (NUIT .NE. 1 .and. jcod .eq. jli)JCOD=JCOD-1
C MODIF CI DESSUS 8 JUILLET MEME POUR LIGNE DU BAS
c                        nb de nombres sur cette ligne
      NBNB=JCOD-JCOG+1

      DO 33 JCO=JCOG,JCOD
      JJ=JCO-JCOG+1
      IFROM=(ncopr(JLI)*(ncopr(JLI)-1)     ) /2+ncopr(JCO)
      JLIFR=ncopr(JLI)*(ncopr(JLI)+1)/2
      JCOFR=ncopr(JCO)*(ncopr(JCO)+1)/2
C    1  =>   NIJ     2=> C IJ      3=> W IJ     4=> H IJ   5 ==> n++
c juillet 97 ajout implications
c
      MARG1=PP(JLIFR)
      MARG2=PP(JCOFR)
      MIN=MIN0(MARG1,MARG2)
      MAX=MAX0(MARG1,MARG2)
      EWIJ= djvi(MIN*(NBI-MAX),NBI)
c ewij est le contenu theorique en effectif
      WIJ=100.* djvi(MIN*(NBI-MAX),NBI*NBI)
      NIJ=PP(IFROM)
c	  write(*,*)jli,'*',jco,'ifrom:',ifrom,':',nij
      CIJ=(100.*NIJ)/NBI
      hij=100.49-div(100.*CIJ,WIJ )
      if (iqorit(ncopr(jli)).eq.iqorit(ncopr(jco)))  then
                    nij=0
                    wij=0
                    cij=0
                    hij=0
                                                     endif
      GO TO (   11,12,13,14,15,1000,1100,1200,2000,2100,2200),NUIT
      write(*,*) 'demande de tableau doit ˆtre compris entre 1 et 11'
      write(imp,*) 'demande de tableau doit ˆtre compris entre 1 et 11'
      write(*,*) ' je trouve nuit=',nuit
      write(imp,*) ' je trouve nuit=',nuit
      call pose(101)
c  nuit=6
1000  continue
      seuil=2.58
      goto 1900
c  nuit=7
1100  continue
      seuil=1.96
      goto 1900
c  nuit=8
1200  continue
      seuil=1.65
      goto 1900
1900  continue
c ----------------------- Implicatio .01
      SQ1=sqrt(( nij * (NBI - nij) ) / (1.0*nbi) )
      xNB2 =  Nij + seuil * SQ1
      txi= 1.0 - ( xNB2 / ewij    )
      iligne(jj)=100.0 * txi
c    1   'nbi=',nbi, '  nij=',nij,'  sq1=',sq1,' nb2=',nb2,
c      write(*,*) ' nb2=',nb2,  '  wij=',wij ,'  txi=',txi
c      read(*,*)
c      write(*,*) 'nb2=',nb2, '  txi=',txi
      goto 30
c  nuit=9
2000  continue
      seuil=2.58
      goto 2900
c  nuit=10
2100  continue
      seuil=1.96
      goto 2900 
c  nuit=11
2200  continue
      seuil=1.65
      goto 2900
c regroupe,t apres seuil
2900  continue
c ----------------------- seuils .01
      SQ1=sqrt(( nij * (NBI - nij) ) / (1.0*nbi) )
      xNB2 =  seuil * SQ1
      nb2=xnb2
c      write(*,*) 'nij=',nij,' nbi=',nbi,
c     1 'sq1=',sq1,' xnb2=',xnb2,' ewij=',ewij
      txi=  xNB2 / ewij   
      iligne(jj)=100.0 * txi
c    1   'nbi=',nbi, '  nij=',nij,'  sq1=',sq1,' nb2=',nb2,
c      write(*,*) ' nb2=',nb2,  '  wij=',wij ,'  txi=',txi
c      read(*,*)
c      write(*,*) 'nb2=',nb2, '  txi=',txi
      goto 30
c  nuit=1
11    ILIGNE(JJ)=nij
      GO TO 30
c  nuit=2
12    ILIGNE(JJ)=CIJ+0.5
      GO TO 30
c  nuit=3
13    ILIGNE(JJ)= WIJ+0.5
      GO TO 30
c  nuit=4
14     iligne(jj)=hij
      goto 30
c  nuit=5
15    CONTINUE
c case ++
      iligne(jj)=min-pp(ifrom)
      goto 30
30    continue

33    CONTINUE
c       write(*,*)'(2)appel ilfaut nbq=',nbq,iqorit(ibit)
c      function ilfaut(nbq,i,nbit,iait,iqorit,qq,numeit)
      ioui= ilfaut(nbq,ncopr(jli),nbit,iait,iqorit,qq,numeit)
	  
	  if (ioui.gt.0 .and. NBNB .GT.0) then
c	    write(*,*)'f50:',f50
c		write(*,*)'123456789 1234567'
       write(*,f50) nmi(ncopr(JLI)),(ILIGNE(JJ),JJ=1,NBNB)
       write(imp,f50) nmi(ncopr(JLI)),(ILIGNE(JJ),JJ=1,NBNB)
	   else
	   write(*,*)'pas ligne',jli,ioui,nbnb
	   endif
	   
C IMPRIMERN LIGNES NON VIDES SEULEMENT 8 JUILLET 86
3     CONTINUE

2     continue
      write(*, f9) (nmi(ncopr(JJ)),':',JJ=JCOG,JCOE)
      write(imp, f9) (nmi(ncopr(JJ)),':',JJ=JCOG,JCOE)
1     CONTINUE
c pas de saut de page sur console!
c      write(*, 4444 ) char(12)
      write(imp, 4444 ) char(12)
4444     FORMAT(a1)
       RETURN
      END
c ....................................................
c                                          prochaine ligne ?
      subroutine testun(nappel,lectur,mot,invite)
      character * 80 invite
      logical deja,eof
      character *127 bb127
      character * 1 luavan(127)
       common /ligne/ eof,deja,luavan
      common /io/ mae,mal,lca,imp
      character * 8 mot,trav
      character * 1 mot1(8)
      i80=80
c	   write(*,*) 'testun ',nappel,deja
       if (deja) goto 500
c	    write(*,*)'testun charge luavan'
       read(invite,'(80A1)') (luavan(jj),jj=1,80)
	   do 5018 jj127=81,127
5018   luavan(jj127)=' '
       do 501 jfin=1,80
       if (luavan(jfin).eq. ';') goto 502
501    continue
       jfin=80
502    continue
       if (lectur .ne. 9 .and. lectur.ne.0) goto 500
 	   write(*,*) 'trace car lectur=',lectur
       write(*,*) (luavan(jj),jj=1,jfin-1)
       write(imp,*) (luavan(jj),jj=1,jfin-1)
c       write(*,*) 'saq dout ligne vide'
       write(imp,*)
500    continue

1      if (.not. deja) then
c         write(*,*)'(2)va lire sur lectur=',lectur
        if (lectur.eq.0) then
    	read(*,'(a)',end=10092 ) bb127
	    else
		read(lectur,'(a)',end=10093 ) bb127
		endif
c 20180125 tester longueur lue > 80
         if (len_trim(bb127).gt.80) then
		 
		    if (lectur.ne.0) then
			write(*,*)'un!'
			   write(*,*)trim(bb127)
			   write(imp,*)trim(bb127)
			   write(*,*)(' ',jjbl=1,79),'!'
			   write(imp,*)(' ',jjbl=1,79),'!'
			   write(*,*) 'ligne de plus de 80 caracteres'
			   write(imp,*) 'ligne de plus de 80 caractères'
			   call pose(633)
			   
			endif
		  endif
		  endif
       if (.not.deja) read(bb127,'(127a1)')(luavan(j),j=1,i80)
	  ilastnb=0
      if (deja) then
c          write(*,*) 'en anticipation on lit:'
		else  
c       write(*,*) 'en anticipation on reexamine:'
	   endif
	   ilastnb=0
	   ipremnb=0
	  do 88 jrc=1,80
	     if ((ipremnb.eq.0) .and. (luavan(jrc).gt.' ')) ipremnb=jrc
	     if (luavan(jrc).gt.' ') ilastnb=jrc
         if (luavan(jrc).lt.' ') write (*,*)'?',ichar(luavan(jrc))
88    continue	  
c	  write(*,*) 'en 88 ilastnb=',ilastnb 
c     eliminer si * en col 1
      if (ilastnb.eq.0 .or. luavan(ipremnb) .eq. '*') then
	     write(*,'(1x,127a1)') (luavan(j),j=1,ilastnb)
         write(imp,'(1x,127a1)') (luavan(j),j=1,ilastnb)
         goto 1
	  endif	 
	  if (.false.) then
	  write(*,*)'testun courant'
      write(*,'(1x,127a1)') (luavan(j),j=1,ilastnb)
c	  write(*,*) 'on a ‚crit 127'
      write(imp,'(1x,127a1)') (luavan(j),j=1,ilastnb)
c      write(*,*) '-----'
      endif
      do 20 j=1,8
20    mot1(j)=' '
      do 40 jdep=1,i80
      if (luavan(jdep) .ne. ' ') goto 50
40    continue
50    continue
      do 10 j=jdep,jdep+8
      if (luavan(j).eq.':') goto 30
      if (j .lt. jdep+8) goto 11
      write(*,*) '… j=',j
      write(imp,*) '(1)à j=',j
cc     if (.not. deja) write(*,*) 'en anticipation on lit:'
c      if (.not. deja) write(imp,*) 'en anticipation on lit:'
c      if ( deja) write(*,*) 'en anticipation on reexamine:'
c      if ( deja) write(imp,*) 'en anticipation on reexamine:'
      write(*,'(1x,127a1)') (luavan(jj),jj=1,i80)
      write(imp,'(1x,127a1)') (luavan(jj),jj=1,i80)
      write(*,*)' en anticipation plus de 8 caracteres ci dessus'
      write(imp,*)' en anticipation plus de 8 caracteres ci dessus'
      call pose(1)
11    continue
      mot1(j-jdep+1)= luavan(j)
      if (mot1(j-jdep+1).ge.'a' .and. mot1(j-jdep+1) .le.'z')
     1  mot1(j-jdep+1)=char(ichar(mot1(j-jdep+1))-ichar('a')+ichar('A'))
10    continue
c              on ne passe pas ici
30    continue
      write(trav,'(8a1)') (mot1(j),j=1,8)
      read(trav,'(a8)') mot
      deja=.true.
c	  write(*,*)' testun reepond ',mot
      return
10091 write(*,*)'10091'
	  goto 1009
10092 write(*,*)'10092'
	  goto 1009
10093 continue
      write(*,*)'fin de fichier en lecture sur: ',lectur
      write(imp,*)'fin de fichier en lecture sur: ',lectur
	  goto 1009
1009  continue
c      write(*,*) 'testun ',nappel,' fin de fichier repond /* '
c      write(imp,*) 'fin de fichier en testun repond /* '
      mot='/*      '
      eof=.true.
       lectur=mae
c      write(*,*)' testun repond vide  ',mot
      return
      end
      
      subroutine anal(nulo,string,lstring,trep,arep,idim,fini,oblig)
	  	  common /io/ mae,mal,lca,imp

      character * 127 string,s80
      character * 1 lu80(127),seg,t1verb(8),t1mot(8)
      logical nu,err,fini,aval,oblig,yaurep,avalnu
      character * 8 verb,tmot(20),verbr,a8va
      character * 8 arep(idim)
      dimension trep(20) ,aval(20),avalnu(20)  ,yaurep(20)
      logical servi(20)
c	  write(*,*) 'PO: anal nulo: ' , nulo,' ',string(1:lstring)
      n80=127
      nmot=0
      do 1090 j=1,idim
c initialiser les réponses
      yaurep(j)= .false.
      arep(j)='        '
1090  trep(j)=0.
c      write(*,*) 'on va a proto'
      call proto(nulo,string,lu80,verb,ipos)
c	  write(*,*)'retour proto :',lu80
10    continue
      call avance(lu80,ipos,tmot(nmot+1),seg,lm,nu,va,a8va)
c	  write(*,*)'retour de avance seg=',seg,' nmot+1=',nmot+1,
c     1 tmot(nmot+1),va,a8va

c intercepter val numerique
      if (nu .and. (nmot.gt.1) .and. avalnu(nmot)
     2 .and. .not. servi(nmot) ) goto 101
	 
      if ((nmot.gt.1) .and. aval(nmot) .and.
     2  .not. servi(nmot) ) goto 1019

	 if (nu .and. lm .gt. 0) then 
	  write(*,*)
     1 'erreur probable (mot cle numerique) '
      write(imp,*)
     1 'erreur probable (mot cle numerique) '
	  endif
C si =, la longueur du mot est 0
      if (lm .le. 0 .and. aval(nmot) .and. trep(nmot) .eq. 0.)
     1  goto 1023
c      write(*,*) 'un nouveau mot nmot+1::',nmot+1,tmot(nmot+1)
      goto 102
1019   continue
c                          ranger la reponse alpha
      servi(nmot)=.true.
      goto 1023
101   continue
c                          ranger la reponse numerique
      trep(nmot)=va
      servi(nmot)=.true.
      goto 1023
c                    1023 au lieu de 1010
102   continue
      aval(nmot+1)= (seg .eq. '=') .or. (seg .eq. ':')
c	  write(*,*) 'avaln(',nmot+1,') mis a  ',aval(nmot+1)
      avalnu(nmot+1)=seg .eq. '='
c	  write(*,*) 'avalnu(',nmot+1,') mis a  ',avalnu(nmot+1)
1010  continue
      nmot=nmot+1
      servi(nmot)=.false.
c     write(*,*) 'nmot mis ™ ',nmot
1023   if (seg .eq. ',' .or. seg .eq. '=' .or. seg .eq. ':') goto 10
       if (seg .ne. ';')
     1  call messag (800, 'erreur dans le prototype !',ipos)
c	  write(*,*)'on a trouve seg=point virgule'
      nuint=0
c      write(*,*) 'fin de decodage nmot=',nmot,' va lire'
20    call lire(nuint,nulo,lu80,127,verb,fini,oblig,ipos)
      if (fini .and. .not. oblig) return
c mettre ici le yaurep a zero
      do 10909 j=1,idim
      yaurep(j)= .false.
10909 continue
c     write(*,*) 'mis a false de yaurep sur ',idim
c prendre une re'ponse et analyser
26    continue
      call avance (lu80,ipos,verbr,seg,lm,nu,va,a8va)
c       write(*,*) 'retour de avance ',verbr,' num:',va,
c     1 ' alpha:',a8va
      if (lm .eq. 0 .and. ipos .gt. n80) goto 20
      if (verbr .eq. '        ' .and. seg.eq.';') return
c
c on a trouv‚ un mot voir si il satisfait un max 
      maxitr=0
      maxlon=0
      maxnb=0
      do 40 i=1,nmot
c      write(*,*) 'comparons ',verbr,'?',tmot(i)
c
c     priorite si ‚galit‚
      if (verbr .eq. tmot(i)) goto 41
      read(verbr,'(8a1)') (t1verb(j1),j1=1,8)
      read(tmot(i),'(8a1)') (t1mot(j1),j1=1,8)
      do 4040 j1=1,8
      if (t1verb(j1) .ne. t1mot(j1)) goto 4041
4040  continue
      j1=9
4041  j1=j1-1
c             les modele et essai ont j1 caracteres identiq.
      if (j1 .lt. maxlon) goto 40
c                                 on avait mieux 
c          
      if (j1.eq. maxlon) goto 4042
      
c                                   mieux
      maxlon=j1
      maxitr=i
      maxnb=1
      goto 40
      
c 
4042   continue
      maxnb=maxnb+1
      
40    continue

c     on sort sans egalit‚ parfaite
c   1) si ambiguite ca ne va pas
      if (maxnb .gt. 1) goto 4049
      if (maxitr .eq. 0) goto 4049
      
c      on garde une forme abregee
      i=maxitr
      goto 41
      
4049  continue      
c      write(*,*)'en  4049 imp=',imp
      if(len_trim(verbr).eq.0) then
      call messag (8140, 'deux separateurs contigus.!',ipos) 
	  call pose(814)
	  else
      call messag (-815, 'unwanted keyword !',ipos)
      write(*,*) 'liste des ',nmot,' mots clefs possibles'
      write(imp,*) 'liste des ',nmot,' mots clefs possibles'
      do 40999 i=1,nmot
      write(*,*) i,': ',tmot(i)
      write(imp,*) i,': ',tmot(i)
40999    continue
      write(*,*) 'mot-clef trouve: "',trim(verbr),seg,'"'
      write(imp,*) 'mot-clef trouvè: "',trim(verbr),seg,'"'
 	  call pose(815)
	  endif
c voir si suivi de valeur
41    continue
c      write(*,*) 'un mot:',verbr,i,seg,aval(i)
      if ( (seg .eq. '=') .and. aval(i) ) then
c	     write(*,*)'goto 501'
		 goto 501
	  endif	 
      if ( (seg .ne. '=') .and. .not. aval(i)) goto 502
      if (aval(i) ) write(*,*) i,'value keyword, missing =! sep=',seg
      if (.not. aval(i) ) call messag
     1 (801,'Boolean keyword, = forbidden! ',ipos)
502   trep(i)=1.
      if (yaurep(i) ) call messag(101,'repeated keyword !',ipos)
      yaurep(i)=.true.
      goto 50
501   continue
c     write(*,*) 'usage du mot ',i,' suivi de = yaurep',yaurep(i)
      if (yaurep(i) ) call messag(1,'multiple affectation !',ipos)
      call avance(lu80,ipos,verbr,seg,lm,nu,va,a8va)
c      write(*,*) i,' affectation valeur lu numerique=',nu,' ',a8va
      if (avalnu(i)) goto 50199
      arep(i)=a8va
c      write(*,*) arep(i),'=',a8va
      yaurep(i)=.true.
      goto 50
50199 continue
      if (.not. nu )
     1 write(*,*) 'Non numeric value after = sign (NAME=num)!'
      if (.not. nu )
     1 write(imp,*) 'Non numeric value after = sign (NAME=num)!'
      if (.not. nu) call pose(2)
      if (yaurep(i) ) call messag(20,'multiple affectation !',ipos)
      trep(i) = va
	  yaurep(i)=.true.
c      write(*,*) 'val nu mise a true de yaurep(',i
50    continue
c      write(*,*)' en 50 seg=',seg
      if (seg .eq. ',') goto 26
      if (seg .ne. ';') then
      write(*,*)ipos,'seg:',seg
	  call messag(2, 'lllegal separator/ missing semi-colon !',ipos)
	  endif
      return
      end
C ......................................................................
c chercher verbe en debut
      subroutine proto(lec,st,ta,ve,ipos)
      character * 127 st , s127
      character * 1 ta(127),seg,tb(127)
      character * 8 ve,a8va
      common /io/ mae,mal,lca,imp
      ipos=0
      s127= ' '
      n80=127
      write(s127,'(A127)') st
      read(s127,'(127A1)') (ta(i),i=1,n80)
C chercher le ; et imprimer invite
      do 1070 j=1,n80
      if (ta(j) .eq. ';') goto 1071
1070  continue
      write(*,*)' proto st="',st,'"'
      write(*,*) '"',st,'"'
	  
      write(imp,*) st
      call messag
     1 (804,'pas de ; dans les',n80,' premiers caracteres ...',n80)
1071  continue
c      write(*,*) 'on a passe 1071 j=',j
c       write(*,*) 'prototype long=',j,'!!!',(ta(jj),jj=1,j),'!!!'
c ci dessous * au lieu de 0
      ndeuxp=0
      do 1051 jj=1,j
      if (ta(jj).eq. ':') ndeuxp=ndeuxp+1
      tb(jj)=ta(jj)
      if (ta(jj).eq. ':' .and. ndeuxp.gt.1)tb(jj)='='
1051  continue
c      0 ci dessous
c      write(*,*) 'en cours'
      if (lec.eq.mae) write(*,'(1x,127A1)') (tb(i),i=1,j)
      if (lec.eq.mae) write(imp,'(1x,127A1)') (tb(i),i=1,j)
        call avance(ta,ipos,ve,seg,lv,nu,va,a8va)
c		 write(*,*)'revenu de avance seg=',seg
      if (seg .ne. ':') then 
	     write(*,*)' a ipos=',ipos,' seg=',seg
	     call messag(3, 'pas deux points !' ,ipos)
		 endif
      return
      end
C .....................................................................
c lecture d'une liste TITRE: Item: ...
      subroutine lianum(type,nulo,string,i4rep,idimi,
     1 a4rep,idima,noblig)
c si si fin de liste au dela de noblig element ok.
      character * 127 string,s80
      character * 4 type,a4rep
      character * 1 lu80(127),seg
	  character*127 lu127
	  equivalence (lu80(1),lu127)
      logical nu,err,fini
      character * 8 verb,verbr,a8va
      dimension a4rep(idima),i4rep(idimi)
      common /io/ mae,mal,lca,imp
      n80=127
      nmot=0
c	  write(*,*)'lianum ',type
      if (type .eq. 'NUME') goto 2890
      do 1090 j=1,idima
1090  a4rep(j)='    '
      goto 2891
2890  do 2892 j=1,idimi
2892  i4rep(j)=0
2891  continue
       i4=0
      call proto(nulo,string,lu80,verb,ipos)
c       write(*,*) 'revenur deproto'
c       write(*,*) lu80
c       write(*,*) 'revenur deproto'
      nuint=0
2620  continue
10    continue
c      write(*,*) 'LIANUM en 10 on va lire '
c      write(*,*) 'appel de lire ',len_trim(lu127)
c	  write(*,*)'lu80:::',trim(lu127),':::'
      call lire(nuint,nulo,lu80,n80,verb,fini,.true.,ipos)
c²      write(*,*) 'revenur de lire ',len_trim(lu127)
c	  write(*,*)'lu127:::',trim(lu127),':::'
c	  write(*,*)'lu80 :::',(lu80(jj80),jj80=1,127),':::'
	  if (len_trim(lu127).eq.127) then
c	   do 4132 jj80=1,127
c	   write(*,*) jj80,ichar(lu80(jj80))
c4132   continue
       
	  endif
	  
       if (type .eq. 'NUME') goto 4000
      if (type .eq. 'TITR') goto 5000
      if (Type .eq. 'ALPH') goto 6000
      call messag(4,'type inconnu (prog. error) !',ipos)
4000  mul = 1
c                liste de nombres ......
4001  continue
      call avance(lu80,ipos,verbr,seg,lv,nu,va,a8va)
      if (ipos .gt. n80) goto 10
      if (.not. nu .and. lv .gt. 0)
     1   call messag(5, ' non numeric value in list !',ipos)
      if (seg .eq. ',' .or. seg .eq. ';' ) goto 4010
      if (seg .eq. '-' ) goto 40101
      if (seg .eq. '*' ) goto 4020
      call messag( 6,'invalid separator in num. list !',ipos)
40101 continue
      ivaldb=va
c      write(*,*) ' intervalle de valeurs intercept‚ db= ',ivaldb
      if (mul.ne.1) call messag(7,'mul interdit sur intervalle',ipos)
      call avance(lu80,ipos,verbr,seg,lv,nu,va,a8va)
      if (ipos .gt. n80)
     1 call messag(8,'intervalle sur une ligne',ipos)
      ivalfi=va
c      write(*,*) ivaldb,' generation intervalle ™',ivalfi
      do 40110 ivlint=ivaldb,ivalfi
      i4=i4+1
      if (I4 .gt. idimi )
     1 call messag (809,'too many value in list (interval) !',ipos)
      i4rep(i4)=ivlint
40110 continue
      goto 40190
4010  do 4015 j4=1,mul
      i4=i4+1
      if (I4 .gt. idimi )
     1 call messag (810,'too many value in list !',ipos)
      i4rep(i4)=va
4015  continue
40190 mul = 1
      if (i4 .lt. idimi .and. seg .eq. ',') goto 4001
      if (i4 .eq. idimi .and. seg .eq. ';') return
      if (seg .ne. ';') call
     1 messag(9,'toutes valeurs servies, manque ; !',ipos)
c      write(*,*) 'i4=',i4,'  idimi=',idimi
      if (i4 .lt. noblig) goto 40199
      noblig=i4
c     write(*,*) 'liste tronqu‚e accept‚e'
      return
40199 continue
      call messag(10,'trouvé ; de fin avant liste servie  !',ipos)
4020  mul=va
      goto 4001
5000  continue
c prendre une re'ponse et analyser
      do 2601 ifin=ipos,n80
      if (lu80(ifin) .eq. ';') goto 2602
2601  continue
      call messag(11,
     1 'missing ending semi-colon (may be line too long!',n80)
2602  ifin = ifin-1
      ipos=ipos+1
      write(s80,'(127A1)') (lu80(j),j=ipos,ifin)
      read(s80,'(20A4)') (a4rep(j),j=1,idima)
      fini = .false.
      return
6000  continue
c prendre une re'ponse et analyser
2633  call avance(lu80,ipos,verbr,seg,lm,nu,va,a8va)
      if (lm .le. 4) goto 2631
      call messag(12, ' alpha value more than 4 characters !',ipos)
2631  continue
c      write(*,*) 'lm,ipos,i4',lm,ipos,i4 ,idima
c accepter que un point virgule tronque la liste
      if (verbr .eq. '        ' .and. seg.eq.';')
     1  write(*,*) 'fin de liste non servie'
      if (verbr .eq. '        ' .and. seg.eq.';')
     1  write(imp,*) 'fin de liste non servie'
      if (verbr .eq. '        ' .and. seg.eq.';')
     1  goto 26391
c      if (ipos .gt. n80 .and. lm .le. 0)
c     1  write(*,*) 'on va lire ligne suite'
      if (ipos .gt. n80 .and. lm .le. 0) goto 2632
      read (verbr,'(a4)') a4rep(i4+1)
      i4=i4+1
      if (i4 .lt. idima) goto 2633
      goto 2639
2632  if (i4 .lt. idima) goto 10
2639  continue
      if (seg .ne. ';') write(*,*) 'pas ; au fin liste i4=',i4
      if (seg .ne. ';') call messag( 13,'alpha list trop longue !',ipos)
26391 fini=.false.
      return
5900  call messag(14,' !',ipos)
      call pose(3)
      end
C ......................................................................
      subroutine lire(nuint,nulo,lu80,i80,verb,fini,oblig,ipos)
      character *127 a127
      character*1 a1(127)
	  equivalence (a127,a1(1))
      character * 1 lu80(i80) ,seg
      character * 8 verbr,verb,vsuit,a8va
      logical fini,oblig ,err,nu
      character * 1 luavan(127)
      logical deja,eof
      common /ligne/ eof,deja,luavan
      common /io/ mae,mal,lca,imp
	  if (i80.ne.127)then
	   write(*,*)'erreur interne et absurde 127<>',i80
	   write(imp,*)'erreur interne et absurde 127<>',i80
	   call pose(716)
	   endif
c
10    ipos=0
c      write(*,*) 'lire en 10 deja=',deja,' verb:',verb,' i80=',i80
c      write(imp,*) 'lire en 10 deja=',deja,' verb:',verb,' i80=',i80
      if (deja) goto 1001
	  do 107 jjbl=1,127
107   lu80(jjbl)=' '
c	  do 1304 jj80=1,i80
c	  write(*,*)jj80,ichar(lu80(jj80))
c1304    continue


c       write(*,*)'fin init 127 espaces'
c       write(*,*) 'lecture en A de A127 sur nulo=' , nulo
c      write(*,*) 'nulo=',nulo
	  if (nulo.eq.0) then 
      read(*,'(a)',end=10091) a127
	  else
      read(nulo,'(a)',end=10091) a127
	  endif
c	  write(*,*) ' (commenter) ',len_trim(a127),'!!!',trim(a127),'!!!'
	  if (len_trim(a127).gt.80) then
      write(*,*)trim(a127) 
 	  write(imp,*)trim(a127) 
	  			   write(*,*)(' ',jjbl=1,79),'!'
			   write(imp,*)(' ',jjbl=1,79),'!'

 	  write(*,*)'ligne ci dessus depasse 80 caracteres'
	   write(imp,*)'ligne ci dessus dépasse 80 caractères'
       call pose(967)	   
	   endif
	  goto 10099
10091 write(*,*)'fin de fichier en lecture ligne 1 du fichier.'
      write(imp,*)'fin de fichier en lecture ligne 1 du fichier.'
      call pose(1009)
10092  write(*,*) 'err lire a127'
       write(imp,*) 'err lire a127'
       call pose(1008)
10099  continue
c	  write(*,*)'prem a127:',a127
c      write(*,*) 'lu dans le fichier a127 ligne suivante'
	  if (len_trim(a127).gt.80) then
	  write(*,*) 'ligne trop longue:',len_trim(a127)
      write(*,*) 'a127: "',trim(a127),'"'
	  write(imp,*) 'ligne trop longue:',len_trim(a127)
      write(imp,*) 'a127: "',trim(a127),'"'
	  call pose(1800)
      endif
       do 10096 j=1,80
10096  lu80(j)=a1(j)
c       write(*,*) 'copie de 80' 
c²	  do 1307 jj80=1,i80
c	  write(*,*)jj80,ichar(lu80(jj80))
c1307    continue
c       write(*,*) 'copie de 80' 
c	   CALL Pose(6161)
c       write(*,*) 'test: lu dans a127 ',lu80
       ilastnb=0
c pourrait len_trim²
	   do 888 jrc=1,80
	     if (lu80(jrc).ne.' ')ilastnb=jrc
888	   continue
c      write(*,*)'lire à garder ilastnb=',ilastnb
      write(*,'(1x,127A1)') (lu80(j),j=1,ilastnb)
      write(imp,'(1x,127A1)') (lu80(j),j=1,ilastnb)
c      write(*,*)'lire à garder'
c	  do 1309 jj80=1,i80
c	  write(*,*)jj80,ichar(lu80(jj80))
c1309    continue
c      write(*,*)'lire à garder'

c       write(*,*)'ilastnb=',ilastnb	  
      if (ilastnb.eq.0) then
c	      write(*,*)'ligne vide (a commenter)'
	      write(*,*) 
          write(imp,*)
          goto 10
		endif  
c       write(*,*) 'fin de li dans a127'
      if (lu80(1) .eq. '*') then
c	   write(*,*) '* en col 1 ',lu80
c      write(*,*) 'ligne a commenter suit laligen "nomaél" i80=',i80
	  write(*,'(1x,127a1)') (lu80(j),j=1,80)
          write(imp,'(1x,127a1)') (lu80(j),j=1,80)
          goto 10
		endif  
      goto 1008
10090 write(*,*)'subroutine lire end en read interne'
      write(imp,*)'subroutine lire end en read interne'
      call pose(517)
1009  fini = .true.
      if (.not. oblig) then 
c	    write(*,*) 'trouve commande facuiltatibe à commenter'
		return
		endif
      write(*,*) 'commande attendue: ',verb 
      write(imp,*) 'commande attendue: ',verb 
      call messag( 15,'end on file on reading the above command ! ',0)
1008  continue
      goto 1002
1001  continue
c      write(*,*)'reprise de luavan'
      do 1003 j80=1,i80
1003  lu80(j80)=luavan(j80)
c      write(*,*) 're lu dans le buffer'
      write(*,'(1x,127A1)') (lu80(j),j=1,i80)
      write(imp,'(1x,127A1)') (lu80(j),j=1,i80)
      deja=.false.
1002  continue

c Upper case
      ipos=0
c	  write(*,*)'upper case ',i80
      do 543 j=1,i80
      if (lu80(j).lt.'a' .or. lu80(j) .GT. 'z') goto 543
      lu80(j)=char(ichar('A')+ichar(lu80(j))-ichar('a'))
 543  continue
      call avance(lu80,ipos,verbr,seg,lm,nu,va,a8va)
c ignorer les lignes de commentaires.
      if (verbr .EQ. 'COMMENT ') goto 10
      if (verbr .ne. 'FINISH  ') goto  1105
      fini = .true.
      if (.not. oblig)      return
      call messag(16, 'FINISH found , premature end of job !',ipos)
1105  continue
      if (verbr .eq. verb) goto 11055
      write(*,*) 'commande attendue: ',verb,': trouve:',verbr
      write(imp,*) 'commande attendue: ',verb,': trouve:',verbr
      call messag(17,'COMMAND found not= expected: !',
     1 ipos)
11055 continue
      if (seg .eq. ':' .and. nuint .eq. 0) goto 1205
c    carte numerotee  touve ou necessaire
c      write(*,*) 'on a seg=',seg,' et nuint=',nuint
      if (seg .ne. '/' .and. nuint .gt. 0) call messag
     1 (818,'continuation card expected with VERB/number',ipos)
      call avance(lu80,ipos,vsuit,seg,lm,nu,va,a8va)
c      write(*,*) 'carte suite va=',va
      if (.not.nu .or. va .ne. nuint) call messag
     1 (813,' invalid sequence number !',ipos)
1205  nuint=nuint+1
      if (seg .ne. ':')
     1 call messag (811,' missing colon after the first word !',ipos)
      fini = .false.
c      write(*,*)'sortie en bas de lire'
c      write(imp,*)'sortie en bas de lire'
c	  do 130 jj80=1,i80
c	  write(*,*)jj80,ichar(lu80(jj80))
c	  write(imp,*)jj80,ichar(lu80(jj80))
c130    continue
c	  write(*,*)'sortie en bas de lire'
	  
      return
      end
C ......................................................................
      subroutine avance(lu80,ipos,motsui,sepfin,lmot,nume,val,a8val)
      character * 1 lu80(127),sepfin,espace,ms1(8),ms2(8),finmot(8)
      logical nume,debval
      character * 8 motsui,m8trav,a8val
      common /io/ mae,mal,lca,imp
      data espace /' '/,
     1 finmot/' ',',','-',';','=',':','*','/'/,nfin/8/
      data mxmot/8/
      mxlig=127
c       write(*,*) 'demande de avnce depuis ',ipos
      debval = .true.
      nptvu = 0
      val = 0.
      sepfin=' '
      nume =.true.
      lmot=0
      ipos=ipos+1
c avancer jusqu a non espace
 21   if (lu80(ipos) .ne. espace) goto 20
      ipos=ipos+1
      if (ipos .gt. mxlig) goto 90
      goto 21
C                debut de mot
c avancer tant que mot en copiant dans ms1 jusqa mxmot
20    continue
c   effacer le mot a construire
      do 25 i=1,mxmot
25    ms1(i)=espace
c analyser le car suivant
39    continue
c  si on est en debut de val autoriser le - au sens de moins
      if (debval .and. lu80(ipos).eq.'-') goto 330
      do 30 i=1 , nfin
      if (lu80(ipos) .eq. finmot(i) ) goto 31
30    continue
c      car ordinaire ajouter au mot
33    continue
      if (nptvu .eq. 0 .and. lu80(ipos) .EQ. '.') goto 330
      nume = nume .and. ( (lu80(ipos) .ge. '0') .and.
     1                    (lu80(ipos) .le. '9')  )
330   debval = .false.
      if (lu80(ipos) .eq. '.') nptvu=nptvu+1
c      if (lu80(ipos) .eq. '.') write(*,*) '. en ipos',ipos
       if (lmot.ge.mxmot) write(*,*) 'lmot,mxmot,ipos',lmot,mxmot,ipos
       if (lmot.ge.mxmot) write(imp,*) 'lmot,mxmot,ipos',lmot,mxmot,ipos
      if (lmot .ge. mxmot)
     1 call messag(18,'word more than height characters !',ipos)
      lmot=lmot+1
      ms1(lmot)=lu80(ipos)
      ipos=ipos+1
      if (ipos .le. mxlig) goto 39
      call messag
     1 (814,'keyword ended at end of line, missing separator!',ipos)
c                fin de mot
c le lire dans motsuiv
c avancer jusqua separtartet si espace
31    if (lu80(ipos) .ne. espace ) goto 35
c
36    if (ipos .ge. mxlig)
     1 call messag(19,
     2 ' missing ponctuation de fin de ligne, or ; !',ipos)
      ipos=ipos+1
      if (lu80(ipos) .eq. espace) goto 36
c mot, espace et different :  un separateur
c ci dessous 1 au lieu de 2
      do 370 j=2,nfin
      if (lu80(ipos) .eq. finmot(j)) goto 35
370   continue
c     write(*,'(80a1)')lu80  
      if(ipos.le.80)then
      call messag(20,'multiple word without separator '//
     1 '!',ipos)
	   else
      call messag(20,'end of line.!',ipos)
	   endif
35    sepfin = lu80(ipos)
      write(motsui,'(8a1)') (ms1(j),j=1,8)
c if nume retourner aussi la valeur enti„re .
      if (nptvu .gt. 1)
     1 call messag(21, 'more than one decimal point ! ',ipos)
c 1 cadrer ™ droite
      do 810 i=1,8
810   ms2(i)=espace
      do 80 i=1,lmot
80    ms2(i+8-lmot)=ms1(i)
      write(m8trav,'(8a1)') (ms2(i),i=1,8)
      if (.not. nume) goto 89
      read(m8trav,'(f8.0)') val
      read(m8trav,'(a8)') a8val
89    continue
      read(m8trav,'(a8)') a8val
c     write(*,*) 'relu dans m8 trav:',a8val
90    return
      end
C ......................................................................
      subroutine messag(nappel,string,ip)
c  si nappel>0 fera pose(nappel)	  
C a placer en bout pour eviter controle type string !
      character * 80 string
      character * 1  st1(80)
      common /io/ mae,mal,lca,imp
c ici string pas terminé! impossible
c	  write(*,*)'msg=',nappel,ip,string
      read(string,'(80a1)') (st1(j),j=1,80)
      do 1 j=1,80
      if (st1(j) .eq. '!') goto 2
1     continue
      j=80
2     write(*,'(1x,127a1)') (' ',i=1,ip),'!'
      write(imp,'(1x,127a1)') (' ',i=1,ip),'!'
      write(*,'(1x,127a1)') (st1(i),i=1,j)
      write(imp,'(1x,127a1)') (st1(i),i=1,j)
       if (nappel.ge.0)call pose(100+nappel)
      
      end
      subroutine upcase(c)
      character * 4 c
      character * 1 c1
      character * 3 c3
      read(c,'(a1,a3)') c1,c3
      if (c1 .ge. 'a' .and. c1 .le. 'z')
     1 c1=char ( ichar(c1) - ichar('a') + ichar('A') )
      write (c,'(a1,a3)') c1,c3
      return
      end
      function djvi(i,j)
      if (j .eq. 0) djvi=0.0
      if (j .ne. 0) djvi=(i*1.0)/j
      return
      end
      function div(a,b)
      real a,b
      if (b .eq. 0.0) div=0.0
      if (b .ne. 0.0) div=a/b
      return
      end
       subroutine STPA(nappel,i,imp)
      logical fsaved
      character*4 byedsn
	  	   common /bye/ byedsn

 	   character*96 dsnlog
      common /chetit/ ctitre,cche,cdate,fsaved,dsnlog
      character * 80 ctitre
      character * 8 cdate,cche
     
c nom parait  inuytole ici	  
      character * 4 nom(6)

c	  write(*,*) 'stpa ',nappel,' imp: ',imp
      write(*,1) ' ',cdate,i
	  if (imp.ne.66) return
c	  write(*,*) 'stpa ',nappel,' para8: ',a8date
      write(imp,1) char(12),cdate,i
	  
      i=i+1
1     format(a1,'Cevipof/Lasmas (CNRS)  Construction Echelle de ',
     1 'Loevinger 'a8,' page',i4)
      write(*,*) ctitre(1:78)
      write(imp,*) ctitre
c      write(*,*) 'chemin=',cche,'    cdate=',cdate
c      write(imp,*) 'chemin=',cche,'    cdate=',cdate
      return
      end


      function ilfaut(nbq,i,nbit,iait,iqorit,qq,numeit)

	  logical iait(1)
	  integer*4 iqorit(1),qq(1),numeit(1)
	  common /controlist/ partil,absents,listqabsentes,listqpresentes
	  logical partil,absents,listqabsentes,listqpresentes

c      write(*,*)'ilfaut P:',partil,' QA:',listqabsentes
c fautil afficher cet item ?
      ilfaut=0
      if (listqabsentes)then
	  if (iqorit(i).le.0) then
	      write(*,*)'listqabsentes ',i,iqorit(i)
	      write(imp,*)'listqabsentes ',i,iqorit(i)
		 call pose(2010)
	  endif	 
c liste uniquement les items des questionsans aucun item
	  if (iqorit(i).gt.nbq) then
	      write(*,*)'listqabsentes ',i,iqorit(i),nbq
	     write(imp,*)'listqabsentes ',i,iqorit(i),nbq
		   call pose(2012)
		 endif
c liste uniquement les items des questionsans aucun item
      if (qq(iqorit(i)).eq.0) then
c	  write(*,*) 'OUI ', i,iqorit(i),'qq :',qq(iqorit(i))

	    ilfaut=1
		
c         write(*,*) 'LQA faut item #',i,numeit(i)
		 return
		else
c	  write(*,*) 'NON ', i,iqorit(i),'qq :',qq(iqorit(i))
c		 	  write(*,*)'list q absentes NON'
       endif
	  goto 25000
	  endif

      if (listqpresentes)then
c 	  write(*,*)'list q presentes'
c liste uniquement les items absents des questionsans présents
      if (qq(iqorit(i)).gt.0) then
c 	  write(*,*)'list q presentes OUI'
	    ilfaut=1
         else
c		  	  write(*,*)'list q presentes NON'

		endif
	  goto 25000
	  endif
	  
	  if (partil) then
           if( iait(i)) then
c	      write(*,*)'present et n''y est pas'
	     ilfaut=1
		 else
c		  write(*,*)'present et y est'
	     endif
         endif
		 if (absents .and. .not. iait(i)) then 
	     ilfaut=1
c		 write(*,*)' absent et pas it ',numeit(i)
	   endif	 
	  if (.not.partil .and. .not. absents) ilfaut=1
25000  continue		  
c     write(*,*)'on sort de iflaut'
      end

	  
c--------------------------------------------------
ccall histo
c ²²L     CALL HISTO (P,IAIT,IMP,NBIT,MQn,mqu,Mx,x,numeit,iqorit)
c h:       CALL HISTO (P,IAIT,IMP,NBIT,MQn,mqu,Mx,x,numeit,iqorit)

      subroutine G(nitp,nbq,p,iait,imp,
     1 nbit,m,mqu,mx,x,numeit,iqorit,qq)
	  dimension qq(1)
	  dimension p(nbit),numeit(nbit),iqorit(nbit)
	  logical x(nbit)
c m abrev pour mqn (nom donné), mqu et mx csv orifine	  
      character * 8 mqu(nbit),mx(nbit),m(nbit)
c 20190209 GAFFE NR eszt aussi non rep!
	  integer*4 nr(nbit)
c slmt sera .true. si un item est à prfendre en comptge	  
      logical slmt
c                      si true seulement les presents
      logical iait(nbit)
      character * 1 tout(132) ,trecap(132), tsave(132)
      logical prem
c separateurf : si 1 +ou* si multiple	  
	  character*1 sep
	  character*80 fsep
	  logical x2
      logical inter2,col5,large
	  common /inter2/ inter2,col5,large,ls

	  x2=large
c n était un raccourci
c             write(*,*)' histio listqabsentes!',listqabsentes

c pour m"moire routine filtre lieste
      call direqui(nbit,nitp)
	  if (x2) then
	  fsep='(9x,'':0'',5(17x,i3))'
	  lontir=99
	  lu=102
	  else
	  fsep='(9x,'':0'',5(7x,i3))'
	  lontir=51
	  lu=54
	  endif
      write(*,fsep)20,40,60,80,100
      write(imp,fsep)20,40,60,80,100
      write(*,*)'--------+',('-',irep=1,lontir),'+---'
      write(imp,*)'--------+',('-',irep=1,lontir),'+---'
      prem=.true.
      do 1141 iout=1,132
1141   trecap(iout)=' '

c balayer tous les items
      do 100 ibit=1,nbit
10000 continue      
c sauver la ligne actuelle pour si collision
      do 147 iout=1,132
147   tsave(iout)=tout(iout)

      if (.not. prem) goto 140
c                                 effacer la ligne
      do 141 iout=1,132
141   tout(iout)=' '
      prem=.false.
140   continue
c  placer item sur la ligne
      if(x2)then
	  iout=3+ 100*p(ibit)
	  else
      iout=3+ 50*p(ibit)
      endif
c      write(*,*)'iout=',iout
c	  write(*,*)'histo call ilf'
      do 21345 ix=1,nbq
c	  write(*,*)'en histo ',ix,qq(ix)
21345 continue	  

	  ilf=ilfaut(nbq,ibit,nbit,iait,iqorit,qq,numeit) 
c	  write(*,*)'revenu ilf=',ilf
      slmt=ilf.gt.0 	  
c      write(*,*)' histo ',numeit(ibit),slmt
      if (slmt) then 
c	     write(*,*)'affichier item ',numeit(ibit)
	     goto 380
		 else
c		 write(*,*)' pas ',numeit(ibit)
		 goto 300
		 endif
c  le - ne vient que si pas deja un desous 
      if (tout(iout).ne.' ') then
c si surcharge, vider ligne en cours	  
      write(*,*) '        ','=',(tsave(jout),jout=1,lu),'='
      write(imp,*) '        ','=',(tsave(jout),jout=1,lu),'='
      do 14144 ioutf=1,132
14144   tout(ioutf)=' '
      endif
        
	    if (iait(ibit)) then
	   	  tout(iout)='+'
          if (trecap(iout) .eq. '-') then
	        trecap(iout)='?'
			    write(*,*)'surcharge a iout=',iout
		  else
		    trecap(iout)='+'
			    write(*,*)'+ a iout=',iout
		  endif 
	    
		else
	      tout(iout)='-'
          if (trecap(iout) .eq. ' ') then
	         trecap(iout)='-'
	         if (trecap(iout) .eq. '+') then
			    write(*,*)'surcharge a iout=',iout
	            trecap(iout)='?'
		     else
		        trecap(iout)='-'
			    write(*,*)'- a iout=',iout
		     endif 
		   endif
	    endif
      goto 381

380   continue
c     mettre item present
      if (tout(iout).eq.' ') goto 38001
c     collision
      write(*,*) '        ','=',(tsave(jout),jout=1,lu),'='
      write(imp,*) '        ','=',(tsave(jout),jout=1,lu),'='
      prem=.true.
      goto 10000

38001 tout(iout)='+'
	    if (.not.iait(ibit)) tout(iout)='-'
	    if (iait(ibit)) tout(iout)='+'

c     on ne gere pas les collision sur trecap
       if(iait(ibit)) then
       if (trecap(iout) .eq. '-') then
	        trecap(iout)='?'
c			    write(*,*)'surcharge a iout=',iout
		  else
		    trecap(iout)='+'
cc			    write(*,*)'+ a iout=',iout
		  endif 
c      trecap(iout)='+'
       endif
	    if (.not.iait(ibit)) then
         if (trecap(iout) .eq. ' ') then
	         trecap(iout)='-'
	         if (trecap(iout) .eq. '+') then
			    write(*,*)'surcharge a iout=',iout
	            trecap(iout)='?'
		     else
		        trecap(iout)='-'
c			    write(*,*)'- a iout=',iout
		     endif 
		   endif
		  endif
c	    if (iait(ibit)) trecap(iout)='+'
381   continue
      inum=numeit(ibit)
310   iout=iout-1
      if (inum .le. 0) goto 300
      i1=inum/10
      i1=inum - 10 *i1
      inum=inum /10
      if (tout(iout).eq.' ') goto 320
c ici faudrait avoir sauvegarde r et recommencer
      write(*,*) '        ','=',(tsave(jout),jout=1,lu),'='
      write(imp,*) '        ','=',(tsave(jout),jout=1,lu),'='
c      write(*,*) ' ci desusus car impossible ecrire:',numeit(ibit)
      prem=.true.
      goto 10000
320   continue
      tout(iout)=char(ichar('0')+i1)
      goto 310
300   continue
      if (ibit .ne. nbit  .and. iqorit(ibit).eq. iqorit(ibit+1))
     1 goto 120
110   continue
c                  routine impression ligne
c      write(*,*)m(iqorit(ibit)),mqu(iqorit(ibit))
      sep=':'
	  if (mx(iqorit(ibit)).ne.'        ')then
	  if (m(iqorit(ibit)).eq.'        ')then
	  if (x(iqorit(ibit)))then
	    sep='*'
		else
		sep='+'
		endif
		endif
	  endif
      if (m(iqorit(ibit)).ne.'        ')then
      write(*,*) m(iqorit(ibit)),sep,(tout(jout),jout=1,LU),':'
      write(imp,*) m(iqorit(ibit)),sep,(tout(jout),jout=1,lu),':'
	  else
      write(*,*) mqu(iqorit(ibit)),sep,(tout(jout),jout=1,lu),':'
      write(imp,*) mqu(iqorit(ibit)),sep,(tout(jout),jout=1,lu),':'
	  endif
      prem=.true.
120   continue
100   continue
      write(*,*)'--------+',('-',irep=1,lontir),'+---'
      write(imp,*)'--------+',('-',irep=1,lontir),'+---'
      write(*,*) '        ',':',(trecap(jout),jout=1,lu),':'
      write(imp,*) '        ',':',(trecap(jout),jout=1,lu),':'
      return
      end
c--------------------------------------------------
 

	  subroutine stamp(unit,ipourcent)
	  integer unit
      character (10)  datedt,time,zone
c      write(*,*)'appel de stamp pour ',unit
	  call date_and_time(datedt,time,zone)
	  if (ipourcent.le.0) then
      write(*,*)'le: ',datedt(7:8),'/',datedt(5:6),'/',datedt(1:4)
     4 ,' a: ',time(1:2),':',time(3:4),':',time(5:6),' [',zone(1:5),']'
      if (unit.eq.0 )then
       else
      write(unit,*)'le: ',datedt(7:8),'/',datedt(5:6),'/',datedt(1:4)
     4 ,' a: ',time(1:2),':',time(3:4),':',time(5:6),' [',zone(1:5),']'
	   end if
      else
	  
	    write(*,*)'le: ',datedt(7:8),'/',datedt(5:6),'/',datedt(1:4)
     4 ,' a: ',time(1:2),':',time(3:4),':',time(5:6),' [',zone(1:5),']' 
     6 ,ipourcent,' %'
      if (unit.eq.0 )then
       else
      write(unit,*)'le: ',datedt(7:8),'/',datedt(5:6),'/',datedt(1:4)
     4 ,' a: ',time(1:2),':',time(3:4),':',time(5:6),' [',zone(1:5),']'
     5 ,ipourcent,' %'
 	  end if
      endif
	  end 
c**********************************²******************* pp
      parameter (q9=99)
	  parameter (L000=2000)
      parameter (L5000=50000)
c nombre de codes (0 à 10 11 pour NR/99)
	  parameter (M1=12)
      logical m
      character*8 AAAAMMJJ
	  character*127 prototype
      DIMENSION         NUMQ(99),MAK(99)  , PP(6000)
	  integer *4 NEUFNEUF(99)
      INTEGER * 2 PP
      EQUIVALENCE (LZ,NUIT)
      CHARACTER * 4 IZIZ,IA,V,METHTA
      character*8 mqu,mqn,mq,mx
	  character*8 mqzun
	  logical x
	  integer*4 nr
c 20190203 ivv passe à 11  code (0 à 10) 
c NR etc devraient etre en Q9
      DIMENSION IVV(M1),mqn(99),MQu(99),nr(99),mx(99),x(99),s(99)
     1, IPOP(100)
	  logical s
      character*4  IVV
      logical IBL
     1 , IHA,IHC,IHD,IHE,IHG,IHL,IHM,IHS
       character*4 meth
	   character * 1 meth1
      DIMENSION IR(200)
      CHARACTER * 8 IBL8,NOMCOP
      character * 8 chemin
      character * 80 chemin80
      DIMENSION NOMCOP(500)
C ******* A REMETTRE A BONNE VALEUR
      DOUBLE PRECISION NBJ
      CHARACTER * 8 IDD,NOMCOD
c ???? 10
      DIMENSION NOMCOD(99,10)
      DOUBLE PRECISION ICI(100),IWI(100),IC,IW,ICIJ,IWIJ,ICII,IWII,FDIV
      DIMENSION CI(100),WI(100),P(100),n(100)
c                         iait a true si item present actuellelmment
      LOGICAL  IAIT (100)
c                         LV (item code) a true si le code inclu dans item
c premiere dim es le nombre d'items max, le second le nb de valeurs
c deuxièeme 100 autorise code 10 pour ré&ponse simple mais max x10
c passé à 101
      logical LV(100,101)
c 20171211  labels de 99 questions 1 à 99 10 codezsn (0à9)
       character*200 labelune,labelprt,labels(99,M1),labelsprt(99,M1)
	   character*200 labelsX(99,M1),labelsXprt(99,M1)
	   character * 200 labelscr,labelsscr(99,M1)
	   character * 200 labelsXscr(99,M1)
	   logical uselabels,labeli,labelr,labelq,usequestions
	   character*200 questionsu(99),questionsx(99),
     1 questionsXscr(99),questionsXprt(99),
     1 questionsUscr(99),questionsUprt(99)
	   character*80 qeq(99),colbrut(L000),ligbrut(L000)
	   character*80 colup(L000)
	   logical usedcol(L000)
c	   integer clientcol(L000)
	   integer ivalcol(L000)
	   logical colvide(L000)
	   common /labels/ uselabels,labels,labelsprt,labelsscr,labeli,
     1  usequestions,questions,questionsprt,questionsscr
	  integer X1(100,M1),x2(100,M1)
      EQUIVALENCE(V,METH,IA)
      LOGICAL MACHIN         ,IMPINT    ,LUTAB ,IMPIT,IMPIGN
      character * 96 dsn,dsnlu,dsnlog,dsnk
c  variables ajoutess pour mot clefs
      real tsave(20)
      character*8 asave(20),proch,arappl
      logical fini
	  common /controlist/ partil,absents,listqabsentes,listqpresentes
      logical partil,absents,listqabsentes,listqpresentes
	  common /presents/  qq
      integer*4 qq(100)	 
    
c
      integer i4rep(20)
      character * 4 a4rep(20)
c tableau des noms d'items
      integer*4 numeit(100)
      integer lircol(99),lircolx(99)
c
c  iqorit indique le ranq de la question dont est tir‚ item .
      integer iqorit(100)
c irepq servira a construire la reponse a la question evenrtuellement croise
      integer irepq(100)
c faitq notera question lue	  
      logical faitq(100)	  
c tit indique le vecteur 0 ou 1 pour calculeer ni
      logical  irepit(100)
c                             irepit profil un indu=vidu
      character * 1 tlu(L5000)
	  character * 50000 tlu1000,tscr,tprt
	  equivalence (tlu(1),tlu1000)
	  
	  character * 1 tlab(L5000)
	  character * 50000 tlab1000
	  equivalence (tlab(1),tlab1000)
	  
	  
c                              pour lire une ligne fichier
      character*100 dsndat,trav80  ,titre,dsnlab,dsnlibq
c                            pour lire le fihcier de donness
      logical lexist
c                       pour inquire
       character * 4 mon,nom,mal
       logical hseul,fa    ,fotitr
       logical fsaved
	   logical COLN,lp,ln,listdict
	
      character*4 byedsn
	  	   common /bye/ byedsn

      common /chetit/ ctitre,cche,cdate,fsaved,dsnlog
      character * 80 ctitre,relu
      character * 8 cdate,cche
      logical inter2,col5,large
	  common /inter2/ inter2,col5,large,ls
	  
	  logical ilyaeu,fautxx
       character * 1 acmd
       character * 1 luavan(127)
       character *80 pourlire80a1
	  character * 80 nomfichierspr
       
	  logical eof,deja,pc
       common /ligne/ eof,deja,luavan
      common /io/ mae,mal,lca,imp
	  logical append,lestla
	   character*1 unc
	   integer*4 la(2)
	   character*8 jcj
	   equivalence (jcj,la)
	   logical egalpropose
	   logical lememe
	   logical csv
	   integer listitem,listq
c true si combinaison	   
	   logical deux 
	   integer itv(100)
	   character *1 quatrea1(4)
	   character *4 a4
c ajusté à ligne kabc
	   character *50 a100
	   equivalence(quatrea1(1),a4)
	   character*80 repetfin
c 20171203
       character*80 f251
	   character*80 entete
	   character*80 invite
	   logical prttete
	   logical listqused
	   
    	  character*8 a8 (500),ltrim

c 21081214
      character*8 qfiltre
	  integer ifiltre,nfiltre
C                                **
C                                **
C     ----------------------- INITIALISATION DES CONSTANTES ALPHA .
      DATA MAXIT/100/,MAXQ/99/,maxlig/50000/
      DATA IBL8/' '/
c      IDINF(I,J)=(I*(I-1))/2+J
c ancien mode seulement trangle inf
      IDINF(I,J)=(max0(I,j)*(max0(i,j)-1))/2+min0(i,J)

c*****************************************************
       ifiltre=0
	   nfiltre=0
	   qfiltre=''
		  AAAAMMJJ='20181212'
	  
	  invite='?  H, A/S, L, P/F, Tncwh+ijkstu,'//
     1 ' +/-/E##, M[01245QRIVW] K[abc], Q.'
       prttete=.false.
	     nbcolcsv=0
	  lrepet=0
	  imp=0
	   egalpropose=.false.
	   lememe=.false.
	   uselabels=.false.
	   write(*,*) 'uselabels:',uselabels
c pour ne pas imprimer encire	  
	  
C    ------------------------ AFFECTATION DES UNITES D ENTRE SORTIE
C     EN CAS D UTILISATION AVEC UNE CONSOLE MAE ET MAL LA DESIGNENT
c     
      inter2=.true.
	  col5=.true.
	  labeli=.false.
	  labelr=.false.
	  labelq=.false.
      fsaved=.true.
c	  write(*,*) 'fsaved init true'
c signifie pas besoin de sauver mais pas qu'une échelle est reprenable	
c passera à false des le passage en A/S  
c donc mae=stdin équivaut à *
      MAE=0
	  jprem=80
	  ilastnb=80
      partil=.false.
	  absents=.false.
	   listqabsentES=.false.
c	         write(*,*)'(1)listqabsentes!',listqabsentes

		  listqpresentes=.false.
		 
		 do 441 iiq=1,99
		 do 441 iiv=1,M1
		 labelsX(iiq,iiv)=''
		 labelsprt(iiq,iiv)=''
		 labelsscr(iiq,iiv)=''
		 labelsXprt(iiq,iiv)=''
		 labelsXscr(iiq,iiv)=''
441      labels(iiq,iiv)=''
	  
c             mae = machine a écrire mal:lire programme ‚crit
c on mettre 66 quand ouvert
c      IMP=66

      IPA=1
      hseul=.false.
      nbpas=1
c	  write(*,*) 'nbpas init 1'
c               (nbpas ne sert que si on ne recharge pas une vieille)
       deja=.false.
      minlrl=0
c                   notera la longueur de ligne neceszsaire
C ******
c      call date_and_time(datedt,time,zone)

	 
c ne pas utiliser mae avant affectation
c attendre connaitre nom imp pour recopier
      write(*, 3001)
3001  FORMAT('VERSION Origine du 19 5 72,',
     1 'MS Fortran du 22/8/88 KHI2 ET C, W, H, EN DOUBLE PRECIS.'/
     2 'Version mot-clef du 14 Juillet 1991 '/
     3 'Version Implication du 4 Juillet 1997'/
     4 'Version .CSV, "libelles"  par G77 le 25 Janvier 2018 16:45'/
     5  'Huge, 20190209 Soustractif OK. 10/99 OK.')

   	  call stamp(0,0)
      write(*,*)
      write(*,*) 'en g‚n‚ral, vous pourrez obtenir de l''aide en'
     1 , ' frappant ?<return>'
3102  continue
c      write(*,*) 'entr‚e ?'

c protection deux param      
c prende le deuxième, il doit être vide
	  call getarg(2,nomfichierspr)
      if (len_trim(nomfichierspr).gt.0) then
	    write(*,*) 'plus d''un argument, peut-etre *'
        write(imp,*) 'plus d''un argument, peut-etre *'
        call pose(613)
		endif

	  call getarg(1,nomfichierspr)
	  dsn=nomfichierspr
	  ifin=len_trim(dsn)
	  
c si pas de parametre on arrive ici avec ifin=0
	  if (ifin.gt.0) then 
c RETRO COMPAT
c il était ptévu * sur invite pour conversationnelc mais impossible sur ligne commande remplce par .
c poit TOUT SEUL
	  if (nomfichierspr.eq.'.') nomfichierspr(1:1)='*'
c trop top pour imp!
	  write(*,*) 'parametre de la ligne commande: ',
     1 '"',trim(nomfichierspr(1:ifin)),'"'
		
	   dsn=nomfichierspr(1:ifin)
c       write(*,*) 'param ifin=',ifin,' ' ,trim(dsn)	   
c	    write(*,*)'now ifin=',ifin
       endif	
	if (ifin.eq.0)then 
		write(*,*) 'entrez votre choix termin‚ par <RETURN>'
        write(*,*)
     1   '* ou . pour travailler en conversationnel'
        write(*,*)
     1    'nom_de_fichier si vous avez pr‚par‚ votre ‚chelle'
        write(*,*)
     1    'nom_sauvegarde_par_K (chemin.abc)'
        write(*,*)
     1  '<F6>, <^Z> ou <^C> pour abandonner.'
	  write(*,*)'[fichier>fichier resultats(nommer listing)]'
	  write(*,*)'[fichier>>fichier resultats(ajouter au listing)]' 
      else 
      endif
	  
      if (egalpropose)write(*,*) '= pour ',relu

c si rien ne vizent sdz ligne de commande lire dsn
	  if (ifin.gt.0) then
c	   write(*,*) 'dsn recu:',dsn
	  else
      read(*,'(a)',end=71098) dsn
c point
       endif
c       write(*,*)'exceptions "',trim(dsn),'"'
        if (trim(dsn).eq.'?') then 
		  write(*,*)'pas d''aide ici'
		  call pose(7510)
		endif
       ilastnb=len_trim(dsn)
       if (ilastnb.eq.0) then
	     write(*,*)'nom vide'
		 call pose(57)
		 endif
		if (ilastnb.eq.1 .and. dsn(1:1).eq.'.') dsn(1:1)='*' 
		if (ilastnb.gt.1 .or. dsn(1:1).ne.'*') then
 		write(*,*)'verifier si existe "',trim(dsn),'"'          		
	      inquire(file=dsn,exist=lestla)
c		  write(*,*)'lestla:',lestla
	      if(lestla)then
             write 		(*,*)'Oui, est-ce une sauvegarde par Kabc?'
			 if (.false.) then
		     open(21,file=dsn,err=10401)
		     goto 10403
10401        continue
             write(*,*) 'erreur ouverture (1)',dsn(1:ilastnb)
		     call pose(6)
10403        continue
             write(*,*)'lecture formatee a4 en A '
		     read(21,'(a)',err=1044,end=1044) a100
			 write(*,*)'ok lecture formatee A '
			 goto 1045
1044         write(*,*) 'err ou end en kabc lire formate a100'
             close(21)
			 endif 
c			 write(*,*)'ouverture en binaire'
			 open (21,file=dsn,form='unformatted',status='old',err=1041)
c			 write(*,*)'ok ouverture  binaire.'	
c lire 80 caractere
		     read(21,err=1042,end=1042) a100
			   write(*,*)'lu: "',(a100(jja1:jja1),jja1=1,50),'"'
			 goto 1045
1041         write(*,*)'erreur lors de la tentative de lecture de "',
     1 trim(dsn),'"'
             write(*,*)'(fichier peut-etre inaccessible.)'
			 call pose(104)
1042         write(*,*) 
     1 'err/end lors de l''essai de lecture en binaire de "',
     1 trim(dsn),'"'
             write(*,*) '(fichier peut-etre innaccessisble.)'
             call pose(1043)
1045         continue
		     close (21)
		     if (a100(1:10).eq. 'sauvegarde') then
		       write(*,*) 'OUI, une sauvegarde '
			   write(*,*) 'cree par ',a100(37:41),
     1 ' Version ',AAAAMMJJ
			   goto 50372
			 else 
               write(*,*)'n''a pas l''air d''une sauvegarde'			 
		     endif
		  else
c		    write(*,*)'pas de fichier ',dsn
		    if (dsn(1:1).eq.'?') then 
			   write(*,*)'pas d''aide ici'
			   call pose(855)
		    endif   
            write(*,*)'NON, on abandonne.'
            call pose(75)		  
		    endif
        endif
cpo


c suivant ote noel 945
c	  endif
	  if (dsn(1:1).eq.'.') dsn(1:1)='*'
c	  write(*,*) 'lu<<<<',dsn,'>>>>'
c	  write(*,*) 'lu(1:1)<<<<',dsn(1:1),'>>>>'
	      jcj(1:1)=dsn(1:1)
c		  write(*,*) la(1)
c 61 c'est =

        
	  	  if (la(1).eq.61) then
		     write(*,*) 'reponse = voir si ;'
              if (.not.  egalpropose) then
			   write(*,*)'impossible pas de fichier slmot2017.in'
c			   write(imp,*)'impossible pas de fichier slmot2017.in'
			   call pose(54)
			   endif
c ctitre a deja été lu avant d'offrir =
			 if (index(relu,';').eq.0) then
			   dsn=relu
			   write(*,*) 'meme fichier a llire: ',dsn
			   lememe=.true.
			 else
			   dsn='slcsv2017.in'
               write(*,*) 'trouve = faudra lire fichier ',dsn			 
			 endif
		endif
			 
			 
       
c	  write(*,*) 'on continue dsn=',dsn
	  idelim=index(dsn,'>')
	  if (idelim.gt.0) then
	    dsnlog=dsn
	    dsn=dsn(1:idelim-1)

	if (idelim.lt.96 .and. dsnlog(idelim+1:idelim+1).eq.'>') then
		  append=.true.
		  idelim=idelim+1
c		  write(*,*) 'trouve >>'  
		else 
           append=.false.		
		endif 
		dsnlog=dsnlog(idelim+1:96)
		dsnlog=dsnlog(idelim+1:96)
			
		write(*,*) 'dsnlog(2):',dsnlog
	  else
c       write(*,*) 'pas de direction:',dsn
	  if (dsn(1:1).ne.'*' ) then

c	  write(*,*)'Ok.'
	  ifin=len_trim(dsn)
     	dsnlog=dsn(1:ifin)//'.txt'
		else
     	dsnlog='slcsv2017.txt'
		endif
      endif 		
c      write(*,*)'endif supprime'
c	  endif
c      write(*,*)'dsnlog==',dsnlog,'==='
	  ifin=len_trim(dsn)

	  write(*,*) 'verifier si existe: ',dsnlog
	  inquire(file=dsnlog,exist=lestla)
	  if (append .and. lestla) then
	   write(*,*)'ouverture (4) append de ' , dsnlog(1:ifin)
      open (66,file=dsnlog,access='append',err=10301)
	  goto 10302
10301  continue
         write(*,*)'erreur append ',dsnlog(1:ifin)
		 call pose(7)
10302  continue
      imp=66
	  write(*,*)'log dirilog dirigee a la fin de: ',dsnlog
	  else 
	  inquire(file=dsnlog,exist=lestla)
	   if (lestla) then
	     write(*,*)'ATTENTION, le fichier "',trim(dsnlog),'"'
		 write(*,*) 'existe deja, repondre O pour le remplacer.'
c confirm
       write(*,*) ' RSVP  O(ui) ou N(on)'
 
      read(*,'(a1)',end=10390,err=10390) acmd
	  goto 10391
10390 continue
      write(*,*) 'fin de fichier/ereur en question log'
      write(imp,*) 'fin de fichier/ereur en question log'
	  call pose(1039)
10391 continue
c pas ouvert!
c       write(imp,*)acmd
      call upcas1(acmd)
      if (acmd.ne. 'O') then 
         write(*,*)'Abandon!'
c	     write(imp,*)'Abandon!'
		 call pose(545)
		 endif
       endif	   
	      write(*,*) 'ouvrir en ecriture le fichier log: ' , dsnlog
        open (66,file=dsnlog,err=10305)
		imp=66
		goto 10306
10305  continue
         write(*,*)'erreur ouverture (2)',dsnlog(1:ifin)
		 call pose(8)
10306  continue
	  endif
c    	write(*,*) 'entete sur imp=',imp
       call stamp(imp,0)
c		write(*,*)'ifin=',ifin
      write(imp, 3001)
	  write (imp,*)ctitre
      write(imp,*)
      write(imp,*) 'en général, vous pourrez obtenir de l''aide en'
     1 , ' frappant ?<return>'
	  write(imp,*) 'fichier préparé: ',dsn
	  write(imp,*) 'listing: ',dsnlog
	  
	  
      write(*,*) '(1)Log dirig‚e vers ' , dsnlog
	  inquire(file=dsnlog,exist=lestla)
	  if (append .and. lestla) then
	   write(*,*)'ouverture (3) append de ' , dsnlog
	   
      open (imp,file=dsnlog,access='append',err=10303)
	  goto 10304
10303  continue
       write(*,*)'erreur append ',dsnlog(1:ifin)
	     call pose(9)
10304  continue	  
      call stamp(imp,0)
      write(*,*) 'Log dirig‚e a la fin de: ' , dsnlog(1:ifin)
      write(imp,*) 'Log dirigée à la fin ' , dsnlog(1:ifin)
	  endif
      goto 71099
71098 write(*,*) 'fin de fichier, on abandonne'
      if (imp.gt.0)write(imp,*) 'on abandonne'
      call pose (10)
71099 continue
      lca=3
      read(dsn,'(A1)') tlu(1)
      if (tlu(1).eq.'*') lca=mae
c      if (tlu(1).ne.'*') goto 3101
c     goto 3102
3101      continue     
c lecture de la carte date (une seule par execution )
       if (lca .ne. mae) then 
      inquire(file=dsn,exist=lexist)
      if (lexist) goto 3007
      write(*,*) 'fichier "',dsn(1:ifin),'" pas trouv‚'
      write(imp,*) 'fichier "',dsn(1:ifin),'" pas trouv‚'
c	  write(*,*)'ctitre raz ' , ctitre
	  ctitre=''
      call pose(11)

3007   continue
c      write(*,*)'fichier prepare:',dsn
      open(lca,file=dsn,form='formatted',status='old',err=10307)
	  goto 10308
10307  continue
        write(*,*)'erreur ouverture ',dsn
	   write(imp,*)'erreur ouverture ',dsn
		 call pose(12)
10308  continue	  
      write(*,*) 'ok ouverture pour lecture: ',dsn
c fin du lca ne mae 
      endif
c
       prototype= 
     1 'LOEVINGR: CHEMIN: , DATE: , RAPPEL: , TITRE , LABELS ;'
       call anal(lca,
     1 prototype,len_trim(prototype),
     2  tsave,asave,5,fini,.true.)

c        write(*,*) 'cpo retour de anal loevingr imp=',imp
       chemin=asave(1)
c	   write(*,*)'chemin="',chemin,'"'
c	   write(*,*)'chemin="',trim(chemin),'"'
	   if (chemin(8:8).eq.'.') then 
	     chemin80=dsn
		 write(*,*)'chemin80=',chemin80
		 else
	   chemin80=chemin	 
	   endif
       cche=chemin
       idd=asave(2)
       arappl=asave(3)
c	   write(*,*)  'PO: idd ===',idd,'===='
	   cdate=idd
       fotitr=tsave(4).ne.0
c       write(*,*) 'fotitr=',fotitr
	  if(tsave(5).ne.0)then
	    uselabels=.true.
			   write(*,*) 'tsave(5) uselabels:',uselabels

		endif
       read(chemin,'(8a1)') (tlu(jlu),jlu=1,8)
       do 61011 jfchmn=1,8
       if (tlu(jfchmn) .ne. ' ') goto 61012
61011   continue
c           fuul espace nom
       jfchmn=8
       goto 61014
61012  continue
c                non   espace trouve  reecrire en tete
       if (jfchmn .gt. 1)
     a  write(chemin,'(8a1)') (tlu(ifchmn),ifchmn=jfchmn,8),
     1  (' ',ifchmn=1,jfchmn-1)
       jfchmn=9-jfchmn
c cas ordinaire
       goto 61013
c espace en pposition 1
61014   write(*,*) 'pas de CHEMIN= ,sauvegarde impossible'
        write(imp,*) 'pas de CHEMIN= ,sauvegarde impossible'
        goto 61013
61013   continue
c        write(*,*) 'jfchmn=',jfchmn,'====',chemin,'===='
C                              ******
C                                **
C               
c 20171008 671 pas utilise                 **
c671   CONTINUE

c        write(*,*) 'PO: d r ou b?'
C       ********************     D   R   OU  B
      nob=1
c      write(*,*) 'PO: arappel======',arappl,'======='
      if (arappl.eq.'        ') GO TO 66
c                    12345678
c   
c 20180125 pa s close                                 reprise
c      close(lca)
c	  write(*,*) 'fermeture de 3 rappel lca=',lca
      read(arappl,'(a1,6x,a1)') tlu(1),tlu(2)
c      write(*,*) 'on va faire un read interne dans ====',chemin,'==='
      read(chemin,'(8a1)') (tlu(jche),jche=1,8)
c      write(*,*) 'on va faire un read interne dans ====',arappl,'==='
      read(arappl    ,'(5x,3a1)') (tlu(jche),jche=9,11)
	  if (tlu(9).eq.'.') tlu(9)=' '
      idf=9
      if (tlu(9).eq. ' ') idf=10
      if (tlu(10).eq. ' ') idf=11
      write(dsn,'(12a1)')( tlu(jche),jche=1,jfchmn),'.',
     1 (tlu(jche),jche=idf,11)
50372 continue
      write(*,*) 'PO: lecture de "',trim(dsn),'"'
      dsnlu=dsn
c $$$$$  err 
      inquire(file=trim(dsn),exist=lexist)
      if (lexist) goto 50732
      write(*,*) 'fichier (2)"',trim(dsn),'" pas trouv‚'
      write(imp,*) 'fichier "',trim(dsn),'" pas trouvé'
       call pose(13)
      
50732 continue
	   write(*,*) 'lecture de la sauvegarde ',dsn
      open(10,file=dsn,form='unformatted',status='old',err=50731)
	  goto 50744
50731 continue
      write(*,*)'erreur ouverture ',dsn
      write(imp,*)'erreur ouverture ',dsn
      call pose(401)	  
50733 continue
      write(*,*)'ligne:',ipt,' erreur lecture ',dsn
      write(imp,*)'ligne:',ipt,' erreur lecture ',dsn
      call pose(402)	  
50799 continue
      write(*,*)'erreur lire K ligne:',ipt,' fin fi labels ',dsn
      write(imp,*)'erreur lire K ligne:',ipt,' fin fi labels ',dsn
      call pose(403)	  
50744 continue
      write(*,*)'Ok.'
c   ignorer la signature
c ignorier ligne sauveagrde	
      ipt=0 
       read(10,err=50733)
	   ipt=ipt+1
       read(10,err=50733)
	   ipt=ipt+1
      READ(10,err=50733)NBI,NBQ,NBIT,partil,absents
	  nalire=nbit*(nbit+1)/2
c	  write(*,*) 'relu nbit=',nbit,nalire
	   ipt=ipt+1
	  read(10,err=50733)chemin,cdate,imp,dsn,dsnlog,ctitre
	   ipt=ipt+1
      read(10,err=50733) (pp(jo),jo=1,nalire ) 
	   ipt=ipt+1
      read(10,err=50733)(mak(jo),mqn(jo),mqu(jo),mx(jo),x(jo),jo=1,nbq)
	   ipt=ipt+1
      read(10,err=50733) NBPAS,nitp,p,ipop,iwi,ici,ic,iw
	  write(*,*) '‚chelle recharg‚e depuis la sauvegarde:',dsn
	   write(*,*) 'dsnlog:',dsnlog
	   write(*,*)'ctitre=',ctitre
c
      write(*,*) 'chemin=',chemin,'    date=',cdate
      cche=chemin
	   ipt=ipt+1
  	   read(10,err=50733)
     1 (numeit(jo),iqorit(jo),iait(jo),
     2 (lv(jo,jjp1),jjp1=1,101),jo=1,nbit)
c lecture epilogue	
	   ipt=ipt+1
c lecture ligne signature basse
       read(10,err=50733)
    	
c 20121211 ajout lecture param format
c initialiser a defaut pour si ancien keyword
       col5=.true.
	   inter2=.true.
	   larg=80
	   uselabels=.false.
	   labeli=.false.
	   labelq=.false.
	   labelr=.false.
	   ipt=ipt+1
       read(10,err=50739,end=50739)
     1 col5,inter2,larg,uselabels,usequestions,labeli,
     2 labelr,labelq,ls,nitp
c	   write(*,*)'larg=',larg,' ls=',ls
       
	   read(10) (qq(j),j=1,nbq)
	   if (uselabels)then
	   read(10)ildoitun
	   do 50722 iiq=1,nbq
       do 50722 jjq=1,10
	   ipt=ipt+1
	   read(10,err=50733,end=50799) x1(iiq,jjq),x2(iiq,jjq)
	   read(10,err=50733,end=50799) labelsprt(iiq,jjq)
	   ipt=ipt+1
	   read(10,err=50733,end=50799) labelsscr(iiq,jjq)
	   
50722  continue	 
       write(*,*)'ok labels'  
	   endif
	   
	   	   if (usequestions) then
	     do 50782 iiq=1,nbq
		 
		 read(10,err=50733,end=50799)questionsUprt(iiq)
		 read(10,err=50733,end=50799)questionsUscr(iiq)
		 read(10,err=50733,end=50799)questionsXprt(iiq)
		 read(10,err=50733,end=50799)questionsXscr(iiq)
		 
50782   continue
	   
	   endif

c 20171228 21h00 
         read(10,err=50733,end=50799)chemin80
	   

       goto 50738
50739  write(*,*)'lu ancien K parametres affichage non recharges'
       col5=.true.
	   inter2=.true.
	   larg=80
	   ls=80
	   uselabels=.false.
	   labeli=.false.
	   labelq=.false.
	   labelr=.false.
       goto 50738	   
50738  continue	   
C
       close( 10,err=50733)
       fsaved=.true.
c	   	  write(*,*) 'fsaved init true'
c 20171122 ouvrir imp en append
c       write(*,*)'imp=',imp
		imp=0
        open(66,file=dsnlog,access='append',err=20161)
		imp=66
		goto 20160
20161   continue
        write(*,*)'Erreur en ouvrant append ' , dsnlog
		dsnlog=''
		call pose(87)
20160   continue
c		 write(*,*) 'ouverture(5) append de ',dsnlog
		 write(*,*) 'fichier Log ‚crit à la suite de ',dsnlog
       write(imp,*) '-------------------------'
C *******************
       NBPAS=NBPAS+1
c	   write(*,*)'nbpas now ',nbpas
c        write(*,*) 'suite, passage ipa:',ipa
        write(*,*) 'suite, passage cdate:',cdate
c        write(imp,*) 'suite, passage:',ipa
        CALL STPA(1,IPA,imp)
      WRITE(*,1010) cdate       ,NBPAS
      WRITE(IMP,1010) cdate       ,NBPAS
1010   FORMAT(' SUITE DU    '     , A8,I3,' EME PASSAGE   ');
       nbord=0
	   lca=0
       GO TO 203
C                              ******
C                               ****
C               * * * * * * * * * * * * NOUVELLE ECHELLE  ( D )
C                                **
66    CONTINUE
c lecture carte echelle a mot clef
c nbit sera cumule depuis les items
       titre=' '
       ctitre=titre
	   nerritem=0
       if (.not. fotitr) goto 66066
      call lianum('TITR',lca,'TITRE: ...;',i4rep,20,a4rep,20,nob)
      write(trav80,'(20a4)') (a4rep(jt),jt=1,20)
      read(trav80,'(a80)') titre
c      write(*,*) 'titre lu =',titre
      ctitre=titre
66066  continue
c essai titre inattendu
c       write(*,*)' prochaine est titre ?'
	   call testun(11,lca,proch,'titre ou echelle;')
c        write(*,*) 'proch:',proch
		if (proch.eq.'/*      ') then 
		  write(*,*) 'ABANDON cause fin prematuree du fichier :',dsn
		  write(imp,*) 'ABANDON cause fin prematuree du fichier :',dsn
          call pose(617)
       endif		  
	   if (proch.eq.'TITRE') then
c	     write(*,*) 'touve titre'
      call lianum('TITR',lca,'TITRE: ...;',i4rep,20,a4rep,20,nob)
      write(trav80,'(20a4)') (a4rep(jt),jt=1,20)
      read(trav80,'(a80)') titre
c      write(*,*) 'titre lu =',titre
      ctitre=titre
      else
	  endif
c ajouter question
       prototype= 'ECHELLE: IMPIT,COLN,LS=,DICT,LABELS,QUESTION,PCT=,'//
     1 'F:,V=,NEUFNEUF:,VIDE:;'
c neufneuf et vide reponse alpha pour savoir si présent     
       call anal(lca,prototype,len_trim(prototype),
     2  tsave,asave,11,fini,.true.)
c	   write(*,*) 'renevu de anal(2)'
      if (fini) then
      write(*,*)'***** fin de fichier en lecture echelle'
      write(imp,*)'***** fin de fichier en lecture echelle'
       call pose(14)
      endif
c récupération des paramètres	  
      IMPIT= tsave(1).ne.0
	  COLN=tsave(2).ne.0
  	  ls=tsave(3)
      listdict=tsave(4).ne.0
	  uselabels=.false.
      usequestions=.false.		
c gaffe foireux lables implique question!
	  if(tsave(6).ne.0)then
	    usequestions=.true.
		endif
	  if(tsave(5).ne.0)then
	    uselabels=.true.
		usequestions=.true.
		endif
      ipct=0		
	  if(tsave(7).ne.0)then
	    ipct=tsave(7)
c		write(*,*)'ipct=',ipct
		endif
		
c filtre   
      if (asave(8).ne.'') then
		  ifiltre=tsave(9)
	      write(*,*)'FILTRER PAR ',asave(8),"=",ifiltre
		  qfiltre=asave(8)
	  endif
   
c neufneuf et vide global
      neufneufg=-1
	  if (len_trim(asave(10)) .gt.0 ) then
	  neufneufg=0
	  do 715 ineuf=1,len_trim(asave(10))
	  i9=ichar(asave(10)(ineuf:ineuf))
	  if (i9.ne.32 )then
	  if (i9.lt.ichar('0') .or. i9.gt.ichar('9')) then 
	    write(*,*)'erreur pas chiffre cidessus'
		write(imp,*)'erreur pas chiffre cidessus'
		call pose(7151)
	  endif
	  neufneufg=10*neufneufg+i9-ichar('0')
	  end if
715   continue
	  end if
c  vide global
      nrg=-1
	  if (len_trim(asave(11)) .gt.0 ) then
	  nrg=0
	  do 7152 ineuf=1,len_trim(asave(11))
	  i9=ichar(asave(11)(ineuf:ineuf))
	  if (i9.ne.32 )then
	  if (i9.lt.ichar('0') .or. i9.gt.ichar('9')) then 
	    write(*,*)'erreur pas chiffre cidessus'
		write(imp,*)'erreur pas chiffre cidessus'
		call pose(7152)
	  endif
	  nrg=10*nrg+i9-ichar('0')
	  end if
7152   continue
	  end if

      write(*,*) 'global neufneuf=',neufneufg,'  vide=',nrg
	  if (nrg.ge.0) then
	      do 7153 jrq=1,99
          nr(jrq)=nrg
7153    continue
	  endif

	  if (neufneufg.ge.0) then
	      do 7154 jrq=1,99
          neufneuf(jrq)=neufneufg
7154    continue
	  endif


		  
      if (.false.) then 
c now dict possible sans question
      if (listdict .and. .not. usequestions) then
	    write(*,*)'sur la carte ECHELLE: DICT exige QUESTION'
	    write(imp,*)'sur la carte ECHELLE: DICT exige QUESTION'
		call pose(543)
		endif
       endif
c            .false.
	  if (ls.eq.0) ls=80
c	  write(*,*)'largeur ',ls
	  ndecount=999999999
	  nstop=999999999
      LISTQ=0
	  LISTITEM=0
6609  continue
c boucle sur les options	  
      call testun(5,lca,proch,'option?;')
c      write(*,*) 'reponse ',proch
		    if (proch.EQ.'SKIP') then
          call lianum('NUME',lca,
     1 'SKIP: ;',i4rep,20,a4rep,20,nob)
		nskip=i4rep(1)
        goto 6609		
		endif
		    if (proch.EQ.'LISTQ')then 
	
          call lianum('NUME',lca,
     1 'LISTQ: ;',i4rep,20,a4rep,20,nob)
		listq=i4rep(1)
        goto 6609		
		endif
		
		
		    if (proch.EQ.'LISTITEM') then
          call lianum('NUME',lca,
     1 'LISTITEM: ;',i4rep,20,a4rep,20,nob)
		LISTITEM=i4rep(1)
        goto 6609		
		endif
 
		    if (proch.EQ.'STOPAFTR') then
c		write(*,*) 'stopliste touve lire compte'
          call lianum('NUME',lca,
     1 'STOPAFTR: ;',i4rep,20,a4rep,20,nob)
		nstop=i4rep(1)
c		write(*,*) 'bndecount ',ndecount
        goto 6609		
		endif

	    if (proch.EQ.'STOPLIST') then
c		write(*,*) 'stoplist touve lire comptge'
          call lianum('NUME',lca,
     1 'STOPLIST: ;',i4rep,20,a4rep,20,nob)
		ndecount=i4rep(1)
c²		write(*,*) 'bndecount ',ndecount
        goto 6609		
		endif
c  fin des d'option 
c	  write(*,*)'fini, options:',impit,listq,listitem
	  
	  
c
c
c                 pour eviter controle
c      write(*,*) 'contruction nouvelle echelle'
c      write(*,*) 'lecture des question '
      nob=1
      call lianum('TITR',lca,'INFILE: ...;',i4rep,20,a4rep,20,nob)
c convertir en string 1
c      write(*,*) 'retour de lianum titre nob=',nob
      write(trav80,'(20a4)') (a4rep(jt),jt=1,20)
      read(trav80,'(a80)') dsndat
c      write(*,*)'dsndat:',dsndat
c       write(*,*)' long:',len_trim(dsndat)
	  
c voir si.csv en fin de ligne
      ifincsv=80
	  csv=.false.
761	  if (dsndat(ifincsv:ifincsv).ne.' ') goto 76
	  ifincsv=ifincsv-1
	  if (ifincsv.gt.1) goto 761
76    continue
c      write(*,*) 'lon nom dsndat:',ifincsv

c      write(*,*) 'lon nom dsndat:',dsndat(ifincsv-3:ifincsv)

      if (dsndat(ifincsv-3:ifincsv).eq.'.CSV' ) then
      write(*,*) 'Lecture d''un fichier .CSV'
	  csv=.true.
      endif
	   
 	  
c      write(*,*) 'fin de dsndat'
      nbq=0
       nbit=0
c pb ci dessous si ....
      IF( MOD(I,25).EQ.0.AND.IMPIT)CALL STPA(2,IPA,imp)
c70     continue
c      write(*,*) 'est ce une question ?'
      call testun(2,lca,proch,'QUESTION ou autre;')
70000 ia=' '
c      write(*,*)'en 70000'
      if (proch.ne.'QUESTION') goto 70999
c encore une question
c      write(*,*) 'oui'
c NR dit en quoi envoyer la valeur vide par défaut 0!
      prototype=
     2 'QUESTION: NOM:,CSVX10:,CSV:,COLX10=,COL=,MAX=,VIDE:,CSVPLUS:'
	   prototype=trim(prototype) //',CSVSPLIT:,NEUFNEUF:;'
      call anal(lca,prototype,len_trim(prototype)
     1 ,tsave,asave,10,fini,.true.)
      if (fini) then 
     
      WRITE(*,'('' FIN DE FICHIER SUR UNITE LOGIQUE '',I3,
     1 '' EN LISANT QUESTION'')') LCA
      WRITE(imp,'('' FIN DE FICHIER SUR UNITE LOGIQUE '',I3,
     1 '' EN LISANT QUESTION'')') LCA
       call pose(15)
      
      endif
	  
c affichage de controle
c       do 66601 jlist=1,10
c66601 write(*,*) jlist,' ',tsave(jlist),' ',asave(jlist)
      if (nbq .gt. maxq) then
      write(*,*) 'le programme est limite a ',maxq,' questions'
      write(imp,*) 'le programme est limit‚ ™ ',maxq,' questions'
       call pose(16)
      endif
	  
      nbq=nbq+1
	  qq(nbq)=0
	  mx(nbq)=''
c      write(*,*) 'qq(',nbq,') mis a 0'	  
c RAZ tris a plat
      do 664 ir1=1,11
      x1(nbq,ir1)=0
      x2(nbq,ir1)=0
664   continue	  
      i=nbq
	  if (csv) then
c                  options pour csv 
c MAX= n'est pas utiloisé pour CSV
      mak(i)=0
c vide=
      if (len_trim(asave(7)).gt.0 .or. nrg.eq.0) then 

	  nrnum=0
	  do 6647 inrnum=1,len_trim(asave(7))
	  if (32.ne.ichar(asave(7)(inrnum:inrnum)))then
	  nrnum=10*nrnum+ichar(asave(7)(inrnum:inrnum))-ichar('0')
	  endif
6647  continue	  
      nr(i)=nrnum
	  if (nr(i).gt.11  .or. nr(i).lt.0) then 
	     write(*,*)'VIDE= limité a 0 a 11'
		 write(imp,*)'VIDE= limité à 0 a 11'
		 call pose(651)
		 endif
		 
	  endif	 
c isoler neufneuf
	  if (len_trim(asave(10)).gt.0 ) then
	  asave(10)=trim(asave(10))
	  nrnum=0
	  do 6643 inrnum=1,len_trim(asave(10))
	  if (32.ne.ichar(asave(10)(inrnum:inrnum)))then
	  nrnum=10*nrnum+ichar(asave(10)(inrnum:inrnum))-ichar('0')
	  endif
6643  continue	  
	  NEUFNEUF(i)=nrnum
	  if (neufneuf(i).gt.11  .or. neufneuf(i).lt.0) then 
	     write(*,*)'NEUFNEUF= limité a 0 a 11'
		 write(imp,*)'NEUFNEUF= limité à 0 a 11'
		 call pose(652)
		 endif
c	  write(*,*) 'code 99 doit etre lu comme ',neufneuf(i),asave(10)
	  endif
c      write(*,*)'q:',i,' nr recode en ',nr(i)	  
	  else
	  if (tsave(3).ne.0) then 
	     write(*,*) 'MAX= est obsolete pour csv, ignoré.'
         endif
		 mak(i)=0
c
      endif
      nbitdq=0
c traitement de NOM:
c supprimer espaces à gauche
	  mqzun=asave(1)
	  deux=.false.
      mqn(i)=ltrim(mqzun)	  
c	  write(*,*) 'NOM(',i,')=',mqn(i)


c traitement de CSVX10:
	  mqzun=asave(2)
      if (len_trim(mqzun).gt.0)then     
	  mx(i)=ltrim(mqzun)
c	 	  write(*,*)'mx(i)=',mx(i),'='

	 	 x(i)=.true.
c	  write(*,*) ' x(i) true csvx10=',mx(i)
	  deux=.true.
	  endif
	  
c traitement de CSVPLUS:
c  supprimer espaces à gauche
c      write(*,*)'traiter csvplus'
	  mqzun=asave(8)
	  if (len_trim(mqzun).gt.0)then
	  if (len_trim(mx(i)).gt.0 .and.len_trim(mqzun).gt.0)then
	     write(*,*) 'CSVPLUS incompatible avec CSVX10'
		 write(*,*)'=',mx(i),'=',len_trim(mx(i))
	     write(imp,*) 'CSVPLUS incompatible avec CSVX10'
		 call pose(181)
	  endif
	  mx(i)=ltrim(mqzun)
c	  write(*,*)'mx(i)=',mx(i),'='
	  x(i)=.false.
	  s(i)=.false.
c 	  write(*,*)' s(i ) a false'
	  deux=.true.
	  endif
	  
c traitement de CSVSPLIT:
c  supprimer espaces à gauche
c      write(*,*)'traiter csvplus'
	  mqzun=asave(9)
	  if (len_trim(mqzun).gt.0)then
	  if (len_trim(mx(i)).gt.0 .and.len_trim(mqzun).gt.0)then
	     write(*,*) 'CSVSPLIT incompatible avec CSVX10='
     1 ,mx(i),'='
	     write(imp,*) 'CSVSPLIT incompatible avec CSVX10='
     1 ,mx(i),'='
	 	 call pose(181)
	  endif
	  mx(i)=ltrim(mqzun)
	  
c	  write(*,*)'mx(i)=',mx(i),'='
	  x(i)=.false.
	  s(i)=.true.
c	  write(*,*)' s(i ) a true'
	  deux=.true.
	  endif
	  
c  traitement de CSV=	  
c  supprimer espaces à gauche
	  mqzun=asave(3)

      if (len_trim(mqzun).eq.0) then
	   write(*,*)'CSV= oblicatoire'
	   write(imp,*)'CSV= oblicatoire'
	   call pose(719)
	   endif
	  mqu(i)=ltrim(mqzun)

	  
      if (csv) then 
	  lircol(i)=0
	  lircolx(i)=-1
	  if (mx(i).ne.'        ') lircolx(i)=0
c	  write(*,*) 'lircol ',lircol(i),lircolx(i)
	  else
	  lircol(i)=tsave(2)
	  lircolx(i)=0
      if (lircol(i) .gt.0) goto 66637
      write(*,*) 'code col= manquant ou nul ci dessus'
      write(imp,*) 'code col= manquant ou nul ci dessus'
       call pose(19)

66637 continue
      endif
c  ci dessus fin de if csv/else	  
	  if (lircol(i) .gt. minlrl) minlrl=lircol(i)
c mqz passe en alpha


c ici vien la lecture des items deduits de la question
70777 continue
c      write(*,*)'call testun'
      call testun(1,lca,proch,'ITEM, QUESTION ou autre;')
c      write(*,*)' item ou pas: ',proch
      ia=' '
      if (proch.ne.'ITEM') goto 70888
c encore une question
      if (nbit .lt. maxit) goto 70666
      write(*,*) 'le programme est limit‚ ™ ',maxit,' items'
      write(imp,*) 'le programme est limit‚ ™ ',maxit,' items'
       call pose(20)

70666 continue
      nbit=nbit+1
      nob=1
      nob=2
c	  write(*,*)'lianum item'
      call lianum('NUME',lca,
     1 'ITEM: ;',i4rep,20,a4rep,20,nob)
      if (i4rep(1) .le. 0) then
      write(*,*) 'numero item nul ou manquant ci dessus'
      write(imp,*) 'numero item nul ou manquant ci dessus'
       call pose(21)
      endif
	  
      if (i4rep(1) .gt. 99) then
      write(*,*) 'numero item >99 ci dessus'
      write(imp,*) 'numero item >99 ci dessus'
       call pose(21)
	   endif

      numeit(nbit)=i4rep(1)
c                                   verifier dupplication
      if (nbit .le. 1) goto 70706
      do 70709 jbit=1,nbit-1
      if (numeit(nbit) .eq. numeit(jbit)) goto 70708
70709 continue
      goto 70706
70708 continue
      write(*,*) 'numero d''item duppliqu‚ ci dessus'
      write(imp,*) 'numero d''item duppliqué ci dessus'
       call pose(22)

70706  continue
c                          question origine de l'item
      iqorit(nbit)=i
c initialise à non les code plus de l'item
      do 72089 icode = 1,101
      lv(nbit,icode)=.false.
72089 continue
c verifier les codes lus
c ????? pourquoi 2 et nob c'est quio
c le 1 c'st le numéro de l'item
      do 72091 icode = 2 , nob
	  if (deux) then
      if (i4rep(icode).gt.99 ) then 
	      write(*,*) 'hors code deux!',i4rep(icode)
	      write(imp,*) 'hors code X10!',i4rep(icode)
		  call pose(23)
		  stop
		  endif
	  else
c       donc pas deux !
      if (i4rep(icode).ge.M1 ) then 
	      write(*,*) 'hors code else !',i4rep(icode)
 	      write(imp,*) 'hors code else!',i4rep(icode)
c		  call pose(24)
c		  stop
		  endif
	  end if	  
c         double:else	  
      if (i4rep(icode) .ge. 0 .and. i4rep(icode).le.100) then
      if( lv(nbit,i4rep(icode)+1)) then
	      write(*,*) 'code repete: ',i4rep(icode)
	      write(imp,*) 'code repete: ',i4rep(icode)
		  nerritem=nerritem+1
	  endif
       lv(nbit,i4rep(icode)+1)=.true.
c	   write(*,*) 'lv(',nbit,',',i4rep(icode)+1,'=true'
	   end if
72091  continue
c       write(*,*)'fin liste des codes'
       goto 70777
70888  continue
c                on a une suivante non item
c        write(*,*)' suite de item pas item'
c ancine 70 sur question
      goto 70000
70999 continue
c      write(*,*) 'pas question'
      if (nerritem.gt.0) then 
	     write(*,*) 'Abandon cause erreur item ci dessus'
		 write(imp,*) 'Abandon cause erreur item ci dessus'
		 call pose(25)
	  endif	 
c	  write(*,*) 'si fin de fichier fudrait ^passer à suite'
      if (proch.eq.'C') goto 70444
      if (proch.eq.'COMPTER') goto 70444
	  if (proch.eq.'/*') goto 70444
	  
	  write(*,*) 'commande inconnue: ', proch,' abandon'
	  write(imp,*) 'commande inconnue: ', proch,' abandon'
	  call pose(26)

	  
70444 write(*,*) 'trouve  C:;, COMPTER:; ou fin de fichier, on charge'
	  write(imp,*)'trouvé  C:;, COMPTER:; fin de fichier, on charge'
	  write(*,*)'recapitualiton des colonnes utilisees'
	  write(imp,*)'récapitualiton des colonnes utilisées'
	  nbmult=0
	  do 7621 iiq=1,nbq
c	   write(*,*)iiq,mx(iiq),x(iiq)
	   a8(iiq)='        '
	  if (mx(iiq).ne.'        ')then
	  nbmult=nbmult+1
      	if (x(iiq))then
c		a8(iiq)='*10+'
		a8(iiq)='+10*'
		else
		if (s(iiq)) then
		  a8(iiq)='|'
		  else
		a8(iiq)='+'
		  endif
		endif
		endif
c       write(*,*)s(iiq),mqn(iiq),' A8(',iiq,') initialisé ',a8(iiq)
7621   continue

c liste x1
      do 702 iiq=1,nbq
	  
c 
		 
c pas utile x()
c      write(*,*) iiq,' ',mqn(iiq) , x(iiq),mqu(iiq),a8(iiq),mx(iiq)
c      write(imp,*) iiq,' ',mqn(iiq) , x(iiq),mqu(iiq),a8(iiq),mx(iiq)
      write(*,*) iiq,' ',mqn(iiq) , ' ',mqu(iiq),a8(iiq),mx(iiq)
      write(imp,*) iiq,' ',mqn(iiq) , ' ' ,mqu(iiq),a8(iiq),mx(iiq)
702   continue	
c  
c iait : item présent
      DO 92 I=1,MAXIT
92    IAIT(I)=.FALSE.
c
c ici etait ancienne boucle sur les items
      if (minlrl.eq.0) then 
c      write(*,*) 'Comptage en cours ...., lecture de .csv'
c      write(imp,*) 'Comptage en cours ...., lecture de .csv'
	  else
c      write(*,*) 'Comptage en cours ...., lecture de ',minlrl,' col.'
c      write(imp,*) 'Comptage en cours ...., lecture de ',minlrl,' col.'
	  endif
      nbi=0
	  nbign=0
	  nlu=0
	  indivenerreur=0
c ouverture fichier
c      write(*,*)'dsn:',dsn
	  ifo=index(dsn,'\',.true.)
	  dsndat=dsn(1:ifo)//dsndat
c	  write(*,*)dsndat
c	  write(imp,*)dsndat
      inquire(file=dsndat,exist=lexist)
      if (lexist) goto 70997
      write(*,*) 'fichier (3)"',trim(dsndat),'" pas trouv‚'
      write(imp,*) 'fichier "',trim(dsndat),'" pas trouvé.'
       call pose(27)

70997 continue
5511  format(a,1h")
      write(*,*)'lecture du fichier ind .csv: "',trim(dsndat),'"'
      write(imp,*)'lecture du fichier  ind .csv: "',trim(dsndat),'"'
      if (minlrl .le. maxlig) goto 70991
      write(*,*) 'la longueur de ligne est limit‚e ™ ',maxlig
      write(imp,*) 'la longueur de ligne est limitée ™ ',maxlig
       call pose(28)

70991 continue
        write(*,*) 'Ouverture du fichier (1): "',trim(dsndat),'"'
        write(imp,*) 'Ouverture du fichier: "',trim(dsndat),'"'
      open(10,action='read',file=dsndat,form='formatted')
c      open(90,action='read',file=dsndat,form='formatted')
       write(*,*) 'OK.   '
       write(imp,*) 'OK.   '
c                    sera incremente
      NBNB=(NBIT*(NBIT+1))/2
c	  write(*,*)' uselabels=',uselabels,' labelq=',labelq
	 
     
	 if (uselabels )then
	  dsnlab=dsndat(1:len_trim(dsndat)-4)//'.labels.csv'
        write(*,*) 'Ouverture des libelles: "',trim(dsnlab),'"'
        write(imp,*) 'Ouverture des libellés: "',trim(dsnlab),'"'
	  

	  inquire(file=dsnlab,exist=lestla)
	  if (lestla)then
	   write(*,*) 'Ok.'
	   write(imp,*)'Ok.'
	   else
	    write(*,*)'pas trouve fichier ',dsnlab
		write(imp,*)'pas trouve fichier ',dsnlab
	   write(*,*)'corriger ou oter LABELS de la carte LOEVINGR:'
       write(imp,*)'corriger ou oter LABELS de la carte LOEVINGR:'

	  call pose(700)
	  endif	
	
		  
     
        write(*,*) 'on va ouvrir "',trim(dsnlab),'"'
        write(imp,*) 'on va ouvrir "',trim(dsnlab),'"'
      open(11,file=dsnlab,form='formatted',err=70901)
	  goto 70902
70901 continue
      write(*,*) 'erreur en ouverture de  labels',dsnlab
      write(imp,*) 'erreur en ouverture de  labels',dsnlab
	  call pose(250)
70902 continue
       write(*,*) 'fichier des labels ouvert   '
       write(imp,*) 'fichier des labels ouvert   '
	 
	  endif
	  
	  
      DO 7099  J=1,NBNB
7099  PP(J)=0
c      write(*,*)'traz pp ',nbnb
c   --- boucle de lecture un individu
710   continue
c      write(*,*) 'en 710'
      if (nbi.eq.0 .and. nbign.eq.0) write(*,*)'lecture en cours....'
        if(nbi.ge.nstop) then 
		 write(*,*) 'STOPAFTR atteint:',nstop
		 write(imp,*) 'STOPAFTR atteint:',nstop
	     goto 700
		 endif
		 
		 
      if (csv) then
	  if (nbi.eq.0.and. nbign.eq.0) then
c -----  entree .csv lire entete puis questions.csv	  
	  write(*,*)'fichier .csv, lecture ligne en-tete'
	  	   write(*,*)'apparier CSV= avec ligne 1'
	   write(imp,*)'apparier CSV= avec ligne 1'

c   lecgture de la ligne titre var et rechercher c olonne	
c      write(*,*)'lire 10'
c	  write(imp,*)'lire 10'
	  read(10,'(a50000)',end=700) tlu1000
       write(*,*) 'lu entete .csv'
	  lonlu=len_trim(tlu1000)
      if (lonlu.ge.L5000) then
c je pourais dire 
	    write(*,*)'ligne .csv de plus de ',L5000,' car.'
	    write(imp,*)'ligne .csv de plus de ',L5000,' car.'
		call pose(111)
	  endif
c       write(*,*)'espere en tete ',trim(tlu1000)
c	   if(listq.gt.0)write(*,*)trim(tlu1000)

      if (uselabels)then
c	  write(*,*)'lire 11' 
c	  write(imp,*)'lire 11' 
	  read(11,'(a50000)',end=700) tlab1000
	  if (len_trim(tlab1000).ge.50000) then
	    write(*,*)'ligne label.csv de plus de 50000 car.'
	    write(imp,*)'ligne label.csv de plus de 50000 car.'
		call pose(110)
	  endif
c	  write(*,*)trim(tlab1000)
	  if (len_trim(tlab1000).ne.len_trim(tlu1000))then
	    write(*,*)len_trim(tlab1000),len_trim(tlu1000)
	    write(*,*)'lignes en-tete .csv et labels différentes'
		write(*,*)'.csv'
		write(*,*)trim(tlu1000)
		write(*,*)'.labels.csv'
		write(*,*)trim(tlab1000)
	    write(imp,*)len_trim(tlab1000),len_trim(tlu1000)
	    write(imp,*)'lignes en-tete .csv et labels différentes'
		write(imp,*)'.csv'
		write(imp,*)trim(tlu1000)
		write(imp,*)'.labels.csv'
		write(imp,*)trim(tlab1000)
		call pose(444)
	  endif
c      write(*,*)'libis',tlab1000
	  
	  do 7106 jlon=1,len_trim(tlab1000)
	    if (tlab1000(jlon:jlon).ne.tlu1000(jlon:jlon))then
	    write(*,*)'en-tete .csv et labels.csv  differentes a :',jlon
	    write(imp,*)'en-tete .csv et labels.csv  differentes a :',jlon
		call pose(445)
		endif
7106  continue	  
c verifier identique	   
	  write(*,*)'OK. (en-tete .csv et labels.csv identiques).'

      endif	  
c 20180124 déplacé le ok ci dessous
  	  write(*,*)'OK. (en-tete csv et labels.csv identiques).'
c     chercher dans tdict les mqz noms
	   icolbrut=0
       icherchemq=1
	   icolcsv=0
	   nbcolcsv=0
732	   mqdep=icherchemq
731	   continue
        if (tlu1000(icherchemq:icherchemq).eq.';')goto 73
       if(   icherchemq.gt.len_trim(tlu1000)) then
c ooops, pas de plus 1 car deja GT
c	     write(*,*) 'fin de ligne'
		 goto 73
		 endif
	   icherchemq=icherchemq+1
	   goto 731
73     continue
c       write(*,*) 'csv nom:',mqdep,' ' ,icherchemq,' ' ,
c     1 tlu1000(mqdep:icherchemq-1)	
c      checher si ce nom est dans les mqu ou mx

	   icolbrut=icolbrut+1
	   usedcol(icolbrut)=.false.
       colbrut(icolbrut)=tlu1000(mqdep:icherchemq-1)
       colup(icolbrut)=tlu1000(mqdep:icherchemq-1)
	   do 7399 icb=1,len(colup(icolbrut))
	   call upcase(colup(icolbrut)(icb:icb))
7399    continue
	   iprem=0
	   do 7373 ifi=1,8
	     if ((qfiltre(ifi:ifi).ne. ' ') .and. (iprem.eq.0))iprem=ifi
c	     write(*,*)'ich ',ifi,ichar(qfiltre(ifi:ifi))
7373   continue
       qfiltre=qfiltre(iprem:8)
c chercher si filtre

       if (trim(qfiltre).eq.'')then
	     write(*,*)'nof'
	   else
c²       write(*,*)'col b',icolbrut,'===',trim(colbrut(icolbrut)),'===',
c²²     1 trim(qfiltre),'==='

	   if (trim(colup(icolbrut)).eq.trim(qfiltre))then
	     nfiltre=icolbrut
		 write(*,*) 'qfiltre ',qfiltre,' en col csv:',nfiltre
	   end if	 
       endif	   
       do 737 iqz=1,nbq
c²	   write(*,*) '"',tlu1000(mqdep:icherchemq-1),'=!',mqu(iqz),'"'
c ***MQZX10 aussi
c convertir en maj
       do 7371 imaj=mqdep,icherchemq-1
        call upcas1(tlu1000(imaj:imaj))
7371   continue	   

       if (tlu1000(mqdep:icherchemq-1).eq.mqu(iqz)) then
c ici il faut noterle rang dans csv comme col
        usedcol(icolbrut)=.true.
		
c	   write(*,*)'usedcol (',icolbrut,') à:',iqz,' ',mqu(iqz),mx(iqz)
       lircol(iqz)=icolcsv+1
c   	  write(*,*)'(1)q#',iqz,': ',mqn(iqz) ,mqu(iqz),'             ',
c     1 lircol(iqz)
c	    write(imp,*) iqz,': ',mqn(iqz) ,mqu(iqz),'             ',
c     1 lircol(iqz)
 		end if
c		write(*,*) '?',mqzx10(iqz)
c	   write(*,*) '"',tlu1000(mqdep:icherchemq-1),'=?',mx(iqz),'"'


       if (tlu1000(mqdep:icherchemq-1).eq.mx(iqz)) then
c	   write(*,*)'==',tlu1000(mqdep:icherchemq-1),'=',mx(iqz)
c ici il faut noterle rang dans csv comme col
       if(mx(iqz).eq.'        ') then
c	   write(*,*)'pas de second csv'
	   lircolx(iqz)=0
c       write(*,*)'(2)q#',iqz,':x/+ ',mqn(iqz),' a (2)'
    
	   else
       usedcol(icolbrut)=.true.
	   write(*,*) 'usedcol',icolbrut
       lircolx(iqz)=icolcsv+1
c      write(*,*)'(3)q#',iqz,': ',mqn(iqz),mqu(iqz),' +/* ',
c     1 mx(iqz),lircol(iqz),lircolx(iqz)
c      write(imp,*)'q#',iqz,':+/* ',mqn(iqz),mqu(iqz),' +/* ',
c     1 mx(iqz),lircol(iqz),lircolx(iqz)
	   endif
 		end if
737    continue		
       icolcsv=icolcsv+1   
       icherchemq=icherchemq+1
	   if(icherchemq.lt.len_trim(tlu1000)) goto 732
735    continue
c      write(*,*) 'en tete finie'
c      verifier toutes question trouvee
       nbmanque=0
       do 7351 iqz=1,nbq
c	   write(*,*)'est la?',iqz,	mqn(iqz),mqu(iqz),lircol(iqz),' x',
c     1 mx(iqz),lircolx(iqz)
	   if (lircol(iqz).eq.0) then 
	    nbmanque=nbmanque+1
		write(*,*) 'question #',iqz,': ',mqn(iqz),' colonne:     ',
     1 mqu(iqz),' pas en ligne 1 du .csv'
		write(imp,*) 'question #',iqz,': ',mqn(iqz),' colonne:     ',
     1 mqu(iqz),' pas en ligne 1 du .csv'
		end if
c            eq 0		
        if (mx(iqz).ne.'        ')then
		if (csv .and. lircolx(iqz).eq.0) then 
	    nbmanque=nbmanque+1
		write(*,*) 'question #',iqz,' ' ,mqn(iqz),'  colonne +x| :',
     1 mx(iqz),' pas en ligne 1 du .csv'
		write(imp,*) 'question #',iqz,' ',mqn(iqz),'  colonne +x| :'
     1 ,mx(iqz),' pas en ligne 1 du .csv'
		end if
		endif
c             x10 0		
		
c VALIDER AUSSI MQZX10		
7351   continue	   
       if (nbmanque.gt.0 ) then
	   write(*,*)'colonnes trouvees sur la ligne en-tete du .csv:'
	   iprev=1
	   icol=0
	   
	   do 711 ipv=1,len_trim(tlu1000)
	   if (tlu1000(ipv:ipv).ne.';' .and. ipv.lt.len_trim(tlu1000)) 
     1 goto 711
	   icol=icol+1
	   write(*,*) icol,': ',tlu1000(iprev:ipv-1)
	   write(imp,*) icol,': ',tlu1000(iprev:ipv-1)
       iprev=ipv+1
711    continue
       
       if (iprev.eq.1)write(*,*) 'pas de ; ! bug' 
	    if (iprev.eq.1)write(imp,*) 'pas de ; ! bug' 
	   call pose(29)
	   end if
c             nbmanque	   
        write(*,*)'Ok (tous csv= dans ligne 1).'

		if (usequestions .or. listdict) then
		dsndat=trim(dsndat)
		dsnlibq=dsndat(1:len_trim(dsndat)-4)//'.questions.csv'
        write(*,*) 'on va ouvrir "',trim(dsnlibq),'"'
      write(imp,*)'dsnlibq="',trim(dsnlibq),'"'
        write(imp,*) 'on va ouvrir questionsr "',trim(dsnlibq),'"'
      open(12,file=dsnlibq,form='formatted',err=70905)
	  goto 70906
70905 continue
      write(*,*)'erreur ouverture questions "',trim(dsnlibq),'"'
      write(imp,*)'erreur ouverture questions "',trim(dsnlibq),'"'
	  call pose(251)
70906  continue
       write(*,*) 'fichier questions ouvert  	   '

c passer entete
c        write(*,*)'(3)labelq=',labelq
        Write(*,*)'lecture du fichier questions en cours'
        Write(imp,*)'lecture du fichier questions en cours'
        if (listdict) then
		  write(*,*)'questions:'
		  write(imp,*)'questions:'
		else
           write(*,*)'pour obtenir liste de toutes les questions: ',
     1 ' ajouter le mot-cle DICT sur carte ECHELLE:'		
           write(imp,*)'pour obtenir liste de toutes les questions: ',
     1 ' ajouter le mot-cle DICT sur carte ECHELLE:'		
		endif
c ignorer en tete et ligne 1
c $$$$$ bug ff		
c       write(*,*) 'va lire'
c 20181220 suorpirme un read et ligbtrut a 1
        read(12,63,end=632,err=632)
c        read(12,63,end=632,err=632)
63      format(1x)
c la première col n'est pas dans le dict
       irq=1
	   nbluq=0
	   ibrut=1
	   
c sans doute point de retour lecture si ignore
631    continue
c       write(*,*)'en 631'
       read(12,'(a50000)',end=632,err=632)tlu1000
	   
        if (len_trim(tlu1000).eq.0)goto 631
	   nbluq=nbluq+1
c       write(*,*)'luq ',nbluq,trim(tlu1000)   
	       irq=irq+1
c	   write(*,*) 'rang:',irq,' ',trim(tlu1000)
	   ipv=0
c explorer l000 ligne lu code;libelle
634    ipv=ipv+1
       if (tlu1000(ipv:ipv).ne.';'  
     1  .and.ipv.le.len_trim(tlu1000)) goto 634
	   if (listdict) then 
	      tscr=''
		  tprt=''
	      call accents(tlu1000,tscr,tprt)
	      write(*,*)irq,' ',trim(tscr)
		  write(imp,*)irq,' ',trim(tprt)
		  endif

		  if (ibrut.ge.L000)then
		  write(*,*)'max colonnes atteint:',L000
		  write(imp,*)'max colonnes atteint:',l000
		  call pose(543)
		  
		endif 
		
c ignorer ligne vides 20181212
1	
        ibrut=ibrut+1
	   ligbrut(ibrut)=	  tlu1000(1:ipv-1)
c²       write(*,*)'nomcsv en debiut ligne',ibrut,ligbrut(ibrut)
	   if (trim(ligbrut(ibrut)).ne.colbrut(ibrut))then
	   write(*,*)'erreur ligxcol ',ibrut, ' ',trim(ligbrut(ibrut)),
     1 '<>',colbrut(ibrut)
	   write(imp,*)'erreur ligxcol ',ibrut, ' ',trim(ligbrut(ibrut)),
     1 '<>',colbrut(ibrut)
	    endif
		
		if (usequestions) then 
c       write(*,*)'exploration des questions:'	,tlu1000(1:ipv-1)  

       do 6369 iilu=1,ipv-1
	   call upcase(tlu1000(iilu:iilu))
6369   continue
	   do 636 iqorig=1,nbq
	   if (tlu1000(1:ipv-1).eq.mqu(iqorig)) then
c     write(*,*)'mis ok ',mqu(iqorig),iqorig
         questionsU(iqorig)=tlu1000(ipv+1:len_trim(tlu1000))
         qeq(iqorig)=tlu1000(1:ipv-1)
         call accents(questionsU(iqorig),
     1 questionsUscr(iqorig) , questionsUprt(iqorig ))
c	     write(*,*)'decode accents'
c²	     write(*,*)iqorig,' ',mqu(iqorig),' ',questionsUscr(iqorig)
c	     write(imp,*)iqorig,' ',mqu(iqorig),' ',questionsUprt(iqorig)
	 
	  if (qeq(iqorig).ne.mqu(iqorig)) then 
	     write(*,*)' irq=',irq,' it:',iit,' erreur iq:',iqorit(iit)
     1 ,'nom=',qeq(iqorig),'<>col:',mqu(iqorig)
	     write(*,*) qeq(iqorig),'<>',mqu(iqorig)
	     write(imp,*)' irq=',irq,' it:',iit,' erreur iq:',iqorit(iit)
     1 ,'nom=',qeq(iqorig),'<>col:',mqu(iqorig)
	     write(imp,*) qeq(iqorig),'<>',mqu(iqorig)
		 call pose(260)
		 endif

		 
       endif
 	   if (tlu1000(1:ipv-1).eq.mx(iqorig)) then
         questionsX(iqorig)=tlu1000(ipv+1:len_trim(tlu1000))
         qeq(iqorig)=tlu1000(1:ipv-1)
         call accents(questionsX(iqorig),
     1 questionsXscr(iqorig) , questionsXprt(iqorig ))
	     write(*,*)iqorig,' ',mqu(iqorig),' ',questionsXscr(iqorig)
	    write(imp,*)iqorig,' ',mqu(iqorig),' ',questionsXprt(iqorig)
	 
	  if (qeq(iqorig).ne.mx(iqorig)) then 
	     write(*,*)' irq=',irq,' it:',iit,' erreur iq:',iqorit(iit)
     1 ,'nom=',qeq(iqorig),'<>col:',mqu(iqorig)
	     write(*,*) qeq(iqorig),'<>',mqu(iqorig)
	     write(imp,*)' irq=',irq,' it:',iit,' erreur iq:',iqorit(iit)
     1 ,'nom=',qeq(iqorig),'<>col:',mqu(iqorig)
	     write(imp,*) qeq(iqorig),'<>',mqu(iqorig)
		 call pose(260)
		 endif

		 
       endif
636     continue
	   
c²       goto 631
       endif 	
c        write(*,*)'retour 631'	   
	   goto 631
       
632    continue

       endif
c     usequestions ajout& 1212
       close(12)
	   
	   write(*,*)'verification tous csv= dans fichier questions.'
c seulement si usesquestions
      if(usequestions) then
c vérifier si toutes les colonnes ont eu une ligne question	  
        nbnoq=0 
       do 6331 iqo=1,nbq
	   if (len_trim(questionsU(iqo)).eq.200) then
	    write(*,*)'manque ligne question  pour csv=',mqu(iqo)
	    write(imp,*)'manque ligne question  pour csv=',mqu(iqo)
		 nbnoq=nbnoq+1
		 else
c	  write(*,*)len_trim(questionsUscr(iqo)),trim(questionsUscr(iqo))
c	  write(imp,*)len_trim(questionsUprt(iqo)),trim(questionsUprt(iqo))
		 endif
6331   continue
       if (nbnoq.gt.0) then

       write(*,*) 'fin du fichier questions (ou erreur) apres:',nbluq,
     1 ' lignes.'
	   write(imp,*) 'fin du fichier questions (ou erreur) après:',nbluq
     1 ,' lignes.'
	   
	   
		  call pose(6331)
		  endif 
       write(*,*)'Ok (questions).'	   

	   endif
       do 6361 icolx=1,icolbrut
c        if (usedcol(icolx)) write(*,*) 'used:',icolx,colbrut(icolx)
6361    continue

	   endif

   
	   
	   if (nbi.eq.0.and. nbign.eq.0) then
        write(*,*)'La lecture des individus peut demander ',
     1 '"un certain temps"'
		if (ipct.eq.0) then
        write(*,*)'(pour afficher la progression, coder PCT=nn ',
     1 'sur la carte ECHELLE:)'		  
	    end if
	    
		call stamp(0,0)

	   ipas=0
c	     write(*,*) 'ipct=',ipct
       if (ipct.gt.0) then
		write(*,*)'comptage des individus pour afficher %.'
		write(imp,*)'comptage des individus pour afficher %.'
	   nblifi=0
	   mxlifi=0
6701   continue
	     read(10,'(a50000)',end=6702)tlu1000
		 nblifi=nblifi+1
c		 write(*,*)nblifi
	   goto 6701
6702   continue
       write(*,*)'Ok, le fichier compte :',nblifi,' lignes'
       write(imp,*)'Ok, mle fichier compte :',nblifi,' lignes'
	   	  ipas=(nblifi*ipct)/100
c         write(*,*)'ipas=',ipas
	   close(10)
	   open(10,action='read',file=dsndat,form='formatted')
	   read(10,'(1x)')
       endif
c            pct
	   endif 
c nnnn     falqz
c  
6709   continue
c re entete	  
c lire la ligne courante
c      write(*,*)'lire 2 10'
c      write(*,*)'(5)labelq=',labelq


      if (nbi.eq.0 .and. nskip.gt.0.and. nbign.eq.0) then
	  write(*,*)'debut skip nlu=',nlu
	    do 812 jignore=1,nskip
c bug si nskipWnbi
		   read(10,'(a50000)',end=8121,err=8121)tlu1000

		   if (uselabels) read(11,'(a50000)',end=8121,err=8121)tlu1000
		     nlu=nlu+1 
812   continue
      goto 8122
8121   continue
       write(*,*)'fin de fichier ou erreur en skip nlu=',nlu
       write(imp,*)'fin de fichier ou erreur en skip nlu=',nlu
	   call pose(812)
8122   continue  
	    write(*,*)' skip:',nskip,'  nlu=',nlu
	    write(imp,*)' skip:',nskip
      endif	  
      if (ipct.gt.0) then
	  iact=nbi/ipas
	  if (nbi.gt.0 .and. nbi.eq.iact*ipas)  then
	    call stamp(imp,iact*ipct)
	  endif
	  endif
c     pct
  	  read(10,'(a50000)',end=700) tlu1000
c      write(*,*)' lu ',trim(tlu1000)  
c protection
       if (len_trim(tlu1000).gt.mxlifi) mxlifi=len_trim(tlu1000)
      if (len_trim(tlu1000).ge.50000) then
	    write(*,*)'ligne csv de plus de 50000 car.'
	    write(imp,*)'ligne csv de plus de 50000 car.'
		call pose(112)
	  endif

      if (uselabels)then
	  read(11,'(a50000)',end=700) tlab1000
c      write(*,*)'lab:',trim(tlab1000)
c protection
      if (len_trim(tlab1000).ge.50000) then
	    write(*,*)'ligne labels.csv de plus de 50000 car.'
	    write(imp,*)'ligne labels.csv de plus de 50000 car.'
		call pose(113)
	  endif
c      write(*,*)'libelles:',trim(tlab1000)
      endif	  
 	  
	  else
c      donc pas csv	  
c      donc pas csv	  
c²      write(*,*)'lire pas csv'
      read(10,'(50000a1)',end=700) (tlu(jlu),jlu=1,minlrl)
      
      end if
c          csv else

      if (nstop.eq.0)then
	      write(*,*)'demande stopaftr=0, abandon du job'
	      write(imp,*)'demande stopaftr=0, abandon du job'
	     call pose(745)
	  endif 
	  nlu=nlu+1
      nbi=nbi+1
	  
c	  write(*,*)'(6)labelq:',labelq
      do 7455 iq=1,nbq
	  faitq(iq)=.false.
7455  irepq(iq)=0 


c GAFFE il ne faudra calculer irepq que pout le premier item de chaque question
    

c en cours: faie une boucle lecture irepq avabt boucke sur le items

c faire une seule boule lecture des valeurs
c ----------
c debut de lireval
	  if (csv) then
c      
      lvisible=len_trim(tlu1000)
	  if (tlu1000(lvisible:lvisible).eq.';' ) then
c      write(*,*) '; en fin de ligne'	  
	  endif
c      write(*,*)'csv ',len_trim(tlu1000),' ===',trim(tlu1000),'==='
 	  icol=0
c ilast vrai si demande denoère colonne
	  ilast=0
	  idebrep=0
7751  ipremc=idebrep+1 
c       write(*,*)'_'
c       write(*,*) 'on avance dans ligne ilire ligne nval premc:',ipremc
775   continue
      if (idebrep.ge.len_trim(tlu1000)) then
c         write(*,*) 'en fin de ligne dernier icol ',icol,ipremc
		 ilast=1
	    else
       idebrep=idebrep+1
c      write(*,*) idebrep,'::',tlu1000(idebrep:idebrep)
       endif
c²      if (tlu1000(idebrep:idebrep).ne.';'
c²     1  .and.  idebrep.lt
      if (tlu1000(idebrep:idebrep).ne.';' .and.ilast.eq.0) then
	  goto 775
	  else
c on tient le ;	 
c      write(*,*)'ilast =',ilast
c     write(*,*)'; a idebrep=',idebrep,' / ',len_trim(tlu1000)
c ici etait lt
c GAFFE ne prend pas en charge ; apre derniere colonne
      if (idebrep.lt.len_trim(tlu1000))then
c	  write(*,*)' val ; trouve a ',idebrep
	  else
	  idebrep=idebrep+1
c	  write(*,*)'fin de ligne val idebrep mis a ',idebrep
	  
	  endif
	  icol=icol+1 
c      write(*,*)'on traite la colonne icol=',icol 
      if (.not. usedcol(icol).and.icol.ne.nfiltre)then
c	  write(*,*)'inutile icol=',icol 
	  else
c	  write(*,*)icol,' val num de',ipremc,idebrep-1,'"',
c     1 tlu1000(ipremc:idebrep),'"'
		
c	  if (icol.eq.nfiltre) write(*,*)'filterr'
c      write(*,*)'est utile la colonne icol=',icol 
	   imult=0 
	  
	  nbnonvide=0
	  do 7752 ichif=ipremc,idebrep-1
	  
      iun=ichar(tlu1000(ichif:ichif))-48
c blanc en zero
      if (iun.ge.0 .and. iun.le.9) then 
	  imult=imult*10+iun
      nbnonvide=nbnonvide+1
c	  write(*,*) 'nbnonvide ',nbnonvide,imult
      endif		 
c	  write(*,*) ichif,'::::',iun
7752  continue
      colvide(icol)=nbnonvide.eq.0
c      write(*,*)'=recap de ',ipremc,' à ',idebrep-1, colvide(icol)
	  
c filtrer
c	    write(*,*)'test ',icol,nfiltre,imult,ifiltre
      if (nfiltre.gt.0 .and.icol.eq.nfiltre) then
c	    write(*,*)'test vat filtre ',imult,ifiltre
      if (imult.ne.ifiltre)then
c	   write(*,*)'ignorer now 710'
       nbi=nbi-1
	   nbign=nbign+1
	   goto 710
	   end if
	   
c 
       endif

c faudra 20190208 si 99

	   ivalcol(icol)=imult
c      write(*,*)'memorise ',icol,'_',tlu1000(ipremc:idebrep-1),'_',
c     1 ivalcol(icol) 	  
c     avancer au dela de ;
       endif
c était lt
      if (idebrep.le.len_trim(tlu1000) .and.ilast.eq.0) then
c	      write(*,*) 'retour en 7751 ?????'
	      goto 7751
		  end if
	  idebrep=idebrep+1
c	  write(*,*)'idebrep now ',idebrep
      end if
 
c      write(*,*)'fini la ligne icol=',icol
	  if (listq.gt.0) then
       do 63611 icolx=1,icolbrut
c        if (usedcol(icolx)) write(*,*) 'used:',icolx,ivalcol(icolx),
c     1 colbrut(icolx)
63611    continue
	  endif
c ------------- affecter valeurs à irepq
c	  write(*,*)'(8)labelq:',labelq
	  
c rep
      do 771 iiq=1,nbq
c 	  write(*,*)'peek iiq:',iiq,' igd;',igd,' iit=',iit
c a priori la reponse de l'individu est non
      irepit(iit)= .false.
	  if (csv) then
c 
      imult= ivalcol( lircol(iiq))
c 00190204 accepter 10	et 11   
c GAFFE place trop haut à faisre apres erceode vid et 99
c      write(*,*)'imult=',imult,iiq
      if (imult.eq.99 .and. neufneuf(iiq).ge.0)then
    	  imult=neufneuf(iiq)
c		  write(*,*) 'imult iiq a 99',imult
		  end if
      if (imult.le.m1-1)then
      x1(iiq,imult+1)=	  x1(iiq,imult+1)+1
c	  write(*,*)'x1(',iiq,imult+1,x1(iiq,imult+1)
      endif      
	   if (mx(iiq).ne.'        ') then
	  ivbis=ivalcol(lircolx(iiq))+1
      if (ivbis.le.m1)then
      x2(iiq,ivbis)=	 x2(iiq,ivbis)+1
c	  write(*,*)'x2(',iiq,',',ivbis,')=',x2(iiq,ivbis)
      endif      

	   if (x(iiq)) then
	   jmult=imult+10*ivalcol(lircolx(iiq))
	   if (listq.gt.0 )then
	   endif
       imult=jmult
	   else
c verifier exclusif
       if (imult*ivalcol(lircolx(iiq)).ne.0) then
	     write(*,*)'CSVSPLIT entre deux colonnes non excludsives'
		 write(*,*) lircol(iiq),mqu(iiq),imult
		 write(*,*) lircolx(iiq),mx(iiq),ivalcol(lircolx(iiq))
		 write(*,*)'en lisant individu:',nbi
	     write(imp,*)'CSVSPLIT entre deux colonnes non excludsives'
		 write(imp,*) lircol(iiq),mqu(iiq),imult
		 write(imp,*) lircolx(iiq),mx(iiq),ivalcol(lircolx(iiq))
		 write(imp,*)'en lisant individu:',nbi
		 call pose(183)
       endif	   
	   imult=imult+ivalcol(lircolx(iiq))
	   endif
	   endif
      endif

c 99
	  irepq(iiq)=imult
	  if (imult.gt.mak(iiq))mak(iiq)=imult
c&	  	  write(*,*)'(9)labelq:',labelq

c comptage tri a plat
c             fin de csv	   


771   continue
c      write(*,*)'771 fin de lire val etr codes'

c -------

	  
c     fin du if csv 	  
	  else
      iun=ichar(tlu(lircol(iqorit(iit))))-48
      if (iun.eq.-16)then
c a voir lu espace en i1!
	      iun=nr(iiq)
c	      if   (listq.gt.0) write(*,*) 'nr recode Q:',iiq
	  endif	  
      if (iun.ge.0 .and. iun .le.9) then
	   irepq(iiq)=iun
	   
	    goto 7722
	  endif
      write(*,*) 'item rang ',iit, '  reponse :',irepq
      write(imp,*) 'item rang ',iit, '  reponse :',irepq
      write(*,*) 'individu:',nbi,' rang item:',iit ,' rang question:',
     1 iiq,' colonne orig:',lircol(iiq)
      write(imp,*) 'individu:',nbi,' rang item:',iit ,' rang question:',
     1 iiq,' colonne orig:',lircol(iiq)
        call pose(30)
       endif
c             fin de csv	   
7722   continue


c	  write(*,*)'(a)labelq:',labelq

c fin du lire val	  

c ----------
c	    write(*,*)'boucle V TERMINEE'
    
c--------------------------
       lll=len_trim(tlab1000)
c       write(*,*)'un lab:',lll,' ',tlab(1:LLL)

	  if (csv) then
c 
 	  icolrep=0
	  idebrep=0
c      write(*,*) 'tab:',len_trim(tlab1000)
c on balaye tlu pour capturer la colonne nc selon iiq igd
c on reviendra en 7751  après 
97751  ipremc=idebrep+1 
c       write(*,*)'ipremc mis a ',ipremc
9775   idebrep=idebrep+1
c      write(*,*) idebrep,'::',tlab1000(idebrep:idebrep)
      if (tlab1000(idebrep:idebrep).ne.';' .and.
     1  idebrep.lt.len_trim(tlab1000)) then
	  goto 9775
	  else
	  if(idebrep.eq.len_trim(tlab1000))then
c      write(*,*)nlu,' fin  lib ' , idebrep,tlab1000(ipremc:idebrep-1)
		else
c      write(*,*)'un  ; labels a ' , idebrep
	  endif
	  
	  if(idebrep.eq.len_trim(tlab1000)) idebrep=idebrep+1
c on tient le ;	  c
	  icolrep=icolrep+1
c      write(*,*)'icolrep now',icolrep
      if (.not.usedcol(icolrep)) then
c       write(*,*) icolrep,' pas :',usedcol(icolrep),
c     1 tlab1000(ipremc:idebrep-1)
	  else

c      write(*,*)' colonne csv used ',icolrep,ipremc
c      write(*,*)'colused',icolrep,ipremc,idebrep-1,  
c     1 tlab1000(ipremc:idebrep-1)
c on est à la fin de la bonne colonne
c irepq doit etre la v   alele
c         write(*,*)' icolrep=',icolrep,' ',ipremc,idebrep-1,'=',
c     1  tlab1000(ipremc:idebrep-1),'='
c         write(*,*)'vect',(irepq(iiq),iiq=1,nbq)
         do 47 iiq=1,nbq
c		 write(*,*)'(z)labelq=',labelq,' iiq=',iiq
c		 write(*,*)icolrep,'???? ',lircol(iiq),lircolx(iiq)
c		 write(*,*)'usin q=',iiq,ipremc,idebrep-1
c		    write(imp,*) icolrep,' used by ',iiq,mqu(iiq),lircol(iiq),
c     1 ' VAL=',tlab1000(ipremc:idebrep-1),'='
c on traite d'ab mqu
         if (icolrep.ne.lircol(iiq))then
c          write(*, *)'non1'
		  else
c		  write(*,*)'icolrerp ',icolrep,colvide(icolrep)
		  if (colvide(icolrep))then
		  irepq(iiq)=nr(iiq)
		  ip1=nr(iiq)+1
c		  write(*,*)'mis valeur nr', ip1 ,iiq,nr(iiq)
		  else
	 	 ip1=ivalcol(icolrep)+1
		 endif
c		 write(*,*)icolrep,' util G',iiq,lircol(iiq),lircolx(iiq),ip1
c     	 write(*,*)'a gauche rep=',ip1
c 		 write(*,*)'iun ou autre',icolrep,' q:',iiq,' val+1=',ip1
               if (ip1.eq.100) then
				  ip1=neufneuf(iiq)+1
				  ivalcol(icolrep)=ip1-1
				  irepq(iiq)=ip1-1
c			      write(*,*) '99 nbi=',nbi,' q:',iiq,' : ',ip1-1
		        endif 
		       if (ip1.gt.M1) then
		          write(*,*) 'trop nbi=',nbi,' q:',iiq,' : ',ip1-1
		       else
			      if (.false.) then
c on construit toujours pour vérifier si )= précédent
	              else
c chantier en cours donc toujours
				    labelune=''
             		if (idebrep.gt.ipremc) then
					  labelune=tlab1000(ipremc:idebrep-1)
c	     write(*,*)'labelune ',labelune
c        write(*,*)'raw MQU: iiq:',iiq,' ',mqu(iiq),
c     1 ipremc,idebrep-1,' val:',ip1-1,'=',trim(labelune),'=',
c     2  trim(labels(iiq,ip1))
c        write(imp,*)'raw: iiq:',iiq,
c     1 ipremc,idebrep-1,' val:',ip1-1,'=',trim(labelune),'=',
c     2  trim(labels(iiq,ip1))
					endif  
		            labelprt=''
		            labelscr=''
                    if (ip1.gt.M1)  then
	                   write(*,*)'ignored GT 11'
	                else
        labelscr=''
		labelprt=''

		if (   len_trim(labelune).eq.0)then
		else
c	    			  		 write(*,*)'(y0)labelq:',labelq
c²					  		 write(*,*)'ajout (raw)labelq:',labelune
c² 		            		 write(*,*)'ajout (scr)labelq:',labelscr
c² 		            		 write(*,*)'ajout (prt)labelq:',labelprt
c 					  		 write(imp,*)'ajout (y1)labelq:',labelscr
c 		            		 write(imp,*)'ajout (y1)labelq:',labelprt
 		 

	                  if (icolrep.eq.lircol(iiq))then
					  
c		  write(*,*) 'q:',iiq,' val:',ip1-1,' icolrep ',icolrep

c		  write(*,*) 'q:',iiq,' val:',ip1-1,' quoi ',labels(iiq,ip1)
c		  write(*,*) 'q:',iiq,' val:',ip1-1,' une  ',trim(labelune)
                       if (labels(iiq,ip1).eq.labelune)then
c				 write(*,*)'deja ignorer'	  
c deja vu identique
					  else
              call accents(labelune,labelscr,labelprt)
					      if (len_trim(labels(iiq,ip1)).ne.0)then
		write(*,*)'decode individu nbi=',nbi
		write(*,*)'label incoherent question: ',iiq,' csv:',mqu(iiq),
     1 ' val:',ip1-1
	    write(*,*)'label enregistré ',trim(labels(iiq,ip1))
        write(*,*) '<>',trim(labelune)
		write(imp,*)'label incohérent question:',iiq,' csv:',mqu(iiq),
     1 ' val:',ip1-1,' ',trim(labels(iiq,ip1)),
     1 '<>',trim(labelune)
				call pose(611)
						  endif
						  
     					  labels(iiq,ip1)=labelune
c		  write(*,*) 'q:',iiq,' val:',ip1-1,' ajout ',trim(labelune)
                            labelsprt(iiq,ip1)=labelprt
                            labelsscr(iiq,ip1)=labelscr
					  endif		
c									 write(*,*)'(y2)labelq:',labelq

c		write(*,*)'endif 1'					
		endif
c		write(*,*)'endif 2'					
        endif	
c		 write(*,*)'(p)labelq:',labelq
		
c        write(*,*) ' fin du if = lircol(iiq)'
		endif
c				write(*,*)'endif 3'					

		endif
c    			write(*,*)'endif 3 ajoute'					

		endif
c    			write(*,*)'endif 3 ajoute ajoute'					
        endif
c		write(*,*)'point UN',icolrep,lircolx(iiq)
        if (icolrep.ne.lircolx(iiq))then
c		write(*,*)' non 2'
		else
        					  labelune=tlab1000(ipremc:idebrep-1)
              call accents(labelune,labelscr,labelprt)

c²		write(*,*)' lircolx(iiq) (x)labelq:',mx(iiq),'=',trim(labelune)
c           write(*,*)icolrep,' util D',iiq,lircol(iiq),lircolx(iiq)
c		write(*,*)' pour x iiq=',iiq,' ',mqn(iiq)
	    ip1=ivalcol(icolrep)+1
c    	if (len_trim(labelsx(iiq,ip1)).gt.0 .or. 
c     1    len_trim(labelune).eq.0)then
    	if (len_trim(labelune).eq.0)then
		else
	 
c	    write(*,*)'oXq:(',iiq,',',ip1,')',mx(iiq),'=',
c     1 trim(labelsx(iiq,ip1)),'=',trim(labelune),'='
                           labelsX(iiq,ip1)=labelune
c        write(*,*)icolrep,' ',mx(iiq),' ',
c     1 'newXq:(',iiq,',',ip1,')=',trim(labelsx(iiq,ip1)),'=',
c     1  len_trim(labelsx(iiq,ip1))
                           labelsXprt(iiq,ip1)=labelprt
                           labelsXscr(iiq,ip1)=labelscr
c                    		write(*,*)' xprt(',iiq,ip1,'):',labelscr
		        endif
c						write(*,*)'endif 5'

				endif
c						write(*,*)'endif 6'					

       
c 1-
c		 write(*,*)'(47)labelq:',labelq

47      continue
	  
c²47    write(*,*)' en 47 iiq=',iiq	  
c on en a fini pour la question!
c	   	  write(*,*)'(b)labelq:',labelq,' iiq=',iiq

c     avancer au dela de ;
c      idebrep=idebrep+1
c	  write(*,*) 'idebrep=',idebrep
      endif
      endif
c 
      if (idebrep.lt.len_trim(tlab1000)) goto 97751
c     fin du if csv 	  
	  else
c     endif
	  
	  endif
c             fin de else csv	   

	  
c      write(*,*)'boucle Q terminee'
c   	  write(*,*)'(c)labelq:',labelq

c--------------------------------------------------
      if (LISTQ.gt.0) then
	  if (.not. prttete)then
	  prttete=.true.
	  write(*,*)'Liste des reponses de ',listq,' individus'
	     write(*,'(1h#5x,1h:,(99(a8,1x)))') 
     1 (mqn(iiq),iiq=1,nbq)
	     write(*,'(1h#5x,1h:,(99(a8,1x)))') 
     1 (mqu(iiq),iiq=1,nbq)
	  write(imp,*)'Liste des reponses de ',listq,' individus'
	     write(imp,'(1h#5x,1h:,(99(a8,1x)))') 
     1 (mqu(iiq),iiq=1,nbq)
	  
      if (nbmult.gt.0) then
	  
 	
	  write(*,'(1h#,5x,1h:,(99(a8,1x)))') 
     1 (a8(iiq),iiq=1,nbq)
	     write(*,'(1h#,5x,1h:,(99(a8,1x)))')
     1 (mx(iiq),iiq=1,nbq)
	  write(imp,'(1h#,5x,1h:,(99(a8,1x)))') 
     1 (a8(iiq),iiq=1,nbq)
	     write(imp,'(1h#,5x,1h:,(99(a8,1x)))')
     1 (mx(iiq),iiq=1,nbq)
	    endif
      write(*,*)
    	endif
		
	     write(*,'(1h#,i5,1h:,(99(i4,5x)))') nlu,
     1 (irepq(iiq),iiq=1,nbq)
	     write(imp,'(1h#,i5,1h:,(99(i4,5x)))') nlu,
     1 (irepq(iiq),iiq=1,nbq)
	         listq=listq-1	  
      if (listq.eq.0) write(*,*)'suite de la lecture silencieuse'
955   continue
	  endif	 
	  
c cree irepit = 
c lv oui si item +
      do 721 iit=1,nbit
      ir1=irepq(iqorit(iit))
	  irepit(iit) =lv(iit,ir1+1)	  
721   continue

      if (LISTITEM.gt.0) then
	  listitem=listitem-1
	   write(*,'(1h#,i5,(30L2))') nlu,(irepit(iit),iit=1,nbit)
       write(imp,'(1h#,i5,(30L2))')nlu, (irepit(iit),iit=1,nbit)
	  end if
	  

c 20171116 separer les boucjes

c      write(*,*)'nbit:',nbit
      do 7411 iit=1,nbit
      do 7412 jjt=1,iit
c ne pas compter les items issus de la meme question	  
c      write(*,*) iit,':',iqorit(iit) ,'*',jjt,':',iqorit(jjt),
c     1 ' ',irepit(iit),'*',irepit(jjt)
	 
c diagonale=compter les oui
      if (iit.eq.jjt .and. irepit(iit) ) goto 742
	  if (iqorit(iit) .eq. iqorit(jjt)) then 
c	  write(*,*) 'PPij (',iit,jjt,'):',idinfx,' meme Q'
	     goto 7412
	  end if	 
      if (.not. ( (irepit(iit) .and. irepit(jjt)))) then
c          write(*,*) 'PPij (',iit,jjt,'):',idinfx,' pas ++'
	  	  goto 7412 
  	  endif
742   continue
      idinfx=idinf(iit,jjt)
c                        ++
      PP(IDINFx)=PP(IDINFx)+1
c	  write(*,*) 'PPij (',iit,jjt,'):',idinfx,'=',pp(idinfx)
7412   continue
c7412   continue
7411   continue

7219   continue
c ------------------- en travaux	  
c	   	  write(*,*)'(d)labelq:',labelq

c       write(*,*)'goto 710'
       goto 710
700   continue
      write(*,*) 'fin de fichier individus nb=',nbi,' lonmax:',mxlifi,
     1 ' ignores:',nbign
    	 write(imp,*) 'fin de fichier individus: ',nbi,' lonmax:',mxlifi,
     1 ' ignorés:',nbign
	  call stamp(0,0)
c	  write(*,*)'(7)labelq=',labelq
      if (nbi .gt. 0) goto 780
      write (*,*)' fichier individu vide !'
      write (imp,*)' fichier individu vide !'
       call pose(31)

	  
c on vient en 780 si fin de fichier non vidfe	  
780   continue

c bug nbi=0
      if (nbi.le.0) then
	     write(*,*)'aucun indivicue fin du job'
		 write(imp,*)'aucun indivicue fin du job'
		 call pose(780)
		 endif
		 
      close(10)
c      write(*,*)'pp:',(pp(jpp),jpp=1,10)

c   transformer PP en W?
c       write(*,'(a8,10i3)') 'numeit:',(numeit(iii),iii=1,10)
      do 7909 iit=2,nbit
      do 7909 jjt=1,iit -1
c   pour les cases sous la diagonale	  
c      write(*,*) iit,jjt, '++:',pp(idinf(iit,jjt)),pp(iit),pp(jjt)
c     si la case NII >  NJJ
c ifdonde pour tester bug, sans intéret
      idone=0
      if (pp(idinf(iit,iit)) .gt. pp(idinf(jjt,jjt))) then
        pp(idinf(iit,jjt))=pp(idinf(jjt,jjt))-pp(idinf(iit,jjt))
		idone=1
		endif
      if (pp(idinf(iit,iit)) .le. pp(idinf(jjt,jjt))) then
	   if (idone.eq.1 ) write(*,*)'bug'
       pp(idinf(iit,jjt))=pp(idinf(iit,iit))-pp(idinf(iit,jjt))
	   endif
c      write(*,*) numeit(iit),numeit(jjt), ' vide:', pp(idinf(iit,jjt))
7909   continue



c      write(*,*)'net:',(pp(jpp),jpp=1,10)

660   continue
1100  continue
C
C
C  ----------------          DEBUT DE LA BOUCLE DE LECTURE DES TQBLEAUX
998   continue
c        write(*,*) 'date=====',idd,'====='
c page sub 20181214
c      CALL STPA(3,IPA,imp)
c
C                              ******
C                               ****
C                                **
C     ---------------------- CHOIX DE LA METHODE AD OU SOUS
C                                **
C                                **
C                               ****
C                              ******
      fsaved=.true.
c pour éviter dialogue a/S pour choix method, passera a false aussitot
     	   meth1='A'
c	  	  write(*,*) 'fsaved init false'
c      write(*,*) 'au dessus de 213'
      ilyaeu=.false.
213   continue
c      write(*,*) 'en meth  de 213'
214   continue
       if (.not. fsaved .and.ilyaeu)	    then
	     write(*,*) 'GAFFE: travail en cours non sauve le perdre O/N?'
	  write(imp,*) 'GAFFE: travail en cours non sauve le perdre O/N?'
		       read(*,'(a1)') acmd
       write(imp,*)acmd
      call upcas1(acmd)
      if (acmd.NE. 'O') THEN
	     write(*,*)'réinitialisation A/S abandonnée'
	     goto 203
     	endif	 
	  endif
      write(*,*) 'initialisation: ',meth1 , 
     1 ' (A: Additif ou S: Soustractif)'
      write(imp,*) 'initialisation: ',meth1 , 
     1 ' (A: Additif ou S: Soustractif)'
       ilyaeu=.false.
      fsaved=.false.
c	  	  write(*,*) 'fsaved init false'
      NITP=0
      IC=0.D0
      IW=0.D0
      h=0
      NBORD=0
      DO 202 I=1,NBIT
      if (nbi .eq. 0) write (*,*)' nbj nul, cela va mal se passer'
      if (nbi .eq. 0) write (imp,*)' nbj nul, cela va mal se passer'
c	  write(*,*) 'calcul item ',i
c GAFFE IPOP et N meme valeur calculee
c mais ipop passera en % plus bas
      N(I)=PP(IDINF(I,I))
      P(I)=(1.*N(i))/NBI
      IPOP(I)=PP(IDINF(I,I) )
      IWI(I)=0.D0
      ICI(I)=0.D0
202   IAIT(I)=METH1 .EQ. 'S'

c rtaz qq
      do 23001 iq=1,nbq
	  qq(iq)=0
23001 continue	  

      
      IF(METH1 .EQ. 'A') then
c continuer par une liste
         meth1='L'
         goto 230		 
		 endif

c donc  ici 'S'
c               comptage initial du S
      NITP=NBIT
c      write(*,*) 'calcul en 222'
      
	  IC=0
	  IW=0

      do2338 i=1,10
c	  write(*,*)'pp(',i,') :',pp(i)
2338  continue
	  do 2339 i=1,nbit
	  IWI(i)=0
	  ICI(I)=0
2339   continue
      DO 223 I=1,NBIT
c	  write(*,*)'ligne i=',i
      qq(iqorit(i))=qq(iqorit(i))+1
c      ICII=0.D0
c      IWII=0.D0
      DO 2222 J=1,NBIT
cc      DO 2222 J=1,i-1
c	  write(*,*)'ligne i ',i,iqorit(i),' col j=',j,	iqorit(j)
      IF (iqorit(I) .EQ. iqorit(J)) GO TO 2222
c  (on ne teind pas compe de deux itempsissus de la meme colonne)
c proba case vide	  
      IWIj=MIN0(IPOP(I),IPOP(J))*(NBI-MAX0(IPOP(I),IPOP(J)))


	   ICIj=PP(IDINF(i,j))
      i4cij=icij
	  i4wij=iwij/nbi
      IWI(I)=IWI(I)+IWIJ
      IW=IW+IWIJ
c	  write(*,*)'avant ',icij,ic
      IC=IC+ICIJ
c	  write(*,*)'apres ',icij,ic
      ICI(I)=ICI(I)+ICIJ
	    i4ic=ic
		i4iw=iw
c	  write(*,*) 's ic int:',i,' j:' ,j,' ic=',i4ic,' iw=',i4iw/NBI
c      write(*,*) i,i,' S ic=',i4ic,'   iw=',i4iw/nbi
c      write(*,*) 'S ic%=',ic/(nbi*nbit),'   iw%=',iw/(nbi*nbi*nbit)
c      write(imp,*) 'S ic=',i4ic/(nbi*nbit),'   iw=',i4iw/(nbi*nbi*nbit)

2222  continue
c       write(*,*) 'fin li:',i,'  ici(i):',ici(i),'  iwi(i):',iwi(i)
223   continue
      i4ic=ic
	  i4iw=iw
c      write(*,*) 'S  ic=',i4ic,'   iw=',i4iw,i4iw/nbi
c      write(imp,*) 'S ic=',i4ic,'   iw=',i4iw
c
c                     continuer en faisant une liste
       meth1='L'
      GO TO 230
C        POUR LISTER L ECHELLE ET CALCULER LE  H
C                              ******
C                               ****
C     -------------------------- DONNEZ VOTRE ORDRE
c                                  si on a de l'avance traiter sans lire
203   continue
20399 continue
 
21255  continue
c  pourlire80a1
c       write(*,*) 'lire sur lca:' , lca,jprem
       if (jprem.lt.ilastnb)then
c       write(*,*)jprem,ilastnb,'reste:',pourlire80a1(jprem+1:ilastnb)
	   endif
c faut voir si il reste des commandes de la ligne précédente
        if (jprem.lt.ilastnb) then
        jprem=jprem+1
        else		
c		write(*,*)'reserve epuisee lire pour de vrai'
       write(*,*)invite 
       write(imp,*) invite
       if (lca.eq.0)    then
     	   READ(*,'(a)',END=20209) pourlire80a1
           write(imp,*) pourlire80a1
c			write(*,*) pourlire80a1
		 else  
            READ(lca,'(a80)',END=20209) pourlire80a1
c²			write(*,*) '(a)',pourlire80a1
        if (.false.)then
            write(*,*)'lu commande <' ,trim(pourlire80a1),'>'
            write(imp,*)'lu commande <' ,trim(pourlire80a1),'>'
			endif
        endif		
c		write(*,*)'lu pourlire:',pourlire80a1
        READ(pourlire80a1,'(80a1)') (tlu(jlu),jlu=1,80)
c eliminer commentaires
c     avance à non espace
c      write(*,*)'lu:',pourlire80a1
      jprem=0
 
c determine 	la portion de ligne utilisée
       ilastnb=-1 
	   do 21277 il=1,80
      if ((jprem.eq.0) .and. (tlu(il).ne.' ')) jprem=il
	   if(tlu(il).ne.' ') ilastnb=il
21277  continue
c      write(*,*) 'ilastnb=',ilastnb,' jprem=',jprem
	   if (ilastnb.eq.-1) then
	     ilastnb=0
		 jprem=0
		 else
c	  write(*,*)'lu utile:',jprem,ilastnb,(tlu(il),il=jprem,ilastnb)
c	  write(imp,*)'lu utile:',jprem,ilastnb,(tlu(il),il=jprem,ilastnb)
	  endif
c alonger le ligne de pa repetition le fin de ligne
c sauf bien sur si commande r/R
c	   write(*,*) 'prem ',jprem,' last:',ilastnb,'+',lrepet
	   if (lrepet.gt.0 )then
	   if (tlu(jprem).ne.'r' .and. tlu(jprem).ne.'R' )then
	   do 22266 jj=1,lrepet
	   tlu(ilastnb+jj)=repetfin(jj:jj)
22266  continue
       ilastnb=ilastnb+lrepet
       write(*,'(80a1)') '---complete ',(tlu(jj),jj=1,ilastnb)
        endif 		
       endif

	   endif

c		if (lca.ne.mae) write(*,'(1x,80a1)') (tlu(jlu),jlu=1,80)
c	   write(*,'(1x,2h<<,80a1,2h>>)') (tlu(jlu),jlu=1,80)
c      write(*,*)'jprem=',jprem,ilastnb
	  if (jprem.eq.0) then
	    meth1=' '
c		write(*,*) '0 force espace'
		else
      meth1=tlu(jprem)
c	  write(*,*)'utiliser ',jprem,meth1
c	  write(imp,*)'utiliser ',jprem,meth1
	  endif
c      write(*,*) 'commande===',meth1,'==='
      call upcas1(meth1)
c      write(*,*) 'commande===',ichar(meth1),'==='
      if (meth1.eq.'*') then
	  write(*,*)'commentaire: '
	  write(*,*)(tlu(jjp),jjp=jprem,80)
	  write(imp,*)'commentaire: '
	  write(imp,*)(tlu(jjp),jjp=jprem,80)
	  jprem=1000
	  goto 203
	  endif

	  if (meth1.eq.',')meth1='?'	 
c 20180125 aidepar ? h fonne h!
c      if (meth1 .ne. '?'  .and. meth1.ne.'H')goto 21388
      if (meth1 .ne. '?')goto 21388
c faut RAZ du buffer clavier pour affichage immédiat
      ilastnb=0	  
	  
c      write(*,*) ' Help, d‚veloppement en cours'

      write(*,*) ' [ReInitialisation () choix de la m‚thode)'
      write(*,*) '  A  partir/repartir en additif (aucun item pr‚sent)'
      write(*,*) '  S  partir/repartir en soustractif (tous les items',
     1 ' pr‚sents)'
      write(*,*) ' H: affiche la valeur du coefficent de Loevinger.'
	  write(*,*) ' -- Composition de l''‚chelle'
	  write(*,*) '  +xx: ajouter l''item xx'
      write(*,*) '  -xx: retirer l''item xx'
	  write(*,*) '  Exx: remplacer le seul item de la question par xx'
    
      write(*,*) ' -- Choix Affichage des items (pour L, T et G):'
      write(*,*) '  F  tous les items (+ ou -)'
      write(*,*) '  P  items presents'
      write(*,*) '  N  items absents'
      write(*,*) '  X  items de questions absentes'
      write(*,*) '  Y  items de questions presentes'
      write(*,*) ' L  liste des items (Hi, Ci, Wi...) selon F,P,N,X,Y'

	  if (usequestions .or. uselabels) then
      write(*,*) ' -- M.  Libelles questions/reponses '
	  write(*,*) '  M0  aucun libelle.p '
	  endif
	  if (usequestions) then
	  write(*,*) '  Mq  libelles des questions '

	  endif
	
	if (uselabels) then
	  write(*,*) '  -- Libelles des reponses'
      write(*,*) ' 	Mr  sous les questions '
	  write(*,*) '  MV  pour les questions ayant un item'
	  write(*,*) '  MW  pour TOUTES les questions'
	  write(*,*) '  Mi  sous les items '
	
	  endif

      write(*,*) '  M1/2 : interligne '
c      write(*,*) '  M4/5 : largeur des cases (pour T)'
      write(*,*) '  ME/L : sortie Etroite ou Large '
	  write(*,*) '  MZ   "purge" le tampon du fichier de log: "'
     1 ,trim(dsnlog),'"'
      write(*,*) ' -- Tx  demande de Tableau '
      write(*,*) '  Tn  Nij = effectif de la "case vide"'
      write(*,*) '  Tc  Cij = % observée de la case vide.'
      write(*,*) '  Tw  Wij = % théorique de la case vide.'
      write(*,*) '  Th  Hij = % H de loevinger (100-Cij/Wij).'
      write(*,*) '  T+  efectif de la case ++'
      write(*,*) '  Ti  Implication 0.01'
      write(*,*) '  Tj  Implication 0.05'
      write(*,*) '  Tk  Implication 0. 10'
      write(*,*) '  Ts  Seuil 0.01'
      write(*,*) '  Tt  Seuil 0.05'
	  write(*,*) '  Tu  Seuil 0.10'

	  

c      write(*,*) '    x: N C W H I J K S T U .'
      write(*,*) ' G  Graphique des popularit‚s par question & items'
      write(*,*) ' Razert ajouter "azert" a la fin de chaque commande'

      write(*,*) '-- K... Conserver le travail en cours'
      write(*,*) '  Kabc  avec le nom chemin.abc'
      write(*,*) '  K     avec son nom original (chemin)'
      write(*,*) ' B     fait un bug pour p‚dagogie'
      write(*,*) ' Q Quitte, si derniere modif non sauv‚es, confirmer'
     1 ,' par E (End)'
      write(*,*) ' * commentaire jusqu''a fin de ligne'
	  goto 203

21388 continue
c     write(*,*) 'en attente commande'
      if (meth1 .ne. 'C') goto 10130
c	   write(*,*) 'trouvé commande C lca=',lca
	  if (lca.ne.0) then
c ???? err=
	  close (lca)
c		 write(*,*) 'close ',lca
	  endif
      lca=mae
      nbord=0
      write(*,*) 'trouve commande ''C'', maintenant entree au clavier'
      write(imp,*) 'trouve commande ''C'', maintenant entree au clavier'

      goto 203
10130 continue
      if (meth1.eq.'R') then
c	   write(*,*) 'memorise a paetir de ',jprem+1
	    do 10139 jj=jprem+1,ilastnb
		
10139   repetfin(jj-jprem:jj-jprem)= tlu(jj)
		lrepet=ilastnb-jprem
		jprem=ilastnb+1
		write(*,*)lrepet,' auto en fin ligne:',repetfin(1:lrepet)
		goto 203
	  end if 

      if (METH1 .eq. 'Q') then
      if (fsaved) call pose(0)
      write(*,*) 'quit demande echelle non sauvee'
      write(imp,*) 'quit demandé échelle non sauvée'
10133 write(*,*) 'frappez A(bandon et continue ) ',
     1 ' ou E(nd sans sauvegarder)'
	  write(imp,*) 'frappez A(bandon et continue ) ',
     1 ' ou E(nd sans sauvegarder)'
	  
	  if (lca.eq.0) read(*,'(a1)',end=1014) acmd
      if (lca.ne.0) read(lca,'(a1)',end=1014) acmd
      write(imp,*) acmd
	  if (lca.ne.0) write(*,*) acmd
      call upcas1(acmd)
      if (acmd.eq. 'E') then 
	     write(*,*)'pas de reprise possible du dernier etat' 
	     write(imp,*)'pas de reprise possible du dernier état' 
		 goto 10132
		 endif
      if (acmd.eq. 'A') goto 203
	  write(*,*) 'ni a ni e mais ',acmd
      goto 10133
1014  write(*,*) 'Fin de fichier en demande de confirmation, QUIT'	
      write(imp,*) 'Fin de fichier en demande de confirmation, QUIT'	
      goto 10132  
10132 continue
       call pose(0)

	   endif
	   
      if (METH1 .eq. 'B') then
      write(*,*) 'demande de "bug" (simule faute programme)'
      write(imp,*) 'demande de "bug" (simule faute programme)'
10136 write(*,*) 'frappez A(bandon et continue ) ',
     1 ' ou E(nd sans sauvegarder)'
	  write(imp,*) 'frappez A(bandon et continue ) ',
     1 ' ou E(nd pour terminer en erreur)'
	  
	  if (lca.eq.0) read(*,'(a1)',end=10141) acmd
      if (lca.ne.0) read(lca,'(a1)',end=10141) acmd
      write(imp,*) acmd
	  if (lca.ne.0) write(*,*) acmd
      call upcas1(acmd)
      if (acmd.eq. 'E') then 
	     goto 10138
		 endif
      if (acmd.eq. 'A') goto 203
	  write(*,*) 'ni a ni e mais ',acmd
      goto 10136
10141  write(*,*) 'Fin de fichier en demande de bug, abandon'	
      write(imp,*) 'Fin de fichier en demande de bug'	
	  
10138   continue
      write(*,*)'le programe va faire "Fortran runtime error"'
      open(10,file='*')

	   endif
	   
c dispatch selon la commande tgrouvée
	   IF(METH1 .EQ. 'K')GO TO 101
c dito
      IF(meth1 .EQ. 'L' .or. meth1.eq.' '.or. meth1.eq.'H') GO TO 230
	  if (meth1.eq.'M' )then
      jprem=jprem+1
      meth1=tlu(jprem)
      call upcas1(meth1)
	  
        if (meth1 .eq. 'V') then 
	      listqused=.true. 
     
          goto 203
		  endif
        if (meth1 .eq. 'W') then 
	      listqused=.false. 
          goto 203
		  endif

c                       fermer/ouvrir txt/txt
      if (meth1.eq.'Z')then
	  write(*,*) 'fichier ',dsnlog(1:len_trim(dsnlog)),' fermé'
	  write(imp,*) 'fichier ',dsnlog(1:len_trim(dsnlog)),' fermé'
      call stamp(imp,0)
	  close(imp)
      open (imp,file=dsnlog,access='append',err=10300)
	  call stamp(imp,0)
      write(*,*)  'fichier ',dsnlog(1:len_trim(dsnlog)),' append.'
      write(imp,*)  'fichier ',dsnlog(1:len_trim(dsnlog)),' append.'
	  goto 10299
10300 continue
      write(*,*)'erreur'
      write(imp,*)'erreur'
	   call pose(51)
10299 continue
      goto 203
	    endif

	  if (meth1.eq.'1') then
	     inter2=.false.
		 goto 203
	  endif	 
	  if (meth1.eq.'0') then
	     labeli=.false.
		 labelr=.false.
		 labelq=.false.
		 goto 203
	  endif	 
    
	  if (meth1.eq.'R') then
      if (.not.uselabels)then
		 write(*,*)'pas LABELS sur la carte ECHELLE:, impossible'
		 write(imp,*)'pas LABELS sur la carte ECHELLE:, impossible'
		 goto 203
		 endif
		     labelr=.true.
		 
		 goto 203
	  endif	 
	  if (meth1.eq.'Q') then
      if (.not.usequestions)then
		 write(*,*)'pas QUESTION sur la carte ECHELLE:, impossible'
		 write(imp,*)'pas LABELS sur la carte ECHELLE:, impossible'
		 goto 203
		 endif
		     labelq=.true.
		
		 goto 203
	  endif	 
	  if (meth1.eq.'I') then
	     if (.not.uselabels)then
		 write(*,*)'pas LABELS sur la carte ECHELLE:, impossible'
		 write(imp,*)'pas LABELS sur la carte ECHELLE:, impossible'
		 goto 203
		 endif
	     labeli=.true.
		 
		 goto 203
	  endif	 

    	if (meth1.eq.'4') then
	     col5=.false.
		 goto 203
      endif		 
      if (meth1.eq.'2') then
	     inter2=.true.
		 goto 203
      endif		 
      if (meth1.eq.'5') then
      col5=.true.
		 goto 203
      endif		 
      if (meth1.eq.'L') then
	     large=.true.
		 goto 203
      endif		 
      if (meth1.eq.'E') then
	     large=.false.
		 goto 203
      endif		 
      write(*,*)
     1 'La commande M doit ˆtre suivie d''une lettre: ',
     1 ' 0 1 2 3 4 L E Q I !'
      write(imp,*)
     1 'La commande T doit être suivie d''une lettre: ',
     1 ' N C W H I j k S t u !'
	  goto 203
	  endif
      IF(meth1 .NE. 'T') GO TO 555
c	  write(*,*)' T '
      nuit=0
c i j k s t u	  
      jprem=jprem+1
      meth1=tlu(jprem)
      call upcas1(meth1)
	  if (meth1 .eq. 'N') nuit=1
      if (meth1 .eq. 'C') nuit=2
      if (meth1 .eq. 'W') nuit=3
      if (meth1 .eq. 'H') nuit=4
      if (meth1 .eq. '+') nuit=5
      if (meth1 .eq. 'I') nuit=6
      if (meth1 .eq. 'J') nuit=7
      if (meth1 .eq. 'K') nuit=8
      if (meth1 .eq. 'S') nuit=9
      if (meth1 .eq. 'T') nuit=10
      if (meth1 .eq. 'U') nuit=11
c	  write(*,*) 'nuit=',nuit
      if (nuit .ne. 0) goto 21111
      write(*,*)
     1 'La commande T doit ˆtre suivie d''une lettre: ',
     1 ' N C W H I j k S t u !'
      write(imp,*)
     1 'La commande T doit être suivie d''une lettre: ',
     1 ' N C W H I j k S t u !'
      goto 203
21111 continue
     
      CALL IMPTAB(nbq,nitp,NBIT,PP,NUIT,IAIT,IPOP,NBI,numeit,iqorit)
      write(*,*) ' '
      write(imp,*) ' '
      GO TO 203
555   continue
c suite de dispatch (sauf L et T traités ci dessus)
      IF(meth1 .EQ. 'A' .OR. meth1 .EQ. 'S' ) GOTO 214
C                                **
C                               ****
C                              ******
      IF(METH1.NE.'-'.AND.METH1.NE.'+' .and.meth1.ne.'E')GO TO 205
	  ilyaeu=.true.
C     -----------------------  AJOUTE OU OTER UN ITEM
c noter la position dans le buffer pour y revenir
       isavejprem=jprem
c prendre le numero d'item
c  ici faut decode nombre
      nuit=0
20077 jprem=jprem+1
      if (tlu(jprem).eq.' ' .and. jprem .lt.80) goto 20077
c     construire l nopmbre
20075 if (tlu(jprem) .lt. '0' .or. tlu(jprem) .gt. '9') goto 20074
      nuit=10*nuit+ichar(tlu(jprem))-ichar('0')
c	  write(*,*)'nombre=',nuit
      jprem=jprem+1
      goto 20075
20074 continue
c le separateur n'est pas encore decode
      jprem=jprem-1 
c       write(*,*)'separateur:',tlu(jprem+1),' dern:',tlu(jprem)
c	   jprem=jprem+1
c convertir nuit and "rang de l'item'"
c     tlu utilisé jusqu'à jprem
       if (nuit.gt.99) then
	     write(*,*) 'num‚ro d''item > 99'
		 write(imp,*) 'numéro d''item > 99'
		 goto 203
	    endif
       do 11080 iuit=1,nbit
       if (nuit .eq. numeit(iuit)) then 
	      nuit=iuit
		  goto 11081
		  endif
11080  continue
       write(*,*) 'le numero d''item demand‚ (',nuit,') n''existe pas'
       write(imp,*)'le numero d''item demandé (',nuit,') n''existe pas'
       goto 203
11081 continue
c maintenan t nuit est  le RANG de l'item!   
c la commande E 	  
       IF(METH1.EQ.'+' .AND.IAIT(NUIT)     ) then
       write(*,*)'  ITEM ',nuit,' A AJOUTER  DEJA PRESENT'   
	     goto 203
		 endif
       IF(METH1.EQ.'E' .AND.IAIT(NUIT)     ) then
       write(*,*)'  ITEM ',nuit,' A ECHANGER  PRESENT'   
	     goto 203
		 endif
       IF(METH1.EQ.'-' .AND..not.IAIT(NUIT)     ) then
       write(*,*)'  ITEM ',nuit,' A RETIRER  ABSENT'   
	     goto 203
		 endif

		 if (meth1.eq.'E') then
	   nbmemq=0
c compter les itemps présents de la même question
c prendre la question de l'item est tiré
       iqe=iqorit(nuit)
       ioter=0
	   do 2081 iautre=1,nbit
	    if (iqe.eq.iqorit(iautre) .and. iautre.ne.nuit) then
		  if (iait(iautre)) then
		  nbmemq=nbmemq+1
		  ioter=iautre
c		   write(*,*) nbmemq,' otable:',ioter
		  endif
		endif  
2081  continue	  
		if (nbmemq.gt.1) then
		  write(*,*)'E impossible: deux ou plus items presents'
		  goto 203
		endif
	if (nbmemq.eq.0) then
		  write(*,*)'E impossible: pas d''item present.'
		  goto 203
		endif
c donc ici un seul concurren: ioter
c       reculer dans le buffer de commande
        jprem=isavejprem-1
		write(*,*) 'tlu et&it',tlu(jprem+1)
		tlu(jprem+1)='+'
c  et forcer le -
        nuit=ioter       		
	  endif
c    fin de 'E'	
c ici donc + ou -	
c       write(*,*)'vhgt nuit=',nuit
		IAIT(NUIT)= .not. iait(nuit)
      fsaved=.FALSE.
      IS=-1
      IF (IAIT (NUIT))IS=+1
c	  write(*,*)nuit,' qq:',qq(iqorit(nuit))
      NITP=NITP+IS
	  qq(iqorit(nuit))=qq(iqorit(nuit))+is
c 	  write(*,*) '+/-(',iqorit(nuit),' now ',qq(iqorit(nuit))
c      write(*,*) ' calcul en 211'
      DO 211 I=1,NBIT
c
c voir ce qui etait ciic
      IF (iqorit(I) .EQ. iqorit(nuit)) GO TO 211
      IWIJ=MIN0(IPOP(I),IPOP(NUIT))*(NBI-MAX0(IPOP(I),IPOP(NUIT)))
      IF(I .LE. NUIT) ICIJ=PP(IDINF(NUIT,I))
      IF(I .GT. NUIT) ICIJ=PP(IDINF(I,NUIT))
	  
c	  write(*,*)'iwij=',i,nuit,iwij
      ICI(I)=ICI(I)+IS*ICIJ
C BUG 20181218 +is*
      IWI(I)=IWI(I)+is*IWIJ
c	  write(*,*)'mage fait i=',i,iait(i)
      IF( .NOT. IAIT(I)) GO TO 211
c	  write(*,*) 'c w étaint ',i,j,ic,iw
      IC=IC+2*IS*ICIJ
      IW=IW+2*is*IWIJ
c	  write(*,*) 'c w now   ',i,j,ic,iw
	  i4cij=icij
	  i4wij=iwij
	  i4c=ic
      i4w=iw 	  /(nbi)
c	  write(*,*)'alive'
c	  write(*,*) '+/-is:',is,' icij:',i4cij,' C#=',i4c,' iwij#=',
c     a i4wij/nbi,' W#:',i4w
c	  write(imp,*) 'is:',is,' icij:',i4cij,' C=',i4c,' W:',i4w
211   CONTINUE

      hseul=.true.
      GO TO 230

C    -------------------------- AUTRES COMANDES
205     continue
c commandes de controle des affichage (liste et fraphique)
        if (meth1 .eq. 'X') then 
    	  partil=.false.
		  absents=.false.
		  listqabsentES=.true.
		  listqpresentes=.false.
c		  write(*,*)'list X listqabsentES'
c       write(*,*)'(2)listqabsentes!',listqabsentes

          goto 203
		  endif
        if (meth1 .eq. 'Y') then 
		  partil=.false.
		  absents=.false.
		  listqpresentes=.true.
		  listqabsentES=.false.
c		        write(*,*)'(3)listqabsentes!',listqabsentes

c		  write(*,*)'list Y listqabsentES'
	      goto 203
		  endif
       			
c	     listqabsentES=.false.
c		       write(*,*)'(4)listqabsentes!',listqabsentes

c	     listqpresentes=.false.
		
        if (meth1 .eq. 'P') then 
    	  partil=.true.
		  listqabsentES=.false.
		  listqpresentes=.false.
c		  write(*,*) 'partil true'
		  absents=.false.
	      goto 203
		  endif
        if (meth1 .eq. 'F') then
		    partil=.false.
			absents=.false.
    	  listqabsentES=.false.
		  listqpresentes=.false.
c		  write(*,*) 'partil false'
	      goto 203
			endif
        if (meth1 .eq. 'N') then
    	    absents=.true.
			partil=.false.
		  listqabsentES=.false.
c		        write(*,*)'(5)listqabsentes!',listqabsentes

		  listqpresentes=.false.
          goto 203
			endif
 			
        IF(METH1.EQ.'G')GO TO 240
        IF(METH1 .EQ. 'L')GO TO 230
C     ----------------------- COMMMANDE INEXISTANTE
       if (meth1 .ne. ' ') WRITE(*,242)  meth1
       if (meth1 .ne. ' ') WRITE(imp,242)  meth1
242    FORMAT(' WAT DO IOU OUANT  ',a4     )
      ilastnb=0
      GO TO 203
C      ---------------------  DEMANDE DU GRAPHIQUE  HIJ, P%I.
240   continue
c      write(*,*)'listqabsentes!',listqabsentes
      CALL G(nitp,nbq,P,IAIT,IMP,NBIT,MQn,mqu,Mx,x,numeit,iqorit,qq)
      GO TO 203
C       --------------------- CALCUL DE H
230   continue
c      if (meth1.eq.'H') write(*,*) 'pour l''aide frappez ?'
c      write(*,*) 'en 230 *,* pmeth1 ',meth1,' mae:',mae,' nitp:',nitp
c si il y a moins de deux items, pas de h etc...
      IF(NITP .le. 1)  then
	     h=0
		 c=0
		 w=0
	  	     WRITE(MAE,232) H,C,W,NITP
         WRITE(imp,232) H,C,W,NITP

      else	  
       FDIV=NITP*(NITP-1)/2
       FDIV=FDIV*NBI
      C=IC/FDIV
c	  write(*,*)'c#=',ic,' c:',c,fdiv
c est le % de la somme des CIJ
c autre expression!
c curieux ci dessous 
      iw1=iw/fdiv
c      write(*,*) 'iwx2',iw,'  iw1=',iw1,' fdiv:',fdiv 
	  
      W=IW/(FDIV*NBI)
	  i4w=w*nbi
	  i4c=ic
      H=1-div (C,W)
	  i4fdiv=fdiv
c      WRITE(*,*)'vakcul H',h, 'c#:',i4c,' fdiv:',i4fdiv,iw,' w#:',i4w
c      write(*,*)'c w nitp ',C,W,NITP
c si L on imprimera seulement apres saut de page
c      if (meth1.ne.'L') then
       if (nitp.gt.0)then      
	     WRITE(MAE,232) H,C,W,NITP
         WRITE(imp,232) H,C,W,NITP
		 endif
232   FORMAT(/'H=',F6.3,3X,'(C=',F6.3,'  W=',F6.3,')  ',I5,' ITEMS .')
c+14	   end if
      endif
	  
      IF(METH1 .NE. 'L')GO TO 203
C     ----------------------- LISTE DES ITEMS selon F P N X Y
  526 continue
c      write(*,*) 'P:',partil,' A:',absents,' QA:',listqabsentes
c 	  write(*,*) 'QP:',listqpresentes
c        write(*,*) 'page=====',ipa,'====='
       CALL STPA(4,IPA,imp)
	   call direqui(nbit,nitp)
	   if (nitp.gt.1) then
c	  write(*,*) 'ecriture de hcwp' 
      WRITE(*,232)H,C,W,NITP
      WRITE(IMP,232)H,C,W,NITP
		
	  endif
c	  call direqui(nbit,nitp)
c	  write(*,*) 'coln=',coln
c      WRITE(*,251)
       if (coln) then
      entete ='ITEM     C(I)    W(I)    N(I)   P(I)    H(I)   ET SI'
	  else
	  entete='ITEM     C(I)    W(I)    P(I)    H(I)   ET SI'
	  endif


      WRITE(*,*)entete
      WRITE(IMP,*)entete

      FDIV=NBI*NITP

      DO 250 I=1,NBIT
	  
c fautil afficher cet item ?
c fautxx sera vrai
c      write(*,*)'appel de ilfaut'
      ilf=ilfaut(nbq,i,nbit,iait,iqorit,qq,numeit) 
c      write(*,*)numeit(i),iqorit(i),ilf
     
	  fautxx=ilf.gt.0

      fsdiv=fdiv
      fci=ici(i)
c en simple precision
      C=div(fci,FsDIV)
      fci=iwi(i)
      W=div(fci,(FsDIV*NBI) )
      HP=1-DIV(C,W)
c      HP=1-C/W
      IS=2
      IF(IAIT(I))IS=-2
      cc1= 1.0*IC+IS*ICI(I)
      cc2= (1.0*IW+IS*IWI(I))/NBI
      HPP=1-div( cc1,cc2 )
c      HPP=1-    (IC+IS*ICI(I))/((IW+IS*IWI(I))/NBI)
c si l'item tire de la mame question pas d'affi de nquestion
      IF(i .ne.1 .and. iqorit(i) .eq. iqorit(i-1)  )GO TO 550
	  
C      CHERCHER LA QUESTION DE L ITEM
      j=iqorit(i)
c	  write(*,*)'compte ',mqn(j),'(',mqu(j),a8(j),mx(j),')'
c  compter les items présents otes de cette question
      nbitdecetteqpr=0
      do 257 iipo=1,nbit
	  if (iqorit(iipo).eq.j .and. iait(iipo)) then
	    nbitdecetteqpr=nbitdecetteqpr+1
c        write(*,*)'q#',j,' utilise par ',iipo,' ',numeit(iipo)
			endif
257   continue	  
      MAKJ=MAK(J)
c 	  
c verrue pour code 10!
	  if (csv) makj=10
	  
	  if (nbitdecetteqpr.eq.0 .and. listqused)then
c	    write(*,*) nbitdecetteqpr ,
c     1 ' pas utilisee:',mqn(j),'(',mqu(j),a8(j),mx(j),')'
	  else	
c	  write(*,*)'utilise par ',nbitdecetteqpr,' iterm presents'
	  write(*,*)
	  write(imp,*)
		write(*,*)'      ',mqn(j),'(',mqu(j),a8(j),mx(j),')'
		write(imp,*)'      ',mqn(j),'(',mqu(j),a8(j),mx(j),')'
c                            ici opn pourrait mettre lf
c                                 4 au lieu de 6 :::::
254   FORMAT(20X,a8,2x,1h(,a8,1h) )
      if (mx(j).ne.'        ')then
      if (labelr.and..not.labelq) write(*,*)mqu(j)
      if (labelr.and..not.labelq) write(imp,*)mqu(j)
	  endif
c	  write(*,*)'(1)labelq =',labelq
      if (labelq)then
	      write(*,*)mqu(j),': ',trim(questionsUscr(j))
		  write(imp,*)mqu(j),': ',trim(questionsUprt(j))
	  endif
      if (labelr)then
c	  write(*,*)'labelr'
	  do 444 icod=0,M1-1
c	  write(*,*)'d   :',j,icod+1,len_trim(labels(j,icod+1))
	  if (len_trim(labels(j,icod+1)).gt.0) then
	    write(*,449) icod,x1(j,icod+1),trim(labelsscr(j,icod+1))
	    write(imp,449) icod,x1(j,icod+1),trim(labelsprt(j,icod+1))
449    format(8x,i2,' (',i5,') :',A)
       else		
	    if (x1(j,icod+1).gt.0) then 
	    write(*,449) icod,x1(j,icod+1),trim(labelsscr(j,icod+1))
	    write(imp,449) icod,x1(j,icod+1),trim(labelsprt(j,icod+1))
		endif
       endif
444   continue
c      write(*,*) 'fin des code U'
      endif
c      write(*,*)'fin du if question iterm used'
	  endif
c	  write(*,*)'faut i l X ',it,':',i,' qj:',j,mx(j)
	  if (nbitdecetteqpr.eq.0 .and. listqused)then
c	    write(*,*) nbitdecetteqpr ,
c     1 ' pas utilisee reponses:',mqn(j),'(',mqu(j),a8(j),mx(j),')'
	  else	

      if ( labelr.and.mx(j).ne.'        ')then
      if (labelq)then
		  if (mx(j).ne.'        ') then 
		  write(*,*)mx(j),': ',trim(questionsXscr(j))
		  write(imp,*)mx(j),': ',trim(questionsXprt(j))
		  endif
	  endif
	  if (mx(j).ne.'        ')then 
	  if (labelr.and..not.labelq) then
      write(*,*)mx(j)
      write(imp,*)mx(j)
      endif
c 20190208 10!
      write(*,*)'label  www'
	  do 443 icod=0,M1-1
	  if (len_trim(labelsX(j,icod+1)).gt.0) then
	    write(*,4499) ' ',icod,x2(j,icod+1),
     1 trim(labelsxscr(j,icod+1))
	    write(imp,4499) ' ',icod,x2(j,icod+1),labelsXprt(j,icod+1)
      else
	  if (x2(j,icod+1).gt.0) then
c       write(*,*)'lib vide val  gt 0'	  
	   
	    write(*,4499) ' ',icod,x2(j,icod+1)
	    write(imp,4499) ' ',icod,x2(j,icod+1)
4499    format(a8,i2,' (',i5,') :',A)
		endif
       endif
443   continue
      endif

c deplace trop tot	  endif
      endif
	  
	  
550   continue
c write(*,* )' en 550'

c                         mettre le vecteur des codes presents
c que signifiee x(j)
c       write(*,*) 'mettrte presents ',makj,mak(j),deux,fautxx,x(j)
      
      if ((deux .or. mak(j).gt.9) .and. x(j)) then
      if (fautxx) then
	    write(*,*)' deux of >9 et faut'
      nbv=0
	  do 2507 jjv=0,100
	  lastv=0
	  if (lv(i,jjv+1)) then
	     nbv=nbv+1
		 itv(nbv)=jjv
	  endif
2507  continue	  
	    write(*,*)'(1)coln:' ,coln,nitp,' nbv=',nbv
      if (COLN) then
	  if (nitp.lt.2) then
      WRITE(*,25271) numeit(I) ,N(i),P(I),IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
      WRITE(IMP,25271) numeit(I)    ,N(i),P(I),IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
	  else
c ici nitp>2
      write(*,*) 'else nit^p'
      WRITE(*,2527) numeit(I),C    ,W    ,N(i),P(I),HP,HPP,IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
      WRITE(IMP,2527) numeit(I),C    ,W    ,N(i),P(I),HP,HPP,IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
      endif
	  else
c isi pas COLN
      if (nitp.le.1)then
      WRITE(*,25281) numeit(I)   ,P(I),IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
      WRITE(IMP,25281) numeit(I)  ,P(I),IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
	   
      else	  
      WRITE(*,2528) numeit(I),C    ,W    ,P(I),HP,HPP,IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
      WRITE(IMP,2528) numeit(I),C    ,W    ,P(I),HP,HPP,IAIT(I),
     2 (Itv(jtv),jtv=1,nbv)
	   endif
       endif
	   
2527   FORMAT(1x,I4,2F8.3,i8,  3F8.3,3X,L1,1x,100i3)
25271   FORMAT(1x,I4,16x,i8,  F8.3,16x,3X,L1,1x,100i3)
2528   FORMAT(1x,I4,2F8.3,  3F8.3,3X,L1,1x,100i3)
25281   FORMAT(1x,I4,16x,  F8.3,16x,3X,L1,1x,100i3)
	   
c	         coln/else
	   endif
c    fin de iait
     
	  else 
c        double
      if (fautxx ) then
c	  write(*,*)'construire vecteur présents',makj
      lastivv=0
	  if (makj.gt.M1) write(*,*) BUG 11 IVV
c	  write(*,*)'RAZ ivv makj=',makj
      DO 553 JJ=0,M1-1
      NOMCOP(JJ+1)=IBL8
      IVV(JJ+1)='-'
c	  write(*,*) 'jj=',jj,' lv(',i,',',jj+1,')=',lv(i,jj+1)
      IF (.NOT. LV(I,JJ+1)) GO TO 553
	  if (jj.le.9) IVV(JJ+1)=char(jj+48)
      if (jj.eq.10) iVV(JJ+1)='10'
	   if (jj.eq.11) iVV(JJ+1)='11'
	 
	  lastivv=jj
553   CONTINue

      if (COLN) then
	  if (nitp.lt.2) then
	  WRITE(*,2521) numeit(I)    ,N(i),P(I),IAIT(I),
     2 (IVV(K+1),                      K=0,lastivv)
      WRITE(IMP,2521) numeit(I),N(i),P(I),IAIT(I),
     2 (IVV(K+1),                      K=0,lastivv)
	   else
c  ici plus de deux items, afficher et si 
c      write(*,*) 'lastivv=',lastivv	   
	  WRITE(*,252) numeit(I),C    ,W    ,N(i),P(I),HP,HPP,IAIT(I),
     2 (IVV(K+1),                      K=0,lastivv)
      WRITE(IMP,252) numeit(I),C    ,W    ,N(i),P(I),HP,HPP,IAIT(I),
     2 (IVV(K+1),                      K=0,lastivv)
	   endif
	   else
c      write(*,*)' simple pi'
      WRITE(*,2525) numeit(I),C    ,W    ,P(I),HP,HPP,IAIT(I),
     2 (IVV(K+1),                      K=0,lastivv)
      WRITE(IMP,2525) numeit(I),C    ,W    ,P(I),HP,HPP,IAIT(I),
     2 (IVV(K+1),                      K=0,lastivv)
	  
	  endif
c    fin de coln
c      write(*,*)'use,i::',uselabels , labeli
      if (labeli)then
c	  write(*,*) 'labeli'
	  do 4491 icod=0,M1-1
c si pas libellé écrire vide
c      write(*,*)'icod ',icod,x1(j,icod+1),ivv(icod+1)
c     1, len_trim(labelsprt(j,icod+1))
	  if( len_trim(labelsprt(j,icod+1)).eq.0) then
	  if (x1(j,jcod+1).gt.0) then
	  if (ivv(icod+1).ne.'-' )then
	    write(*,449)icod,x1(j,icod+1) ,' lib vide'
	    write(imp,449)icod,x1(j,icod+1) ,' lib vide'
		end if
		endif
	  else
	  if (ivv(icod+1).ne.'-' )then
	  write(*,449) icod,x1(j,icod+1),trim(labelsscr(j,icod+1))
	    write(imp,449) icod,x1(j,icod+1),trim(labelsprt(j,icod+1))
		endif
       endif
4491   continue
      endif

      endif
c    fin de iait
      endif
c ajouté!po
c   	  write(*,*)'fin du else question'
252   FORMAT(1x,I4,2F8.3,i8,  3F8.3,3X,L1,1x,11A2,1x,a2)
2521   FORMAT(1x,I4,16x,i8,  F8.3,16x3X,L1,1x,11A2,1x,a2)
2525   FORMAT(1x,I4,2F8.3,  3F8.3,3X,L1,1x,11a2,1x,a2)
c ne faire interligne que si item
c      write(*,*) '  '
c      write(imp,*) '  '
c fin de pas double
      endif 
	  
250    continue	  
      WRITE(*,*)entete
      WRITE(IMP,*)entete
c apres liste on imprime h
c      write(*,*) 'apers l'
      WRITE(*,232)H,C,W,NITP
      WRITE(IMP,232)H,C,W,NITP
      GO TO 203
9999   call pose(33)
        
101   CONTINUE
c le dsn est de jprem+1  espace
c      dsn=dsnlu
c	  write(*,*)' k===',dsn,'==='
	  labc=0
10155      if (jprem.lt.ilastnb) then
	  if (tlu(jprem+1).ne.' ') then
     
        if (labc.ge.3) then 
		    write(*,*)'commande K suivie de plus de 3 caract.'
			write(*,*)'commande K et fin de la commande ignoree'
			goto 203
	    endif		
        labc=labc+1
		jprem=jprem+1
c upper case pour comparaison a .TXT plus bas
		if (tlu(jprem).eq.'t' )tlu(jprem)='T'
		if (tlu(jprem).eq.'x' )tlu(jprem)='X'
        mon(labc:labc)=tlu(jprem)
c	    write(*,*)labc,' ext pas a aps abc=',mon	(1:labc)
        goto 10155	
	  endif
      end if	  
      if (labc.gt.0) goto 10244
	  write(*,*)'K sans extension interdit (depuis 20171229)!'
	  write(imp,*)'K sans extension interdit (depuis 20171229)!'
c  verifier dsn existe
      write(*,*) 'vous devez donner un nom … la sauvegarde'
      write(imp,*) 'vous devez donner un nom à la sauvegarde'
      goto 203

10244 continue
c      write(*,*)'kmon vide===',mon(1:3),'===' 
      if(mon(1:3).eq.'TXT' )then
	      write(*,*) 'extension .txt interdite pour sauvegarde'
		  goto 203
	    endif
c      write(*,*) 'k  10244 jfchmn=',jfchmn
c	  write(*,*)' dans a ne plaus écraser était dsn=',dsn
      dsnk=trim(chemin80)//'.'//trim(mon(1:3))
10144  continue
c       write(*,*) 'dsn=',dsn
c	   write(*,*) 'byedsn(1)=',byedsn

       WRITE(*,'('' MEMORISATION demandee dans "'',a,''"'')') trim(dsnk)
       WRITE(imp,'('' MEMORISATION demandée dans "'',a,''"'')') 
     1 trim(dsnk)
C *******************
	  ifin=96
	  do 10109 isupbl=1,12
	  if (dsnk(1:1).eq.' ') dsnk=dsnk(2:12)//' '
10109 continue
      write(*,*) 'verifier si existe: "',trim(dsnk),'"'
      write(imp,*) 'verifier si existe: "',trim(dsnk),'"'
      inquire(file=dsnk,exist=lexist)
      if (lexist) goto 10103
      write(*,*) 'fichier (2): "',trim(dsnk),'" pas trouv‚, on le cr‚e'
      write(imp,*) 'fichier: "',trim(dsnk),'" pas trouv‚, on le cr‚e'
      open (10,file=dsnk,form='unformatted',status='new')
      goto 10102
10103 continue
      write(*,*) '"',trim(dsnk),'" existe d‚j…, le remplacer ?'
      write(imp,*) '"',trim(dsnk),'" existe déjà, le remplacer ?'
10143 write(*,*) ' RSVP  O(ui) ou N(on)'
       write(imp,*) ' RSVP  O(ui) ou N(on)'

      read(*,'(a1)',end=10180,err=10180) acmd
	  goto 10181
10180 continue
      write(*,*)'erreur/end en reponse question reusee K'
	  write(imp,*)'erreur/end en reponse question reuse K'
	  call pose(1018)
10181 continue
       write(imp,*)acmd
      call upcas1(acmd)
      if (acmd.eq. 'O') goto 10142
      if (acmd.eq. 'N') goto 203
      goto 10143
10142 continue
      open (10,file=dsnk,form='unformatted',status='old',err=10108)
	  goto 10102
	  
10108 continue
      write(*,*)'erreur ouverture ',dsnk
	   call pose(36)
10102 continue
      write(10)'sauvegarde echelle de loevinger par SlCsv '//aaaammjj
c pour voir dans le dump, ne sera pas exploité	
c le mot sauvegarde sera exigé pour rechearger échelle	
		write(10)cdate,chemin, rappel
      write(10)NBI,NBQ,NBIT,partil,absents
	  write(10) chemin,cdate,imp,dsn,dsnlog,titre
      write(10) (pp(jo),jo=1,nbit*(nbit+1)/2 ) 
c²	  write(*,*) 'ecrites pp ',nbit*(nbit+1)/2 
      write(10) (mak(jo),mqn(jo),mqu(jo),mx(jo),x(jo),jo=1,nbq)
      write(10) NBPAS,nitp,p,ipop,iwi,ici,ic,iw
c pour info: la suite n'est pas exploitee	  
       write(10)
     1 (numeit(jo),iqorit(jo),iait(jo),
     2 (lv(jo,jjp1),jjp1=1,101),jo=1,nbit)

c pour être visible par type
    	 write(10)'fichier sauvegarde echelle de loevinger'
       write(10)col5,inter2,larg,uselabels,usequestions,
     1   labeli,labelr,labelq,ls,nitp
        write(10)(qq(jjq),jjq=1,nbq)
	
	   if (uselabels)then
	   write(10)1
	   do 50711 iiq=1,nbq
	   do 50755 jjq=1,10
c	   write(*,*)'sauve ',iiq,x1(iiq,jjq),labelsprt(iiq,jjq),'!'
	   write(10) x1(iiq,jjq),x2(iiq,jjq)
	   write(10) labelsprt(iiq,jjq)
	   write(10) labelsscr(iiq,jjq)
50755  continue
	   
50711  continue	   
	   endif

	   if (usequestions) then
	     do 50781 iiq=1,nbq
		 write(10)questionsUprt(iiq)
		 write(10)questionsUscr(iiq)
		 write(10)questionsXprt(iiq)
		 write(10)questionsXscr(iiq)
		 
50781   continue
	   
	   endif
c 20171228 21h00 
         write(10)chemin80
    	 write(10)'fichier sauvegarde echelle de loevinger'
    	 write(10)'fichier sauvegarde echelle de loevinger'
    	 write(10)'fichier sauvegarde echelle de loevinger'
    	 write(10)'fichier sauvegarde echelle de loevinger'
    	 write(10)'fichier sauvegarde echelle de loevinger'
       close(10)
       write(*,*)'Ok.'
	   write(imp,*)'Ok.'
c        write(*,*) 'dsnlog:',dsnlog
c       write(*,*) 'K titre:',titre
c       write(*,*) 'K chemin:',chemin
       fsaved=.true.
C *******************
      GO TO 203
c030   WRITE(IMP,2031)     MAXIT,MAXQ
       WRITE(IMP,2031)     MAXIT,MAXQ
       WRITE(*,2031)     MAXIT,MAXQ
2031   FORMAT(/' ERREUR SUR LA CARTE ECHELLE CI DESSUS, MAXIT= ',I3,
     1' ,MAXQ= ',I3)
        call pose(37)
       
20209   WRITE(IMP,2021) lca
        write(*,2021) lca
2021   FORMAT(/'FIN DE FICHIER TROUVEE SUR LE FICHIER PARAMETRE:',i3)
       if (lca .eq. mae) then
c alerte	   
	      call pose(38)
	    endif	    
        write(*,*)'NOTE: on continue au clavier, comme si C' 
		write(imp,*)'NOTE: on continue au clavier, comme si C' 
		
       lca=mae
c GAFFE: 213 force methode A	   
       goto 203
20208   WRITE(IMP,2021) lca
       if (lca .eq. mae) call pose(39)
       lca=mae
	   END
c -------------
       subroutine direqui(nbit,nitp)
       common /io/ mae,mal,lca,imp
   
	  common /controlist/ partil,absents,listqabsentes,listqpresentes
	  logical partil,absents,listqabsentes,listqpresentes
    
	     if (listqabsentes .or.  listqpresentes .or. absents) then
	  if(listqabsentes)write(*,*)'pour questions sans item presents.'
	  if(listqpresentes)write(*,*)'pour questions avec item presents.'
	  if (absents) write(*,*) 'liste des ',nbit-nitp,' items absents'
	  else
	  if (partil) then
	     write(*,*)'pour les ',nitp,' items presents'
	   else
      write(*,*)'pour les ',nbit,' items (+:',nitp,'/ -:',nbit-nitp,')'
		endif
	  endif 

	   end
	   
	   subroutine accents(labelune,labelscr,labelprt)
       common /io/ mae,mal,lca,imp
       character*200 labelune,labelscr,labelprt
	   iprt=0
	   iaccent=0
	   labelscr=''
	   labelprt=''
	   if (len_trim(labelune).gt.0) then
c	  write(*,*)'accent',len_trim(labelune),'===',trim(labelune),'==='
      endif
	  do 4480 iiaccent=1,len_trim(labelune)
c iaccent augmenter de 2 quand caracyère double	   
	   iaccent=iaccent+1 
    	 if (labelune(iaccent:iaccent).le.'~') then
c ascii non accent, conserver 	   
	   iprt=iprt+1
	   labelprt(iprt:iprt)=labelune(iaccent:iaccent)
       labelscr(iprt:iprt)=labelune(iaccent:iaccent)
c        write(*,*)'a pos ',iaccent,' conserve'
		goto 4480
		end if
c donc sinon!	  
       i1=ichar(labelune(iaccent:iaccent))
       if (iaccent+1.le.len_trim(labelune)  )then
   	   i2=ichar(labelune(iaccent+1:iaccent+1))
	   else
	   i2=-1
	   end if
c	   write(*,*)'a pos ',iaccent,' un >~',i1,i2


c ------------- a un coup
        if (i1.eq.171 .or. i1.eq.187)then
		 write(*,*) 'precedent ' , ichar(labelprt(iprt:iprt))
c horrible!
c	     iprt=iprt+1
		 labelprt(iprt:iprt)='"'
		 labelscr(iprt:iprt)='"'
		 iaccent=iaccent+1
		 goto 448
	    endif
	
		  if (i1.eq.226)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)=''''
	   labelscr(iprt:iprt)=''''
	   iaccent=iaccent+2
c       write(*,*)'intercepte ccedille ',labelscr(1:iprt)	   
	   goto 448
       endif
	   if (i1.eq.128)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='è'
	   labelscr(iprt:iprt)='e'
	   
c       write(*,*)'intercepte ccedille ',labelscr(1:iprt)	   
	   goto 448
       endif


	   if (i1.eq.194)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ê'
	   labelscr(iprt:iprt)='e'
	   
c       write(*,*)'intercepte ccedille ',labelscr(1:iprt)	   
	   goto 448
       endif
	   if (i1.eq.187)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ê'
	   labelscr(iprt:iprt)='e'
	   
c       write(*,*)'intercepte ccedille ',labelscr(1:iprt)	   
	   goto 448
       endif
	   if (i1.eq.171)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ê'
	   labelscr(iprt:iprt)='e'
	   
c       write(*,*)'intercepte ccedille ',labelscr(1:iprt)	   
	   goto 448
       endif
	   if (i1.eq.146)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)=''''
	   labelscr(iprt:iprt)=''''
	   
c       write(*,*)'intercepte ccedille ',labelscr(1:iprt)	   
	   goto 448
       endif
c ne joue pas 
	   if (i1.eq.231)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ç'
	   labelscr(iprt:iprt)='c'
	   
c       write(*,*)'intercepte ccedille ',labelscr(1:iprt)	   
	   goto 448
       endif
	      if (i1.eq.232)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='è'
	   labelscr(iprt:iprt)='e'
	    
	   goto 448
       endif
	   if (i1.eq.233)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='é'
	   labelscr(iprt:iprt)='e'
	    
	   goto 448
       endif
	   
	   if (i1.eq.234)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ê'
	   labelscr(iprt:iprt)='e'
	    
	   goto 448
       endif
	   
	   if (i1.eq.235)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ë'
	   labelscr(iprt:iprt)='e'
	    
	   goto 448
       endif
	   
	   if (i1.eq.224)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='à'
	   labelscr(iprt:iprt)='a'
	   
	   goto 448
       endif

	   if (i1.eq.244)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ô'
	   labelscr(iprt:iprt)='o'
c	   write(*,*)'vu capeau o'
	   goto 448
       endif
	   

	   if (i1.eq.174)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='î'
	   labelscr(iprt:iprt)='i'
	   
c       write(*,*)'intercepte ichapeau ',labelscr(1:iprt)	   
	   goto 448
       endif

C ------- à deux coups

	   if (i1.eq.195) then
c	   write(*,*)'traite 195'

	   if (i2.eq.168) then
c	   write(*,*)'195 168'
	   iprt=iprt+1
	   labelprt(iprt:iprt)='è'
	   labelscr(iprt:iprt)='e'
	   iaccent=iaccent+1 
       goto 448
	   endif

	   if (i2.eq.169) then
c	   write(*,*) 'en 169'
	   	   iprt=iprt+1
	   labelprt(iprt:iprt)='é'
	   labelscr(iprt:iprt)='e'
       iaccent=iaccent+1 
    
	   goto 448
	   endif
	   
	   
c deplacer ici ç
	   if (i2.eq.167) then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ç'
	   labelscr(iprt:iprt)='c'
	   iaccent=iaccent+1 
       goto 448
	   endif

	   endif
c       sans doute de i1=195	   

c ---------- a deux coup 194	   
c	   write(*,*)'general'   
	   if (i1.eq.194) then
c²	   write(*,*)'traite 194'



	   
	   
c n°

  	   if (i2.eq.176)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='°'
	   labelscr(iprt:iprt)='.'
	   
c       write(*,*)'intercepte ichapeau ',labelscr(1:iprt)	   
	   goto 448
        endif
		
	 	
		  endif 

	
c	   write(*,*)'accent I1 non traduit a',iaccent,' i1=',i1,i2,
c     1 char(i1),char(i2)
c	   write(*,*)'precedent ',ichar(labelune(iaccent-1:iaccent-1)) 
c	   write(*,*)'accent I1 non traduit :',labelune
c	   write(imp,*)'accent non traduit a',iaccent,' i1:',i1,labelune
       


	   if(i2.eq.160) then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='à'
	   labelscr(iprt:iprt)='a'
	   iaccent=iaccent+1 
       goto 448
	   endif

	   if(i2.eq.175) then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ï'
	   labelscr(iprt:iprt)='i'
	   iaccent=iaccent+1 
       goto 448
	   endif

	   if(i2.eq.187) then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='û'
	   labelscr(iprt:iprt)='u'
	   iaccent=iaccent+1 
       goto 448
	   endif
	   
	   if (i2.eq.174)then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='î'
	   labelscr(iprt:iprt)='i'
	   
c       write(*,*)'intercepte ichapeau ',labelscr(1:iprt)	   
	   goto 448
       endif

	   if(i2.eq.137) then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='é'
	   labelscr(iprt:iprt)='e'
	   iaccent=iaccent+1 
       goto 448
	   endif
	   
	   if (i2.eq.170) then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ê'
	   labelscr(iprt:iprt)='e'
	   iaccent=iaccent+1 
       goto 448
	   endif
	   
	   if (i2.eq.171) then
	   iprt=iprt+1
	   labelprt(iprt:iprt)='ë'
	   labelscr(iprt:iprt)='e'
	   iaccent=iaccent+1 
       goto 448
	   endif
	   
	   if (i2.eq.180) then
	   	   iprt=iprt+1
	   labelprt(iprt:iprt)='ô'
        labelscr(iprt:iprt)='o'
	      iaccent=iaccent+1 
        goto 448
	   endif

	   write(*,*)'accent non traduit a i1  i2=',iaccent,i1,i2
	   write(imp,*)'accent non traduit ',i1,i2,' ' ,labelune
c sinon!
	   labelune(iaccent:iaccent+1)='??'
       iprt=iprt+1
	   labelprt(iprt:iprt)='?'
	   labelscr(iprt:iprt)='?'
	   write(*,*)'pas decode a',iaccent,i1,i2,' ',trim(labelune)
	   write(imp,*)'pas decode a',iaccent,i1,i2,' ',trim(labelune)

	   
 	   goto 4480
448    continue
c²c       write(*,*)'a',i1,i2,' traduit ',labelscr(iprt:iprt)
c²       write(imp,*)'a',i1,i2,' traduit ',labelscr(iprt:iprt)
c       write(imp,*)'a',iold,i1,i2
4480   continue

c      if (len_trim(labelune).gt.0) 
c     & 	  write(imp,*) trim(labelune),'===',trim(labelscr)
	   end
       character *8 function ltrim(a)
	   character *8 a
	   ideb=9
	   do 1 i=1,8
	   if (a(i:i).ne.' ' )then
	   ideb=i
	   goto 2
	   endif
1      continue
2       if (ideb.eq.9) then
	   ltrim=''
	   else
	   ltrim=a(ideb:8)
	   endif
       end
