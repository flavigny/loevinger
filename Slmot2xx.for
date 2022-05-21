

C     Last change:  JC   13 Jul 2012   10:29 am
      subroutine pose
      common /io/ mae,mal,lca,imp
      write(*,*) 'frappez <return>'
      pause
      stop
      end
      
      subroutine wpose(nu)
      common /io/ mae,mal,lca,imp
      return
      write(*,*) 'pause ',nu,' frappez <return>'
      pause
      return      
      end
      
      subroutine upcas1(c1)
      common /io/ mae,mal,lca,imp
      character * 1 c1
      character * 3 c3
      if (c1 .ge. 'a' .and. c1 .le. 'z')
     1 c1=char ( ichar(c1) - ichar('a') + ichar('A') )
      return
      end

      character * 1 function aplmn(vf)
      logical vf
      aplmn='+'
      if (.not. vf) aplmn='-'
      return
      end

      function a1plmn(vf)
      character * 1 a1plmn
      logical vf
      aplmn='+'
      if (.not. vf) aplmn='-'
      return
      end

      SUBROUTINE IMPTAB(NBIT,PP,LUIT,IAIT,IPOP,NBI,
     1 nmi,partil,iqorit )
      common /chetit/ ctitre,cche,cdate
      character * 80 ctitre
      character * 8 cdate,cche
      LOGICAL  IAIT (1) ,partil
      DIMENSION ILIGNE(25)  ,PP(1) ,iqorit(100)
      INTEGER * 2 PP
      CHARACTER * 4 ITEXT
      DIMENSION ITEXT(11)
      dimension nmi(nbit)
      dimension ncopr(100)
      common /io/ mae,mal,lca,imp
      DATA ITEXT/'N IJ','C IJ','W IJ','H IJ','++  ',
     1 'I 01','I 05','I 10','S 01','S 05', 'S 10'/
c      MUIT=MOD(LUIT,100)
      NUIT=MOD(LUIT,100)
      NBCO=15
C REDUIT A 20 CI DESSUS
C
      IP=0
c                                 boucle sur les l‚s

c     si partil compter les colonnes presentes
      nbitpr=0
      do 7 ibit=1,nbit
      if ( partil .and. .not. iait(ibit)) goto 7
      nbitpr=nbitpr+1
      ncopr(nbitpr)=ibit
7     continue

      DO 1 JCOG=1,NBITpr,NBCO
      NBLI=25

c                                 boucle sur les colonnes
      DO 2 JL1H=JCOG,NBITpr,NBLI
      JLIH=JL1H
c                                  JLIH numero ligne haut de le
      JLIB=MIN0(NBITpr,JLIH+NBLI-1)
c                                  JLIB numero bas de le
      JCOE=MIN0(NBITpr,JCOG+NBCO-1)
c                                   JCOE nu colonnne end (droite)
      IP=IP+1
c                                   Ip numero de page
      write(*, 8) ITEXT(NUIT) ,IP,nmi(ncopr(JCOG)),
     1 nmi(ncopr(JCOE)),
     1 nmi(ncopr(JLIH)),nmi(ncopr(JLIB))
8     FORMAT('1TABLEAU ',A8,24x,'PAGE ',I3,I9,' A ',I3,'  PAR  ' ,I3,' A
     1 ',I3)
      write(*,*) ctitre
      write(*,*) 'chemin=',cche,'   date=',cdate
      write(*, 9) (nmi(ncopr(JJ)),':',JJ=JCOG,JCOE)
9     FORMAT('-    ',25(I4,a1))
c             pour N on a la diagonale 1,1 sinon on party a 2
      IF(JLIH.EQ.1.AND.NUIT.NE.1)JLIH=2

      DO 3 JLI=JLIH,JLIB
c      IF(partil .AND. .NOT. IAIT(JLI)) GO TO 3
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
      write(*,*) ' je trouve nuit=',nuit
      return
1000  continue
      seuil=2.58
      goto 1900
1100  continue
      seuil=1.96
      goto 1900
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
2000  continue
      seuil=2.58
      goto 2900
2100  continue
      seuil=1.96
      goto 2900 
2200  continue
      seuil=1.65
      goto 2900
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
11    ILIGNE(JJ)=nij
      GO TO 30
12    ILIGNE(JJ)=CIJ+0.5
      GO TO 30
13    ILIGNE(JJ)= WIJ+0.5
      GO TO 30
   14  iligne(jj)=hij
      goto 30
15    CONTINUE
c case ++
      iligne(jj)=min-pp(ifrom)
      goto 30
30    continue
c      IF((.NOT. IAIT(JCO) .OR. .NOT. IAIT(JLI) ).AND. MUIT .GT.10)
c     1 ILIGNE(JJ)=0
33    CONTINUE
      IF(NBNB .GT.0)
     1 write(*,50) nmi(ncopr(JLI)),(ILIGNE(JJ),JJ=1,NBNB)
C IMPRIMERN LIGNES NON VIDES SEULEMENT 8 JUILLET 86
50    FORMAT('0',I3,':',25I5)
3     CONTINUE
2     write(*, 9) (nmi(ncopr(JJ)),':',JJ=JCOG,JCOE)
1     CONTINUE
      write(*, 4 )
4     FORMAT('1')
       RETURN
      END
c                                          prochaine ligne ?
      subroutine testun(lectur,mot,invite)
      character * 80 invite
      logical deja,eof
      character * 1 luavan(127)
       common /ligne/ eof,deja,luavan
      common /io/ mae,mal,lca,imp
      character * 8 mot,trav
      character * 1 mot1(8)
      i80=80
       if (deja) goto 500
       read(invite,'(80A1)') (luavan(jj),jj=1,80)
       do 501 jfin=1,80
       if (luavan(jfin).eq. ';') goto 502
501    continue
       jfin=80
502    continue
       if (lectur .ne. 9) goto 500
       write(*,*) (luavan(jj),jj=1,jfin-1)
       write(*,*)
500    continue
1      if (.not. deja)
     1 read(lectur,'(127A1)',end=1009 ) (luavan(j),j=1,i80)
c     eliminer si * en col 1
      if (luavan(1) .eq. '*') write(*,'(1x,127a1)') (luavan(j),j=1,i80)
      if (luavan(1) .eq. '*') goto 1
c      if (.not. deja) write(*,*) 'en anticipation on lit:'
c      if ( deja) write(*,*) 'en anticipation on reexamine:'
      write(*,'(1x,127a1)') (luavan(j),j=1,i80)
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
      if (.not. deja) write(*,*) 'en anticipation on lit:'
      if ( deja) write(*,*) 'en anticipation on reexamine:'
      write(*,'(1x,127a1)') (luavan(jj),jj=1,i80)
      write(*,*)' en anticipation plus de 8 caracteres ci dessus'
      call pose
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
      return
1009  continue
c      write(*,*) 'fin de fichier en anticipation repiond blanc '
      mot='        '
      eof=.true.
       lectur=mae
      return
      end
      
      subroutine anal(nulo,string,trep,arep,idim,fini,oblig)
      character * 127 string,s80
      character * 1 lu80(127),seg,t1verb(8),t1mot(8)
      logical nu,err,fini,aval,oblig,yaurep,avalnu
      character * 8 verb,tmot(20),verbr,a8va
      character * 8 arep(idim)
      dimension trep(20) ,aval(20),avalnu(20)  ,yaurep(20)
      logical servi(20)
      n80=127
      nmot=0
      do 1090 j=1,idim
      yaurep(j)= .false.
      arep(j)='        '
1090  trep(j)=0.
c      write(*,*) 'on va a proto'
      call proto(nulo,string,lu80,verb,ipos)
c      write(*,*) 'on revient de proto'
10    continue
      call avance(lu80,ipos,tmot(nmot+1),seg,lm,nu,va,a8va)
      nmp1 = nmot+1
c      write(*,*)' nmp1,tmot(nmp1),seg,nu,va,a8va',
c     1    nmp1,tmot(nmp1),seg,nu,va,a8va
c       write(*,*) ' frappez return'
c       read(*,*)
c si on a une valeur numerique et precedent est a valeur nu : ok
c     if (nu .and. (nmot.gt.1) .and. avalnu(nmot))
c    1 write(*,*) 'on va a 101 car nu et aval'
      if (nu .and. (nmot.gt.1) .and. avalnu(nmot)
     2 .and. .not. servi(nmot) ) goto 101
c     if ((nmot.gt.1) .and. aval(nmot))
c    1 write(*,*)' on va a 1019 car aval du precedent'
      if ((nmot.gt.1) .and. aval(nmot) .and.
     2  .not. servi(nmot) ) goto 1019
      if (nu .and. lm .gt. 0) write(*,*)
     1 'erreur probable (mot cle numerique) '
C si =, la longueur du mot est 0
      if (lm .le. 0 .and. aval(nmot) .and. trep(nmot) .eq. 0.)
     1  goto 1023
c     write(*,*) 'un nouveau mot nmot+1::',nmot+1,tmot(nmot+1)
      goto 102
1019   continue
c                          ranger la reponse alpha
c      write(*,*) 'ici ranger valeur alpha defaut non fait'
      servi(nmot)=.true.
      goto 1023
101   continue
c                          ranger la reponse numerique
      trep(nmot)=va
c     write(*,*) 'ranger numerique par defaut:',va
      servi(nmot)=.true.
      goto 1023
c                    1023 au lieu de 1010
102   continue
      aval(nmot+1)= (seg .eq. '=') .or. (seg .eq. ':')
      avalnu(nmot+1)=seg .eq. '='
1010  continue
      nmot=nmot+1
      servi(nmot)=.false.
c     write(*,*) 'nmot mis ™ ',nmot
1023   if (seg .eq. ',' .or. seg .eq. '=' .or. seg .eq. ':') goto 10
       if (seg .ne. ';')
     1  call messag ( 'erreur dans le prototype !',ipos)
      nuint=0
c      write(*,*) 'fin de decodage nmot=',nmot
20    call lire(nuint,nulo,lu80,127,verb,fini,oblig,ipos)
      if (fini .and. .not. oblig) return
c mettre ici le yaurep a zerpo
      do 10909 j=1,idim
      yaurep(j)= .false.
10909 continue
c     write(*,*) 'mis a false de yaurep sur ',idim
c prendre une re'ponse et analyser
26    continue
      call avance (lu80,ipos,verbr,seg,lm,nu,va,a8va)
c      write(*,*) 'retour de avance ',verbr,' num:',va,
c     1 ' alpha:',a8va
      if (lm .eq. 0 .and. ipos .gt. n80) goto 20
      if (verbr .eq. '        ' .and. seg.eq.';') return
c
c on a trouv‚ un mot voir si il satisfait un max 
      maxitr=0
      maxlon=0
      maxnb=0
      do 40 i=1,nmot
c     write(*,*) 'comparons ',verbr,'?',tmot(i)
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
c     write(*,*) 'liste des mots clefs possibles'
c      do 40999 i=1,nmot
c     write(*,*) i,'---',tmot(i),'----'
c 40999    continue
c      write(*,*) 'mot clef suivant trouve=---',verbr,'--- + ',seg
      call messag ( 'unwanted keyword !',ipos)
c voir si suivi de valeur
41    continue
      if ( (seg .eq. '=') .and. aval(i) ) goto 501
      if ( (seg .ne. '=') .and. .not. aval(i)) goto 502
      if (aval(i) ) write(*,*) i,'value keyword, missing =! sep=',seg
      if (.not. aval(i) ) call messag
     1 ('Boolean keyword, = forbidden! ',ipos)
502   trep(i)=1.
      yaurep(i)=.true.
      goto 50
501   continue
c     write(*,*) 'usage du mot ',i,' suivi de = yaurep',yaurep(i)
      if (yaurep(i) ) call messag('multiple affectation !',ipos)
      call avance(lu80,ipos,verbr,seg,lm,nu,va,a8va)
c      write(*,*) i,' affectation valeur lu numerique=',nu,' ',a8va
      if (avalnu(i)) goto 50199
      arep(i)=a8va
c      write(*,*) arep(i),'=',a8va
c       write(*,*) 'val alpha mise a true de yaurep(',i
      yaurep(i)=.true.
      goto 50
50199 continue
      if (.not. nu )
     1 write(*,*) 'Non numeric value after = sign (NAME=num)!'
      if (.not. nu) call pose
      trep(i) = va
      yaurep(i)=.true.
c      write(*,*) 'val nu mise a true de yaurep(',i
50    continue
      if (seg .eq. ',') goto 26
      if (seg .ne. ';')
     1 call messag( 'lllegal separator/ missing semi-colon !',ipos)
      return
      end
C ......................................................................
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
      write(*,*) st
      call messag
     1 ('pas de ; dans les',n80,'premieres colonnes ...',n80)
1071  continue
c      write(*,*) 'on a passe 1071 j=',j
c     write(*,*) 'prototype long=',j
c ci dessous * au lieu de 0
      ndeuxp=0
      do 1051 jj=1,j
      if (ta(jj).eq. ':') ndeuxp=ndeuxp+1
      tb(jj)=ta(jj)
      if (ta(jj).eq. ':' .and. ndeuxp.gt.1)tb(jj)='='
1051  continue
c      0 ci dessous
      if (lec.eq.mae) write(*,'(1x,127A1)') (tb(i),i=1,j)
        call avance(ta,ipos,ve,seg,lv,nu,va,a8va)
      if (seg .ne. ':') call messag( 'pas deux points !' ,ipos)
      return
      end
C .....................................................................
      subroutine lianum(type,nulo,string,i4rep,idimi,
     1 a4rep,idima,noblig)
c si si fin de liste au dela de noblig element ok.
      character * 127 string,s80
      character * 4 type,a4rep
      character * 1 lu80(127),seg
      logical nu,err,fini
      character * 8 verb,verbr,a8va
      dimension a4rep(idima),i4rep(idimi)
      common /io/ mae,mal,lca,imp
      n80=127
      nmot=0
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
      nuint=0
2620  continue
10    continue
c      write(*,*) 'en 10 on va lire '
      call lire(nuint,nulo,lu80,n80,verb,fini,.true.,ipos)
c      write(*,*) 'revenur de lire'
       if (type .eq. 'NUME') goto 4000
      if (type .eq. 'TITR') goto 5000
      if (Type .eq. 'ALPH') goto 6000
      call messag('type inconnu (prog. error) !',ipos)
4000  mul = 1
c                liste de nombres ......
4001  continue
      call avance(lu80,ipos,verbr,seg,lv,nu,va,a8va)
      if (ipos .gt. n80) goto 10
      if (.not. nu .and. lv .gt. 0)
     1   call messag( ' non numeric value in list !',ipos)
      if (seg .eq. ',' .or. seg .eq. ';' ) goto 4010
      if (seg .eq. '-' ) goto 40101
      if (seg .eq. '*' ) goto 4020
      call messag( 'invalid separator in num. list !',ipos)
40101 continue
      ivaldb=va
c      write(*,*) ' intervalle de valeurs intercept‚ db= ',ivaldb
      if (mul.ne.1) call messag('mul interdit sur intervalle',ipos)
      call avance(lu80,ipos,verbr,seg,lv,nu,va,a8va)
      if (ipos .gt. n80)
     1 call messag('intervalle sur une ligne',ipos)
      ivalfi=va
c      write(*,*) ivaldb,' generation intervalle ™',ivalfi
      do 40110 ivlint=ivaldb,ivalfi
      i4=i4+1
      if (I4 .gt. idimi )
     1 call messag ('too many value in list (interval) !',ipos)
      i4rep(i4)=ivlint
40110 continue
      goto 40190
4010  do 4015 j4=1,mul
      i4=i4+1
      if (I4 .gt. idimi )
     1 call messag ('too many value in list !',ipos)
      i4rep(i4)=va
4015  continue
40190 mul = 1
      if (i4 .lt. idimi .and. seg .eq. ',') goto 4001
      if (i4 .eq. idimi .and. seg .eq. ';') return
      if (seg .ne. ';') call
     1 messag('toutes valeurs servies, manque ; !',ipos)
c      write(*,*) 'i4=',i4,'  idimi=',idimi
      if (i4 .lt. noblig) goto 40199
      noblig=i4
c     write(*,*) 'liste tronqu‚e accept‚e'
      return
40199 continue
      call messag('; avant liste servie  !',ipos)
4020  mul=va
      goto 4001
5000  continue
c prendre une re'ponse et analyser
      do 2601 ifin=ipos,n80
      if (lu80(ifin) .eq. ';') goto 2602
2601  continue
      call messag('missing ending semi-colon !',n80)
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
      call messag( ' alpha value more than 4 characters !',ipos)
2631  continue
c      write(*,*) 'lm,ipos,i4',lm,ipos,i4 ,idima
c accepter que un point virgule tronque la liste
      if (verbr .eq. '        ' .and. seg.eq.';')
     1  write(*,*) 'fin de liste non servie'
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
      if (seg .ne. ';') call messag( 'alpha list trop longue !',ipos)
26391 fini=.false.
      return
5900  call messag(' !',ipos)
      call pose
      end
C ......................................................................
      subroutine lire(nuint,nulo,lu80,i80,verb,fini,oblig,ipos)
      character * 1 lu80(i80) ,seg
      character * 8 verbr,verb,vsuit,a8va
      logical fini,oblig ,err,nu
      character * 1 luavan(127)
      logical deja,eof
      common /ligne/ eof,deja,luavan
      common /io/ mae,mal,lca,imp
c
10    ipos=0
c      write(*,*) ' debut de lire deja=',deja
      if (deja) goto 1001
      read(nulo,'(127A1)',end=1009 ) (lu80(j),j=1,i80)
c      write(*,*) 'lu dans le fichier'
      write(*,'(1x,127A1)') (lu80(j),j=1,i80)
      if (lu80(1) .eq. '*') write(*,'(1x,127a1)') (lu80(j),j=1,i80)
      if (lu80(1) .eq. '*') goto 10
      goto 1008
1009  fini = .true.
      if (.not. oblig) return
      write(*,*) 'commande attendue:',verb
      call messag( 'end on file on reading the above command ! ',0)
1008  continue
      goto 1002
1001  continue
      do 1003 j80=1,i80
1003  lu80(j80)=luavan(j80)
c      write(*,*) 're lu dans le buffer'
c      write(*,'(1x,127A1)') (lu80(j),j=1,i80)
      deja=.false.
1002  continue
      ipos=0
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
      call messag( 'FINISH found , premature end of job !',ipos)
1105  continue
      if (verbr .eq. verb) goto 11055
      write(*,*) 'commande attendue: ',verb,':'
      call messag('COMMAND found not= expected: !',
     1 ipos)
11055 continue
      if (seg .eq. ':' .and. nuint .eq. 0) goto 1205
c    carte numerotee  touve ou necessaire
c      write(*,*) 'on a seg=',seg,' et nuint=',nuint
      if (seg .ne. '/' .and. nuint .gt. 0) call messag
     1 ('continuation card expected with VERB/number',ipos)
      call avance(lu80,ipos,vsuit,seg,lm,nu,va,a8va)
c      write(*,*) 'carte suite va=',va
      if (.not.nu .or. va .ne. nuint) call messag
     1 (' invalid sequence number !',ipos)
1205  nuint=nuint+1
      if (seg .ne. ':')
     1 call messag (' missing colon after the first word !',ipos)
      fini = .false.
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
c       write(*,*) 'demande de aavnce depuis ',ipos
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
      if (lmot .ge. mxmot)
     1 call messag('word more than height characters !',ipos)
      lmot=lmot+1
      ms1(lmot)=lu80(ipos)
      ipos=ipos+1
      if (ipos .le. mxlig) goto 39
      call messag
     1 ('keyword ended at end of line, missing separator!',ipos)
c                fin de mot
c le lire dans motsuiv
c avancer jusqua separtartet si espace
31    if (lu80(ipos) .ne. espace ) goto 35
c
36    if (ipos .ge. mxlig)
     1 call messag(
     2 ' missing ponctuation de fin de ligne, or ; !',ipos)
      ipos=ipos+1
      if (lu80(ipos) .eq. espace) goto 36
c mot, espace et different : est ce un separateur
c ci dessous 1 au lieu de 2
      do 370 j=2,nfin
      if (lu80(ipos) .eq. finmot(j)) goto 35
370   continue
      call messag('multiple word without separator !',ipos)
35    sepfin = lu80(ipos)
      write(motsui,'(8a1)') (ms1(j),j=1,8)
c if nume retourner aussi la valeur enti„re .
      if (nptvu .gt. 1)
     1 call messag( 'more than one decimal point ! ',ipos)
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
      subroutine messag(string,ip)
C a placer en bout pour eviter controle type string !
      character * 80 string
      character * 1  st1(80)
      common /io/ mae,mal,lca,imp
      read(string,'(80a1)') (st1(j),j=1,80)
      do 1 j=1,80
      if (st1(j) .eq. '!') goto 2
1     continue
      j=80
2     write(*,'(1x,127a1)') (' ',i=1,ip),'!'
      write(*,'(1x,127a1)') (st1(i),i=1,j)
       call pose
      
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
       subroutine STPA(i,date,imp,titre)
      common /chetit/ ctitre,cche,cdate
      character * 80 ctitre
      character * 8 cdate,cche
      character * 8 date
      character * 80 titre
      character * 4 nom(6)
c      CALL STPA(IPA,IDD)
      write(*,1) date,i
      cdate=date
      i=i+1
1     format(' Cevipof/Lasmas (CNRS)  Construction ‚chelle de ',
     1 'Loevinger ',a8,' page',i4/)
      write(*,*) titre
      write(*,*) 'chemin=',cche,'    date=',cdate
      return
      end
      
      subroutine histo  (p,iait,imp,nbit,mqz,numeit,iqorit,slmt)
      dimension p(nbit),numeit(nbit),iqorit(nbit)
      character * 8 mqz(nbit)
      logical slmt
c                      si true seulement les presents
      logical iait(nbit)
      character * 1 tout(132) ,trecap(132), tsave(132)
      logical prem
c240   CALL HISTO (P,IAIT,IMP,NBIT,MQ,numeit,iqorit)
      write(*,*)
     1 '        :',
     2 '    0         20        40        60        80       100:'
      write(*,*)
     1 '---------',
     2 '---------------------------------------------------------'
      prem=.true.
      do 1141 iout=1,132
1141   trecap(iout)=' '

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
      iout=5+ 50*p(ibit)
      
      if (slmt .and. .not. iait(ibit) ) goto 300
      if (iait(ibit)) goto 380
c  le - ne vient que si pas deja un desous 
      if (tout(iout).eq.' ') goto 38050
      write(*,*) '        ','=',(tsave(jout),jout=1,56),'='
      do 14144 ioutf=1,132
14144   tout(ioutf)=' '
      
38050 tout(iout)='-'
      if (trecap(iout) .eq. ' ') trecap(iout)='-'
      goto 381

380   continue
c     mettre item present
      if (tout(iout).eq.' ') goto 38001
c     collision
      write(*,*) '        ','=',(tsave(jout),jout=1,56),'='
      prem=.true.
      goto 10000

38001 tout(iout)='+'
c     on ne gere pas les collision sur trecap
      trecap(iout)='+'
381   continue
      inum=numeit(ibit)
310   iout=iout-1
      if (inum .le. 0) goto 300
      i1=inum/10
      i1=inum - 10 *i1
      inum=inum /10
      if (tout(iout).eq.' ') goto 320
c ici faudrait avoir sauvegarde r et recommencer
      write(*,*) '        ','=',(tsave(jout),jout=1,56),'='
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
      write(*,*) mqz(iqorit(ibit)),':',(tout(jout),jout=1,56),':'
      prem=.true.
120   continue
100   continue
      write(*,*)
     1 '---------',
     2 '---------------------------------------------------------'
      write(*,*) '        ',':',(trecap(jout),jout=1,56),':'
      return
      end
c***************************************************** pp
      DIMENSION         NUMQ(99),MAK(99)  , PP(6000)
      INTEGER * 2 PP
      EQUIVALENCE (LZ,NUIT)
      CHARACTER * 4 IZIZ,IA,V,METHTA
      character*8 mqz,mq
      DIMENSION IVV(10),MQZ(99)
     1, IPOP(100)
      CHARACTER * 4 IHY,IHZ,IHP,IVV
      CHARACTER * 4 IBL
     1 , IHA,IHC,IHD,IHE,IHG,IHL,IHM,IHS
     2 ,METH
      character * 1 meth1
      DIMENSION IR(200)
      CHARACTER * 8 IBL8,NOMCOP
      character * 8 chemin
      DIMENSION NOMCOP(500)
C ******* A REMETTRE A BONNE VALEUR
      DOUBLE PRECISION NBJ
      CHARACTER * 8 IDD,NOMCOD,ida
      DIMENSION NOMCOD(99,10)
      DOUBLE PRECISION ICI(100),IWI(100),IC,IW,ICIJ,IWIJ,ICII,IWII,FDIV
      DIMENSION CI(100),WI(100),P(100)
c                         iait a true si item present actuellelmment
      LOGICAL  IAIT (100)
c                         LV (item code) a true si le code inclu dans item
      logical LV(100,10)
      EQUIVALENCE(V,METH,IA)
      LOGICAL MACHIN         ,IMPINT    ,LUTAB ,IMPIT,IMPIGN
      character * 36 dsn,dsnlu
c  variables ajoutess pour mot clefs
      real tsave(20)
      character*8 asave(20),proch,arappl
      logical fini,partil
c
      integer i4rep(20)
      character * 4 a4rep(20)
c tableau des noms d'items
      integer*4 numeit(100)
      integer lircol(99)
c
c  iqorit indique le ranq de la question dont est tir‚ item .
      integer iqorit(100)
c tit indique le vecteur 0 ou 1 pour calculeer ni
c      integer tit(100,10)
      logical  irepit(100)
c                             irepit profil un indu=vidu
      character * 1 tlu(5000)
c                              pour lire une ligne fichier
      character*80 dsndat,trav80  ,titre
c                            pour lire le fihcier de donness
      logical lexist
c                       pour inquire
       character * 4 mon,nom,mal
       logical hseul,fa    ,fotitr
       logical fsaved
      
      common /chetit/ ctitre,cche,cdate
      character * 80 ctitre
      character * 8 cdate,cche

       character * 1 acmd
       character * 1 luavan(127)
       logical eof,deja,mac,pc
       common /ligne/ eof,deja,luavan
      common /io/ mae,mal,lca,imp
C                                **
C                                **
C     ----------------------- INITIALISATION DES CONSTANTES ALPHA .
      DATA MAXIT/100/,MAXQ/99/,maxlig/5000/
      DATA IBL8/' '/
      DATA IHA,IHC,IHD,IHE,IHG,IHL,IHM,IHS      /'A','C','D','E','G'
     1 ,    'L','M','S'/
      DATA IHY,IHZ/'-','+'/
      DATA IHP/'P'/
      IDINF(I,J)=(I*(I-1))/2+J
C    ------------------------ AFFECTATION DES UNITES D ENTRE SORTIE
C     EN CAS D UTILISATION AVEC UNE CONSOLE MAE ET MAL LA DESIGNENT
c      open(6,file='con')
      fsaved=.true.
c                        mae=0 pour pc    9 pour mac
      mac=.false.
c il ya un pb ci dessus ....
      MAE=0
      if (mac) mae=9
      partil=.false.
c             mae = machine ou operateur lit et programme ‚crit
      IMP=0
       if (mac) imp=9

      IPA=1
      hseul=.false.
      nbpas=1
c               (nbpas ne sert que si on ne recharge pas une vieille)
       deja=.false.
      minlrl=0
c                   notera la longuier de ligne neceszsaire
C ******
c ne pas utiliser mae avant adffectation
      write(*, 3001)
3001  FORMAT('0VERSION AU 19 5 72,',
     1 ' MS Fortran du 22/8/88 KHI2 ET C, W, H, EN DOUBLE PRECIS.'/
     2 ' Version mot-clef du 14 Juillet 1991 ')
      write(*,*) ' Version Implication du 4 Juillet 1997'
      write(*,*)
      write(*,*) 'en g‚n‚ral, vous pourrez obtenir de l''aide en'
     1 , ' frappant ?<return>'
      write(*,*) 'entr‚e ?'
      write(*,*)
     1 'nom_de_fichier<RETURN> si vous avez pr‚par‚ votre ‚chelle'
      write(*,*)
     1 '             *<RETURN> pour travailler en conversationnel'
c   * donc 0 ou 9 selon pc ou mac !
      read(*,'(a)') dsn
      lca=3
      read(dsn,'(A1)') tlu(1)
      if (tlu(1).eq.'*') lca=mae

c lecture de la carte date (une seule par execution )
       if (lca .eq. mae) goto 3008
      inquire(file=dsn,exist=lexist)
      if (lexist) goto 3007
      write(*,*) 'fichier ',dsn,' pas trouv‚'
      call pose

3007   continue
      open(3,file=dsn,form='formatted',status='old')
3008   continue
       call anal(lca,
     1  'LOEVINGR: CHEMIN: , DATE: , RAPPEL: , TITRE ;',
     2  tsave,asave,4,fini,.true.)

c        write(*,*) 'retour de anal loevingr'
       chemin=asave(1)
       cche=chemin
       idd=asave(2)
       arappl=asave(3)
       fotitr=tsave(4).ne.0
c       write(*,*) 'fotitr=',fotitr
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
61014   write(*,*) 'sauvegarde impossible'
        goto 61013
61013   continue
c        write(*,*) 'jfchmn=',jfchmn,'====',chemin,'===='
C                              ******
C                                **
C                                **
671   CONTINUE
C       ********************     D   R   OU  B
199   CONTINUE
      nob=1
c      write(*,*) 'arappel=',arappl,'======='
      if (arappl.eq.'        ') GO TO 66
c                    12345678
c                                    reprise
      read(arappl,'(a1,6x,a1)') tlu(1),tlu(2)
c      write(*,*) ichar(tlu(1)),ichar(tlu(2))
      if (tlu(1).eq. ' ' .and. tlu(2).eq. ' ') goto 66
      read(chemin,'(8a1)') (tlu(jche),jche=1,8)
c      write(*,*) 'on va faire un read interne de ====',nom,'==='
      read(arappl    ,'(5x,3a1)') (tlu(jche),jche=9,11)
      idf=9
      if (tlu(9).eq. ' ') idf=10
      if (tlu(10).eq. ' ') idf=11
      write(dsn,'(12a1)')( tlu(jche),jche=1,jfchmn),'.',
     1 (tlu(jche),jche=idf,11)
50372 continue
      write(*,*) 'lecture de ',dsn
      dsnlu=dsn
      inquire(file=dsn,exist=lexist)
      if (lexist) goto 50732
      write(*,*) 'fichier ',dsn,' pas trouv‚'
       call pose
      
50732 continue
      open(10,file=dsn,form='unformatted',status='old')
C *******************
       READ(10) NBI,NBQ,NBIT,
     1 (pp(jjpo),jjpo=1,nbit*(nbit+1)/2 ) ,
     2 (mak(jjpo),mqz(jjpo),jjpo=1,nbq),IDA,
     4 NBPAS,nitp,p,ipop,iwi,ici,ic,iw
     
       read(10)
     1 (numeit(jjpo),iqorit(jjpo),iait(jjpo),
     2 (lv(jjpo,jjp1),jjp1=1,10),jjpo=1,nbit)
       close( 10)
       fsaved=.true.
C *******************
       NBPAS=NBPAS+1
        write(*,*) 'suite ipoa=',ipa
        CALL STPA(IPA,IDD,imp,titre)
      WRITE(IMP,1010) IDA       ,NBPAS
1010   FORMAT(' SUITE DU    '     , A8,I3,' EME PASSAGE   ');
       nbord=0
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
       if (.not. fotitr) goto 66066
      call lianum('TITR',lca,'TITRE: ...;',i4rep,20,a4rep,20,nob)
      write(trav80,'(20a4)') (a4rep(jt),jt=1,20)
      read(trav80,'(a80)') titre
      write(*,*) 'titre lu =',titre
      ctitre=titre
66066  continue
       call anal(lca,
     1  'ECHELLE: IMPIT ; ',
     2  tsave,asave,2,fini,.true.)
      if (.not. fini) goto 6605
      write(*,*)'***** fin de fichier en lecture echelle'
       call pose
      
6605   continue
      IMPIT= tsave(1).ne.0
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
      write(*,*)'dsndat=',dsndat
      nbq=0
       nbit=0
c pb ci dessous si ....
      IF( MOD(I,25).EQ.0.AND.IMPIT)CALL STPA(IPA,IDD,imp,titre)
70    continue
c      write(*,*) 'est ce une question ?'
      call testun(lca,proch,'QUESTION ou autre;')
70000 ia=' '
      if (proch.ne.'QUESTION') goto 70999
c encore une question
c      write(*,*) 'oui'
      call anal(lca,
     1 'QUESTION: NOM: , COL= , MAX= ;',
     2 tsave,asave,3,fini,.true.)
      if (fini) goto 665
      GOTO 666
665   WRITE(MAE,'('' FIN DE FICHIER SUR UNITE LOGIQUE '',I3,
     1 '' EN LISANT QUESTION'')') LCA
       call pose
      
666   CONTINUE
c       do 66601 jlist=1,3
c 66601 write(*,*) jlist,' ',tsave(jlist),' ',asave(jlist)
      if (nbq .lt. maxq) goto 66608
      write(*,*) 'le programme est limit‚ ™ ',maxq,' questions'
       call pose
      
66608 continue
      nbq=nbq+1
      i=nbq
      mak(i)=tsave(3)
      if (mak(i) .gt. 0) goto 66680
      write(*,*) 'maximum nul ci dessus'
       call pose

66680 continue
      nbitdq=0
      mqz(i)=asave(1)
      lircol(i)=tsave(2)
      if (lircol(i) .gt.0) goto 66637
      write(*,*) 'code col= manquant ou nul ci dessus'
       call pose

66637 continue
      if (lircol(i) .gt. minlrl) minlrl=lircol(i)
c mqz passe en alpha
c ici vien la lecture des items deduits de la question
70777 continue
      call testun(lca,proch,'ITEM, QUESTION ou autre;')
      ia=' '
      if (proch.ne.'ITEM') goto 70888
c encore une question
      if (nbit .lt. maxit) goto 70666
      write(*,*) 'le programme est limit‚ ™ ',maxit,' items'
       call pose

70666 continue
      nbit=nbit+1
      nob=1
      nob=2
      call lianum('NUME',lca,
     1 'ITEM: ;',i4rep,20,a4rep,20,nob)
c      write(*,*) 'memoriser item nombre:',nob
      if (i4rep(1) .gt. 0) goto 70333
      write(*,*) 'numero item nul ou manquant ci dessus'
       call pose

70333 continue
      numeit(nbit)=i4rep(1)
c                                   verifier dupplication
      if (nbit .le. 1) goto 70706
      do 70709 jbit=1,nbit-1
      if (numeit(nbit) .eq. numeit(jbit)) goto 70708
70709 continue
      goto 70706
70708 continue
      write(*,*) 'numero d''item duppliqu‚ ci dessus'
       call pose

70706  continue
c                          question origine de l'item
      iqorit(nbit)=i
      do 72089 icode = 1,10
      lv(nbit,icode)=.false.
72089 continue
c       tit(nbit,icode)=0
      do 72091 icode = 2 , nob
      if (i4rep(icode).gt.9) write(*,*) 'hors code !'
c     if (i4rep(icode) .ge. 0 .and. i4rep(icode).le.9)
c    1  tit(nbit,i4rep(icode)+1)=1
      if (i4rep(icode) .ge. 0 .and. i4rep(icode).le.9)
     1  lv(nbit,i4rep(icode)+1)=.true.
72091  continue
       goto 70777
70888  continue
c                on a une suivante non item
c ancine 70 sur question
      goto 70000
70999 continue
c      write(*,*) 'ligne non question, on change'
      DO 92 I=1,MAXIT
92    IAIT(I)=.FALSE.
c ici etait ancienne boucle sur les items
      write(*,*) 'Comptage en cours ....',minlrl
      nbi=0
c ouverture fichier
      write(*,*)' on va lire ',dsndat
      inquire(file=dsndat,exist=lexist)
      if (lexist) goto 70997
      write(*,*) 'fichier ',dsndat,' pas trouv‚'
       call pose

70997 continue
      if (minlrl .le. maxlig) goto 70991
      write(*,*) 'la longueur de ligne est limit‚e ™ ',maxlig
       call pose

70991 continue
       write(*,*) ' on va ouvrir'
      open(10,file=dsndat,form='formatted')
      write(*,*) 'fichier de donnes ouvert   '
c                    sera incremente
      NBNB=(NBIT*(NBIT+1))/2

      DO 7099  J=1,NBNB
7099  PP(J)=0
710   continue
      read(10,'(1000a1)',end=700) (tlu(jlu),jlu=1,minlrl)
      goto 720
700   continue
      write(*,*) 'fin de fichier individus nbi=',nbi
      if (nbi .gt. 1) goto 780
      write (0,*)' fichier individu vide !'
       call pose

720   continue
      nbi=nbi+1

      do 721 iit=1,nbit
c a priori la reponse de l'individu est non
      irepit(iit)= .false.
      irepq=ichar(tlu(lircol(iqorit(iit))))-48
      if (irepq.eq.-16) irepq=0
      if (irepq.ge.0 .and. irepq .le.9) goto 722
      write(*,*) 'item rang ',iit, '  reponse :',irepq
      write(*,*) 'individu:',nbi,' rang item:',iit ,' rang question:',
     1 iqorit(iit),' colonne orig:',lircol(iqorit(iit))
        call pose

722   continue
c  irepq est la reponse
      if (  lv(iit,1+irepq) ) irepit(iit)= .true.
721   continue
c     if (irepit(1).ne.0 )
c    1 write(*,'(1x,10i1)') (irepit(iit),iit=1,nbit)

      do 741 iit=1,nbit
      do 741 jjt=1,iit
      if ((iqorit(iit) .eq. iqorit(jjt)) .and. (iit.ne.jjt)
     1 .and. (irepit(iit).or.irepit(jjt)) ) goto 742
      if ( .not. (irepit(iit) .and. irepit(jjt))) goto 741
742   continue
      idinfx=idinf(iit,jjt)
c                        ++
      PP(IDINFx)=PP(IDINFx)+1
c      IDINF(I,J)=(I*(I-1))/2+J
741   continue
      goto 710
780   continue
c       write(*,'(a8,10i3)') 'numeit:',(numeit(iii),iii=1,10)
      do 7909 iit=2,nbit
      do 7909 jjt=1,iit -1
c       write(*,*) iit,jjt, pp(idinf(iit,jjt))
      if (pp(idinf(iit,iit)) .gt. pp(idinf(jjt,jjt)))
     1 pp(idinf(iit,jjt))=pp(idinf(jjt,jjt))-pp(idinf(iit,jjt))
      if (pp(idinf(iit,iit)) .le. pp(idinf(jjt,jjt)))
     1 pp(idinf(iit,jjt))=pp(idinf(iit,iit))-pp(idinf(iit,jjt))
7909   continue
      close(10)
660   continue
1100  continue
C
C
C  ----------------          DEBUT DE LA BOUCLE DE LECTURE DES TQBLEAUX
998   continue
c        write(*,*) 'date=====',idd,'====='
      CALL STPA(IPA,IDD,imp,titre)
C                              ******
C                               ****
C                                **
c
C                              ******
C                               ****
C                                **
C                                **
C                               ****
C                              ******
C     ---------------------- CHOIX DE LA METHODE AD OU SOUS
C                              ******
C                               ****
C                                **
C                                **
C                               ****
C                              ******
      fsaved=.false.
213   continue
      
       meth1='A'
214   write(*,*) 'initialisation  ',meth1
      call wpose(1)
      fsaved=.false.
      NITP=0
      IC=0.D0
      IW=0.D0
      h=0
      NBORD=0
      DO 202 I=1,NBIT
      if (nbi .eq. 0) write (*,*)' nbj nul, cela va mal se passer'
      P(I)=(1.*PP(IDINF(I,I)))/NBI
      IPOP(I)=PP(IDINF(I,I) )
      IWI(I)=0.D0
      ICI(I)=0.D0
202   IAIT(I)=METH1 .EQ. 'S'
      call wpose(2)
      IF(METH1 .EQ. 'A') GO TO 203
c               comptage initial du S
      NITP=NBIT
c      write(*,*) 'calcul en 222'
      call wpose(3)
      DO 223 I=1,NBIT
      ICII=0.D0
      IWII=0.D0
      DO 222 J=1,NBIT
      IF (iqorit(I) .EQ. iqorit(J)) GO TO 222
      IWII=IWII+MIN0(IPOP(I),IPOP(J))*(NBI-MAX0(IPOP(I),IPOP(J)))
      IF(J .GE. I) GO TO 224
      ICII=ICII+PP(IDINF(I,J))
       GO TO 222
224   ICII=ICII+PP(IDINF(J,I))
222   CONTINUE
       IWI(I)=IWII
      IW=IW+IWII
      IC=IC+ICII
      ICI(I)=ICII
c      write(*,*) 'li:',i,'  ici(i):',ici(i),'  iwi(i):',iwi(i)
223   continue
c      write(*,*) 'ic=',ic,'   iw=',iw
      call wpose(4)
       METH=IHL
c                     continuer en faisant uine liste
       meth1='L'
      GO TO 230
C        POUR LISTER L ECHELLE ET CALCULER LE  H
C                              ******
C                               ****
C     -------------------------- DONNEZ VOTRE ORDRE
c                                  si on a de l'avance traiter sasn lire
203   continue
20399 continue

21255 if (lca .eq. mae) write(*,*) '?'
        READ(lca,'(80a1)',END=20209) (tlu(jlu),jlu=1,80)
       if (lca.ne.mae) write(*,'(1x,80a1)') (tlu(jlu),jlu=1,80)
c eliminer commentaires
c     avance ™ non espace
      jprem=0
21288 jprem=jprem+1
      if (tlu(jprem).eq.' ' .and. jprem .lt. 79) goto 21288
      if (tlu(jprem).eq. '*') goto 21255
      meth1=tlu(jprem)
      call upcas1(meth1)
c      write(*,*) 'commande:',meth1
      if (meth1 .ne. '?') goto 21388
      write(*,*) ' Help, d‚veloppement en cours'
      write(*,*) ' C  donne la main au clavier'
      write(*,*) ' L  donne la liste des items (Hi, Ci, Wi) '
      write(*,*) ' A  partir/repartir en additif (aucun item pr‚sent)'
      write(*,*) ' S  partir/repartir en soustractif (tous les items',
     1 ' pr‚sents'
      write(*,*) ' T  donne un Tableau des Nij, Cij, Wij, Hij  .'
      write(*,*) '                         tn   tc   tw   th'
      write(*,*) ' F  affichage de tous les items (+ ou -)'
      write(*,*) ' P  affichage des seuls items pr‚sents'
      write(*,*) ' G  Graphique des popularit‚s par question & items'
      write(*,*) ' Kabc  conserver cette ‚chelle avec le nom chemin.abc'
      write(*,*) ' K     conserver une ‚chelle avec son nom original'
      write(*,*) ' Q   Quitte, si deniere modif non sauv‚es, confirmer'
     1 ,' par E (End)'
      goto 203

21388 continue
      if (meth1 .ne. 'C') goto 10130
      lca=mae
      nbord=0
      write(*,*) 'entr‚e au clavier'
      goto 203
10130 continue
      if (METH1 .NE. 'Q') goto 10139
      if (fsaved) goto 10132
      write(*,*) 'quit demand‚, ‚chelle non sauv‚e'
10133      write(*,*) 'frappez A(bandon) ou E(nd)'
      read(*,'(a1)') acmd
      call upcas1(acmd)
      if (acmd.eq. 'E') goto 10132
      if (acmd.eq. 'A') goto 203
      goto 10133
10132 continue
       call pose

10139 continue
      IF(METH1 .EQ. 'K')GO TO 101
c dito
      IF(meth1 .EQ. 'L') GO TO 230
      IF(meth1 .NE. 'T') GO TO 555
      nuit=0
      meth1=tlu(jprem+1)
      call upcas1(meth1)
      if (meth1 .eq. 'N') nuit=1
      if (meth1 .eq. 'C') nuit=2
      if (meth1 .eq. 'W') nuit=3
      if (meth1 .eq. 'H') nuit=4
      if (meth1 .eq. 'I') nuit=6
      if (meth1 .eq. 'J') nuit=7
      if (meth1 .eq. 'K') nuit=8
      if (meth1 .eq. 'S') nuit=9
      if (meth1 .eq. 'T') nuit=10
      if (meth1 .eq. 'U') nuit=11
      if (nuit .ne. 0) goto 21111
      write(*,*)
     1 'La commande T doit etre suivie d''une lettre: ',
     1 ' N C W H I j k S t u !'
      goto 203
21111 continue
      CALL IMPTAB(NBIT,PP,NUIT,IAIT,IPOP,NBI,numeit,partil,iqorit)
      write(*,*) ' '
      GO TO 203
555   IF(meth1 .EQ. 'A' .OR. meth1 .EQ. 'S'
     1 ) GOTO 214
C                                **
C                               ****
C                              ******
      IF(METH1.NE.IHY.AND.METH1.NE.IHZ)GO TO 205
C     -----------------------  AJOUTE OU OTER UN ITEM
c prendre le numero d'item
c  ici faut decode nombre
      nuit=0
20077 jprem=jprem+1
      if (tlu(jprem).eq.' ' .and. jprem .lt.80) goto 20077
c     construire l nopmbre
20075 if (tlu(jprem) .lt. '0' .or. tlu(jprem) .gt. '9') goto 20076
      nuit=10*nuit+ichar(tlu(jprem))-ichar('0')
      jprem=jprem+1
      goto 20075
20076 if (tlu(jprem ) .eq. ' ')goto 20074
      write(*,*) 'La commande +/- est suivie d''un nombre invalide'
      goto 203
20074 continue
c convertir nuit
       do 11080 iuit=1,nbit
       if (nuit .eq. numeit(iuit)) goto 11081
11080  continue
       write(*,*) 'le numero d''item demand‚ n''esiste pas'
       goto 203
11081 continue
      nuit=iuit
       IF(METH1.EQ.'+' .AND.IAIT(NUIT)     ) GO TO 206
       IF(METH1.EQ.'-'.AND. .not. IAIT(NUIT))  goto 206
       GO TO 208
206   WRITE(MAE,209) numeit(NUIT)
209   FORMAT(1H ,I4,' ITEM A OTER  ABSENT  OU AJOUTER PRESENT'   )
      GO TO 203
208   IAIT(NUIT)= .not. iait(nuit)
      fsaved=.FALSE.
      IS=-1
      IF (IAIT (NUIT))IS=+1
      NITP=NITP+IS
c      write(*,*) ' calcul en 211'
      DO 211 I=1,NBIT
c
c voir ce qui etait ciic
      IF (iqorit(I) .EQ. iqorit(nuit)) GO TO 211
      IWIJ=IS*MIN0(IPOP(I),IPOP(NUIT))*(NBI-MAX0(IPOP(I),IPOP(NUIT)))
      IF(I .LE. NUIT) ICIJ=PP(IDINF(NUIT,I))
      IF(I .GT. NUIT) ICIJ=PP(IDINF(I,NUIT))
      ICI(I)=ICI(I)+IS*ICIJ
      IWI(I)=IWI(I)+IWIJ
      IF( .NOT. IAIT(I)) GO TO 211
      IC=IC+2*IS*ICIJ
      IW=IW+2*IWIJ
211   CONTINUE

      hseul=.true.
      GO TO 230

C    -------------------------- AUTRES COMANDES
205     continue
        if (meth1 .eq. 'P') partil=.true.
        if (meth1 .eq. 'F') partil=.false.
        if (meth1 .eq. 'P' .or. meth1 .eq. 'F') goto 203
        IF(METH1.EQ.'G')GO TO 240
        IF(METH1 .EQ. 'L')GO TO 230
C     ----------------------- COMMMANDE INEXISTANTE
       if (meth1 .ne. ' ') WRITE(MAE,242)  meth1
242    FORMAT(' WAT DO IOU OUANT  ',a4     )
      GO TO 203
C      ---------------------  DEMANDE DU GRAPHIQUE  HIJ, P%I.
240   continue
      CALL HISTO (P,IAIT,IMP,NBIT,MQz,numeit,iqorit,partil)
      GO TO 203
C       --------------------- CALCUL DE H
230   continue
      IF(NITP .LE. 1) GO TO 235
       FDIV=NITP*NITP
       FDIV=FDIV*NBI
      C=IC/FDIV
      W=IW/(FDIV*NBI)
c      WRITE(MAE,*) ic,iw,C,W,NITP
      H=1-div (C,W)
      WRITE(MAE,232) H,C,W,NITP
232   FORMAT('0H=',F6.3,3X,'(C=',F6.3,'  W=',F6.3,')  ',I5,' ITEMS PRESE
     1NTS')
235   continue
      IF(METH1 .NE. 'L')GO TO 203
c ancienne ligne ci dessous on ne pouvait faire liste si 0 item presents
c      IF(METH .NE. 'L   '.OR.NITP.EQ.0)GO TO 203
C     ----------------------- LISTE DES ITEMS PRESENTS
  526 continue
c        write(*,*) 'page=====',ipa,'====='
       CALL STPA(IPA,IDD,imp,titre)
      WRITE(IMP,232)H,C,W,NITP
      WRITE(IMP,251)
 251   FORMAT(43H0IT   C(I)    W(I)    P(I)    H(I)    ET SI          )
      FDIV=NBI*NITP

      DO 250 I=1,NBIT
C     C    =ICI(I)/(1.*NBI*NITP)
C     W    =IWI(I)/(1.*NBI*NBI*NITP)
c ci desous mis div
c     C=ICI(I)/FDIV
c     W=IWI(I)/(FDIV*NBI)
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
      IF(i .ne.1 .and. iqorit(i) .eq. iqorit(i-1)  )GO TO 550
C      CHERCHER LA QUESTION DE L ITEM
      j=iqorit(i)
      MAKJ=MAK(J)
c      WRITE(IMP,254)MQ.....(I),(NOMQ(J,K),K=1,6)
        WRITE(IMP,254)MQz(j)
c                            ici opn pourrait mettre lf
c                                 4 au lieu de 6 :::::
254   FORMAT(' ',20X,a8,1X,6A4)
550   continue
c                         mettre le vecteur des codes presents
      DO 553 JJ=0,MAKJ
      NOMCOP(JJ+1)=IBL8
      IVV(JJ+1)=IHY
      IF (.NOT. LV(I,JJ+1)) GO TO 553
      IVV(JJ+1)=char(jj+48)
c      NOMCOP(JJ+1)=NOMCOD(J,JJ+1)
553   CONTINue
      if (partil .and. .not. iait(i)) goto 250

c     WRITE(IMP,252) numeit(I),C    ,W    ,P(I),HP,HPP,IAIT(I),
c    1 mqz(J),
c    2 (IVV(K+1),                      K=0,MAKJ)

      WRITE(IMP,252) numeit(I),C    ,W    ,P(I),HP,HPP,IAIT(I),
     2 (IVV(K+1),                      K=0,MAKJ)

250    continue
252   FORMAT(1x,I4,3F8.3,  2F8.3,3X,L1,1x,10(1X,A1))
      write(*,*) '  '
      GO TO 203
9999   call pose
        
101   CONTINUE
c le dsn est de jprem+1 ™ espace
      dsn=dsnlu
      write(mon,'(3A1)') (tlu(jprem+ilu),ilu=1,3)
      if (mon .ne. '   ') goto 10244
c  verifier dsn existe
      read(dsn,'(a1)') tlu(1)
      if (tlu(1).ne. ' ') goto 10244
      write(*,*) 'vous devez donner un nom … la sauvegarde'
      goto 203

10244 continue
      write(*,*) 'jfchmn=',jfchmn
      read(chemin,'(8a1)') (tlu(jche),jche=1,8)
      read(mon    ,'(3a1)') (tlu(jche),jche=9,11)
      write(dsn,'(12a1)')( tlu(jche),jche=1,jfchmn),'.',
     1 (tlu(jche),jche=9,11)
10144  continue
       WRITE(MAE,'(/'' MEMORISATION SUR DISQUE dans '',a12/)') dsn
C *******************
      if (dsn .ne. dsnlu)  goto 10101
      write(*,*) 'sauvegarde sur le meme en old donc'
      open (10,file=dsn,form='unformatted',status='old')
      goto 10102
10101 continue
c     write(*,*) 'sauvegarde different'
      inquire(file=dsn,exist=lexist)
      if (lexist) goto 10103
      write(*,*) 'fichier ',dsn,' pas trouv‚, on le cr‚e'
      open (10,file=dsn,form='unformatted',status='new')
      goto 10102
10103 continue
      write(*,*) 'ce fichier existe d‚j… le remplacer ?'
10143 write(*,*) ' RSVP  O(ui) ou N(on)'

      read(*,'(a1)') acmd
      call upcas1(acmd)
      if (acmd.eq. 'O') goto 10142
      if (acmd.eq. 'N') goto 203
      goto 10143
10142 continue
      open (10,file=dsn,form='unformatted',status='old')
10102 continue
        write(10)NBI,NBQ,NBIT,
     1 (pp(jjpo),jjpo=1,nbit*(nbit+1)/2 ) ,
     2 (mak(jjpo),mqz(jjpo),jjpo=1,nbq),
     3 IDA,NBPAS,nitp,p,ipop,iwi,ici,ic,iw

       write(10)
     1 (numeit(jjpo),iqorit(jjpo),iait(jjpo),
     2 (lv(jjpo,jjp1),jjp1=1,10),jjpo=1,nbit)
       close(10)
       fsaved=.true.
C *******************
      GO TO 203
2030   WRITE(IMP,2031)     MAXIT,MAXQ
2031   FORMAT('0 ERREUR SUR LA CARTE ECHELLE CI DESSUS, MAXIT= ',I3,
     1' ,MAXQ= ',I3)
        call pose
       
20209   WRITE(IMP,2021) lca
2021   FORMAT('0FIN DE FICHIER TROUVEE SUR LE FICHIER ',i3,', STOP'/)
       if (lca .eq. mae) call pose
       lca=mae
       goto 213
20208   WRITE(IMP,2021) lca
       if (lca .eq. mae) call pose
       lca=mae
       goto 20399
      END

