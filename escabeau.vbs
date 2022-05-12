option explicit
dim oFs
set oFS=createobject("scripting.filesystemobject")

dim sFi
dim oArgs
set oArgs=Wscript.arguments
if oArgs.count<>1 then 
  msgbox "Lorgnette pour construction échelle de Loevinger sur enquête  monstrueuse" & vbcrlf & _
    "Extraction de colonnes d'un trriplet .csv, .questions.csv .labels.csv " & vbcrlf & _
   "pour utiliser, draguez le fichier 'extrait'" & vbcrlf & _
   "lire: http://antiguide.free.fr/wiki/wakka.php?wiki=SlCsv2019 "
  wscript.quit
end if
sFi=ucase(oArgs(0))

dim obli
obli=".TXT"
if mid(sFi,len(sFi)-len(obli)+1)<>obli theN
  msgbox "erreur:" & mid(sFi,len(sFi)-len(obli)+1)
  wscript.quit
end if

dim oFi
set oFi=oFs.opentextfile(sFi)

dim sRadical
dim sRacine
sRacine=mid(sfi,1,len(sfi)-4)

dim iradical
iradical=instrrev(sracine,".")
sradical=mid(sracine,iradical+1)
sracine=mid(sracine,1,iradical-1)

dim sFo,sFq,sFl
sFo=sracine & "." & sradical & ".csv" 
sFq=sracine & "." & sradical & ".questions.csv" 
sFl=sracine & "." & sradical & ".labels.csv" 
dim oFo,oFq,oFl
set oFo=oFs.createtextfile(sFo)
set oFq=oFs.createtextfile(sFq)
set oFl=oFs.createtextfile(sFl)
ofo.write ";"
dim list(10000)
dim used(10000)
dim found(10000)
dim out
out=""
dim nbq
nbq=0
dim ivar
ivar=1
for iq=1 to 2000
 used(iq)=false
next

while not ofi.atendofstream
dim ligne

 ligne=ucase(oFi.readline)
 if len(trim(ligne))>0 then 
 if instr(ligne,";")>0 then ligne=mid(ligne,1,instr(ligne,";")-1)
 nbq=nbq+1
 list(nbq)=ligne
out=out &ligne&";"
end if
wend
'msgbox out
' lecture .csv
dim oFcsvin,oFqin, Oflin
set oFcsvin=oFs.opentextfile(sracine & ".csv")
set oFqin=oFs.opentextfile(sracine & ".questions.csv")
set oFlin=oFs.opentextfile(sracine & ".labels.csv")

' entete de question
ofq.writeline ofqin.readline
dim nbqin
nbqin=0
dim vu(2000)
for iq=1 to nbq
 vu(iq)=false
next
'msgbox "filtrer"
ivar=1
while not ofqin.atendofstream
  ligne=ucase(ofqin.readline)
ivar=ivar+1
  ipv=instr(ligne,";")
 if ipv=0 then 
   msgbox "ligne question ioncalide"
   wscript.quit
 end if
dim iq
  for iq=1 to nbq
    if mid(ligne,1,ipv-1) = list(iq) then
      ofq.writeline ligne
      nbqin=nbqin+1
      vu(iq)=true
      used(ivar)=true
    '  msgbox "ivar:" & ivar 
    end if
  next

wend ' eof

dim manque
manque=""
if nbq<>nbqin then
for iq=1 to nbq
 if not vu(iq) then manque=manque & " " & list(iq)
next
   msgbox "manque ligne dans .questions.csv: "&nbq & "<>"&nbqin & vbcrlf & manque
   
   wscript.quit
  end if

'msgbox "nbqin="&nbqin
dim entete
entete=ucase(oFcsvin.readline)
'msgbox entete
dim icol
icol=0
dim ipos
ipos=0

dim ipv
ipv=1

dim fini  ' on a traité le dernier de la ligne
fini=false

while not fini
dim idep
idep=ipv+1
'msgbox "avancer a oartir de " & idep & vbcrlf & entete
ipv=instr(idep,entete,";")
if ipv=0 then 
   ipv=len(entete)
   fini=true
end if
'msgbox ipv

dim unecol
unecol=mid(entete,idep,ipv-idep)

' chercher su unecol dans list
dim ivu
dim OK
ok=false
for ivu=1 to nbq
  if list(ivu)=unecol then 
    found(ivu)=true
    ofo.write unecol 
    if ivu<nbq then 
      ofo.write ";"
    else
      ofo.writeline
    end if
ok=true
end if
next

wend

' chercher si incohérence
for ivu=1 to nbq
  if not found(ivu) then 
    msgbox "erreur " & list(ivu)
   wscript.quit
  end if
next

' msgbox "fini en tete cvs"
' traitement une ligne de .csv et .labels.csv

dim nbi
nbi=0
while not ofcsvin.atendofstream
  ligne=ofcsvin.readline
  nbi=nbi+1
 ' msgbox "ligne .csv" & vbcrlf & ligne 
fini=false
ipv=0
icol=0
dim idone
idone=0

' ofo.write nbi & ";"
while not fini

idep=ipv+1
'msgbox "avancer a partir de " & idep & vbcrlf & entete
ipv=instr(idep,ligne,";")
if ipv=0 then 
 '   msgbox "on utilise la derniere colonne de la ligne"
   ipv=len(entete)=1
   fini=true
end if
icol=icol+1
if used(icol) or (icol=1) then 
if used(icol) then idone=idone+1
 'msgbox nbi & " " & idone & " trouve col:"&icol&" lu:"&mid(ligne,idep,ipv-idep)
 ofo.write mid(ligne,idep,ipv-idep)
 if idone<nbq then 
   ofo.write ";"
   else
   ofo.writeline
   fini=true
'msgbox "ecrit fin ligne"
'wscript.quit
  end if
end if
wend


'msgbox "ecrit une ligne " & nbi
'if nbi>2 then wscript.quit
  ligne=oflin.readline
'  msgbox ligne
fini=false
ipv=0
icol=0

while not fini

idep=ipv+1
'msgbox "avancer a oartir de " & idep & vbcrlf & entete
ipv=instr(idep,ligne,";")
if ipv=0 then 
   ipv=len(entete)
   fini=true
end if
icol=icol+1
if used(icol) then 
 ofl.write mid(ligne,idep,ipv-idep)
 if icol<nbq then 
   ofl.write ";"
   else
   ofl.writeline
  end if
end if


wend


wend


msgbox "terminé, création de :" & vbcrlf & _
   sfo & vbcrlf & sfl & vbcrlf & sfq & vbcrlf & "nbi=" & nbi & vbcrlf &  _
   "sélection: " &out
