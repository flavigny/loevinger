' flavigny@free.fr, 20171214, 20220523

' conserver fichiers 
keep=true
Set args = Wscript.Arguments
if args.count<>1 then 
 msgbox "Conversion Spss .sav en .csv"&vbcrlf&"draguer le fichier .sav sur mon icone"
  wscript.quit
end if
improbable="!$#~[workfilespss2csv]~#$!"
nom=args(0)
nomu=nom  ' servira pour extraire chemin
nn=""
' confirmer les \ pour R
for i=1 to len(nom)
if mid(nom,i,1)="\" then nn=nn&"\"
nn=nn&mid(nom,i,1)
next 'i
nom=nn
' supprimer les " que windows aurait mises autour d'un nom avec espace ou autre 
if mid(nom,1,1)="""" and mid(nom,len(nom),1)=""""  then
 nom=mid(nom,2,len(nom)-2)
end  if

if ucase(mid(nom,len(nom)-3)) <> ".SAV" then
 msgbox ".sav obligatoire" & vbcrlf &ucase(mid(nom,len(nom)-3))
 wscript.quit
end if
racine=mid(nom,1,len(nom)-4)
out= racine & ".csv"
lab=racine & ".labels.csv"
lib=racine & ".questions.csv"

dim fso
set fso = CreateObject("Scripting.FileSystemObject")
CurrentDir =  mid(nom,1,instrrev(nom,"\"))
CurrentDiru = mid(nomu,1,instrrev(nomu,"\"))
batname=   racine & ".r.bat"
rname= racine & ".r"

set fo=fso.createtextfile(batname)
fo.writeline "@echo off"
fo.writeline "@echo "& now
fo.writeline "echo prepare par sav2csv.vbs pour passage de spss a echelle de Loevinger"
fo.writeline "cd """&currentdiru &""""
fo.writeline "set rpath=""C:\Program Files\R\R-4.0.2\bin\i386\RSCRIPT.exe"""
fo.writeline "if exist %rpath% goto lestla"
'fo.writeline "set rpath=""C:\Program Files(x86)\R\R-3.1.0\bin\i386\RSCRIPT.exe"""
'fo.writeline "if exist %rpath% goto lestla"
fo.writeline "echo: GAFFE: pas trouvé %rpath%, on tente usage par le path systeme"
fo.writeline "set rpath=rscript.exe"
fo.writeline ":lestla"
'fo.writeline "set scratchr="""&currentdiru & improbable & ".r"""
fo.writeline "set scratchr="""&racine & ".r"""
fo.writeline "echo ------ script pour R"
fo.writeline "type %scratchr%"
fo.writeline "echo ------ appel de R"
fo.writeline "%rpath% %scratchr% > "& racine&".r.txt"
fo.writeline "echo Vous pouvez consulter la log de R dans le fichier "&racine&".r.txt"
'fo.writeline "echo ------ fin de R"
fo.writeline "pause"
fo.writeline "exit"
fo.close
set fo=fso.createtextfile(rname)
fo.writeline "# "& now
fo.writeline "# prepare par sav2csv.vbs pour passage de spss à echelle de Loevinger"

fo.writeline  "library(foreign)"
fo.writeline  "values = read.spss("""&nom&""", to.data.frame=TRUE , use.value.labels = FALSE)"

'fo.writeline  " Freq(values$ECVA1) "
'fo.writeline  " values$dizaine <- 0 "
'fo.writeline  " values$pof <- values$ECVA1 - 10*values$dix  }"
fo.writeline  "write.csv2(values,file="""&out&""" ,quote = FALSE, na = "" "" )"
fo.writeline  "labels = read.spss("""&nom&""", to.data.frame=TRUE , use.value.labels = TRUE)"
fo.writeline  "write.csv2(labels,file="""&lab&""" ,quote = FALSE, na = "" "" )"
fo.writeline  "po.labels<- as.data.frame(attr(labels, ""variable.labels""))"
fo.writeline  "write.csv2(po.labels,file="""&lib&""" ,quote = FALSE, na = "" "" )"
fo.writeline  "quit()"
fo.close 
'msgbox batname & " " & rname
Dim oShell
Set oShell = CreateObject("WScript.Shell")
rep=oShell.Run("cmd /k """& batname &"""" ,,true)
' msgbox "done"
' faudrait effacer .r et .bat
' commenter les deux ligne suivantes pour autopsier l'exécution si abend
if not keep then fso.deletefile batname
if not keep then fso.deletefile rname
