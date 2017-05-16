@echo off
color b1
Title Knn_in_Lisp
del /q /f KNN.fas
clisp -c KNN.lisp
echo.
echo Fim da compilacao!
pause
cls
echo Defina os seguintes parametro:
echo :maxsim :maxmem :core :clock :ram :display :pixel :K
echo .
echo .
echo Antes de fechar, salve o arquivo!
start /wait user.txt
cls
KNN.fas > out.txt
echo Resultados:
out.txt
