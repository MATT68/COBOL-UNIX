#!/bin/bash
# Script de Compilacion para programas COBOL con acceso a DB2
# El programa requiere como parámetro el nombre del programa
if [ $# -eq 0  ]
then
   echo ' Este script compila un programa COBOL con DB2 '
   echo ' Requiere dos parámetros: el nombre de programa sin su extensión y la base de datos. '
   exit
fi

prog_name=$1
database=$2

echo ' Conexión a Base de datos ' 
db2 connect to ${database}

echo ' Precompilación ' 
db2 prep $HOME/cobol/fuentes/${prog_name}.sqb bindfile target ANSI_COBOL

echo ' Compilación ' 
cobc -static $HOME/cobol/fuentes/${prog_name}.cbl -L/home/db2inst1/sqllib/lib64 -ldb2 -x -O -I /home/forma2/cobol/copys                                           -L/$HOME/cobol/rutinas -o $HOME/cobol/bin/${prog_name}

echo ' Bind contra DB2 ' 
db2 bind $HOME/cobol/fuentes/${prog_name}.bnd
db2 connect reset

if [ -f $HOME/cobol/bin/${prog_name}  ] 
 then
   echo '********* Proceso completo  **********' 
else
   echo '********* No ha sido posible concluir la compilacion  **********'
fi
echo ' Compilado  ' 
ls -l $HOME/cobol/bin/${prog_name}  
