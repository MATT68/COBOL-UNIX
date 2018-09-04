      *****************************************************************

      *                                                               *

      * PROGRAMA DE LECTURA DEL FICHERO DE EMPLEADOS                  *

      *  QUE ESCRIBE EN LOS FICHEROS SALIDA1 Y SALIDA2                *

      *  EL FICHERO SALIDA1 CONTIENE LOS EMPLEADOS CON APELLIDO < M   *

      *  Y EL FICHERO SALIDA2 CONTIENE EL RESTO DE EMPLEADOS.         *

      *                                                               *

      *****************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID.    COBOL300.

      *

      *****************************************************************

      * ENVIROMENT DIVISION                                           *

      *****************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

      *

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT EMPLE

           ASSIGN TO

           '/home/forma2/cobol/ficheros/fichero.empleado'

           FILE STATUS IS WS-FILE-STATUS.

      *

           SELECT SALIDA1

           ASSIGN TO

           '/home/forma2/cobol/ficheros/fichero.salida1'

           FILE STATUS IS WS-FILE-STATUS.

      *

           SELECT SALIDA2

           ASSIGN TO

           '/home/forma2/cobol/ficheros/fichero.salida2'

           FILE STATUS IS WS-FILE-STATUS.



      *****************************************************************

      * DATA DIVISION                                                 *

      *****************************************************************

       DATA DIVISION.

      *

       FILE SECTION.

       FD  EMPLE.

       01  REG-EMPLE                         PIC X(61).

      *

       FD  SALIDA1

           BLOCK CONTAINS 50 RECORDS

           RECORD CONTAINS 63 CHARACTERS

           DATA RECORD IS REG-SALIDA1.

       01  REG-SALIDA1                       PIC X(63).

      *

       FD  SALIDA2.

       01  REG-SALIDA2                       PIC X(57).

      *

      *****************************************************************

      * WORKING STORAGE SECTION                                       *

      *****************************************************************

       WORKING-STORAGE SECTION.

      ***************************************************************** *

      **              DEFINICION VARIABLES                            * *

      ***************************************************************** *

       01  WS-VARIABLES.

           05  WS-CONTADOR2                 PIC ZZZ,ZZ9.

           05  WS-FILE-STATUS               PIC XX.

           05  WS-FICHERO-ERR               PIC X(8).

           05  WS-PARRAFO-ERR               PIC X(18).

           05  WS-OPERACION-ERR             PIC X(8).

           05  WS-FECHA.

               10  WS-ANYO                  PIC X(4).

               10  WS-MES                   PIC X(2).

               10  WS-DIA                   PIC X(2).





      ***************************************************************** *

      **            WC-CONTADORES                                     * *

      ***************************************************************** *

       01  WC-CONTADORES.

           05  WC-CONTADOR                  PIC S9(6) COMP VALUE 0.

           05  WC-CONTADOR-S1               PIC S9(6) COMP VALUE 0.

           05  WC-CONTADOR-S2               PIC S9(6) COMP VALUE 0.

           05  WC-CONTADOR-D                PIC S9(6) COMP VALUE 0.

           05  WC-CONTADOR-O                PIC S9(6) COMP VALUE 0.

      ***************************************************************** *

      **              CONSTANTES Y LITERALES                          * *

      ***************************************************************** *

       01  CT-CONSTANTES.

           05  CT-DIEZ                      PIC 99     VALUE 10.

           05  LT-OPEN                      PIC X(8)   VALUE 'OPEN'.

           05  LT-CLOSE                     PIC X(8)   VALUE 'CLOSE'.

           05  LT-READ                      PIC X(8)   VALUE 'READ'.

           05  LT-WRITE                     PIC X(8)   VALUE 'WRITE'.

           05  LT-PROGRAMA                  PIC X(8)   VALUE 'COBOL300'.

           05  LT-EMPLE                     PIC X(8)   VALUE 'EMPLE'.

           05  LT-SALIDA1                   PIC X(8)   VALUE 'SALIDA1'.

           05  LT-SALIDA2                   PIC X(8)   VALUE 'SALIDA2'.

      ***************************************************************** *

      **              VARIABLES  FICHERO ENTRADA                      * *

      *****************************************************************

      *

       COPY COPYEMPLE.

      ***************************************************************** *

      **              VARIABLES  FICHERO SALIDA                       * *

      ***************************************************************** *

       COPY EMPLE_SALIDA1 REPLACING ==:NNN:== BY ==WS-SAL1==.

       COPY EMPLE_SALIDA2 REPLACING ==:NNN:== BY ==WS-SAL2==.

      ***************************************************************** *

      **              SWITCHES                                        * *

      ***************************************************************** *

       01  SW-SWITCHES.

           05  SW-FIN-FICHERO                PIC 9.

               88 FIN-FICHERO                           VALUE 1.

               88 NO-FIN-FICHERO                        VALUE 0.

      *

           05  SW-ERROR                      PIC 9      VALUE 0.

               88 SI-ERROR                              VALUE 1.

               88 NO-ERROR                              VALUE 0.

      *

      ***************************************************************** *

      **              PROCEDURE  DIVISION.                            * *

      ***************************************************************** *

       PROCEDURE DIVISION.

      *

           PERFORM 1000-INICIO

              THRU 1000-INICIO-EXIT.

      *

           PERFORM 3000-PROCESO

              THRU 3000-PROCESO-EXIT

            UNTIL  FIN-FICHERO.

      *

           PERFORM 8000-FIN

              THRU 8000-FIN-EXIT.

      *

      ***************************************************************** *

      **              INICIO                                          * *

      ***************************************************************** *

       1000-INICIO.

           INITIALIZE  WS-VARIABLES

                       WC-CONTADORES

                       WS-SAL1-REG-EMPLEADO

                       WS-SAL2-REG-EMPLEADO

                       WS-REG-EMPLEADO.

      *

           DISPLAY ' ************ INICIO *************** '.

      *   FORMATEAMOS LA FECHA DESDE UN ACCEPT

      *   QUE LUEGO COLOCAREMOS EN LOS FICHEROS SALIDA1 Y 2

           ACCEPT WS-FECHA  FROM DATE YYYYMMDD.

      *

           OPEN INPUT EMPLE.

      *

           IF  WS-FILE-STATUS = '00'

                CONTINUE

           ELSE

                MOVE  LT-EMPLE        TO WS-FICHERO-ERR

                MOVE  '1000-'         TO WS-PARRAFO-ERR

                MOVE  LT-OPEN         TO WS-OPERACION-ERR

                PERFORM 9100-GESTION-ERRORES

                   THRU 9100-GESTION-ERRORES-EXIT

           END-IF.

      *

           OPEN OUTPUT SALIDA1.

      *

           IF  WS-FILE-STATUS = '00'

                CONTINUE

           ELSE

                MOVE  LT-SALIDA1      TO WS-FICHERO-ERR

                MOVE  '1000-'         TO WS-PARRAFO-ERR

                MOVE  LT-OPEN         TO WS-OPERACION-ERR

                PERFORM 9100-GESTION-ERRORES

                   THRU 9100-GESTION-ERRORES-EXIT

           END-IF.

      *

           OPEN OUTPUT SALIDA2.

      *

           IF  WS-FILE-STATUS = '00'

                CONTINUE

           ELSE

                MOVE  LT-SALIDA2      TO WS-FICHERO-ERR

                MOVE  '1000-'         TO WS-PARRAFO-ERR

                MOVE  LT-OPEN         TO WS-OPERACION-ERR

                PERFORM 9100-GESTION-ERRORES

                   THRU 9100-GESTION-ERRORES-EXIT

           END-IF.

      *

           PERFORM 9200-LEER-FICHERO

              THRU 9200-LEER-FICHERO-EXIT.

      *

       1000-INICIO-EXIT.

           EXIT.

      *****************************************************

      * PROCESO                                           *

      *****************************************************

       3000-PROCESO.

      *

      *     DISPLAY ' ************ PROCESO *************** '.

      *     DISPLAY 'COD.EMPLE : ' WS-EMPLE-CODIGO

      *             ' NOMBRE : '   WS-EMPLE-NOMBRE.

           EVALUATE TRUE

              WHEN WS-EMPLE-APELLIDO(1:1) < 'M'

                               PERFORM 3100-ESCRIBIR-SALIDA1

                                  THRU 3100-ESCRIBIR-SALIDA1-EXIT

              WHEN OTHER

                               PERFORM 3200-ESCRIBIR-SALIDA2

                                  THRU 3200-ESCRIBIR-SALIDA2-EXIT

           END-EVALUATE

      *

           PERFORM 9200-LEER-FICHERO

              THRU 9200-LEER-FICHERO-EXIT.

      *

       3000-PROCESO-EXIT.

           EXIT.

      ***************************************************************** *

      **       ESCRITURA DEL FICHERO SALIDA1                          * *

      ***************************************************************** *

       3100-ESCRIBIR-SALIDA1.

      *

           INITIALIZE  WS-SAL1-REG-EMPLEADO

      *

           MOVE  WS-EMPLE-NOMBRE       TO  WS-SAL1-NOMBRE

           MOVE  WS-EMPLE-INICIAL      TO  WS-SAL1-INICIAL

           MOVE  WS-EMPLE-APELLIDO     TO  WS-SAL1-APELLIDO

           MOVE  WS-EMPLE-SALARIO      TO  WS-SAL1-SALARIO

           MOVE  WS-EMPLE-COMISION     TO  WS-SAL1-COMISION

           MOVE  WS-EMPLE-DEPT         TO  WS-SAL1-CODEPT

      *

           MOVE   WS-FECHA              TO  WS-SAL1-FECHA

      *

           WRITE REG-SALIDA1

              FROM  WS-SAL1-REG-EMPLEADO

           AFTER ADVANCING 1 LINE.

      *

           IF  WS-FILE-STATUS = '00'

               ADD 1 TO WC-CONTADOR-S1

           ELSE

                MOVE  LT-SALIDA1                  TO WS-FICHERO-ERR

                MOVE  '3100-ESCRIBIR-SALIDA1'     TO WS-PARRAFO-ERR

                MOVE  LT-WRITE                    TO WS-OPERACION-ERR

                PERFORM 9100-GESTION-ERRORES

                   THRU 9100-GESTION-ERRORES-EXIT

           END-IF.

      *

       3100-ESCRIBIR-SALIDA1-EXIT.

           EXIT.

      ***************************************************************** *

      **       ESCRITURA DEL FICHERO SALIDA2                          * *

      ***************************************************************** *

       3200-ESCRIBIR-SALIDA2.

      *

           INITIALIZE  WS-SAL2-REG-EMPLEADO

      *

           MOVE  WS-EMPLE-CODIGO       TO  WS-SAL2-NUEMPL

           MOVE  WS-EMPLE-NOMBRE       TO  WS-SAL2-NOMBRE

           MOVE  WS-EMPLE-APELLIDO     TO  WS-SAL2-APELLIDO

           MOVE  WS-EMPLE-DEPT         TO  WS-SAL2-CODEPT

           MOVE  WS-EMPLE-SALARIO      TO  WS-SAL2-SALARIO

      *

           MOVE   WS-FECHA             TO  WS-SAL2-FECHA

      *

           WRITE REG-SALIDA2

              FROM  WS-SAL2-REG-EMPLEADO

              AFTER ADVANCING 1 LINE.

      *

           IF  WS-FILE-STATUS = '00'

               ADD 1 TO WC-CONTADOR-S2

           ELSE

                MOVE  LT-SALIDA2                  TO WS-FICHERO-ERR

                MOVE  '3200-ESCRIBIR-SALIDA2'     TO WS-PARRAFO-ERR

                MOVE  LT-WRITE                    TO WS-OPERACION-ERR

                PERFORM 9100-GESTION-ERRORES

                   THRU 9100-GESTION-ERRORES-EXIT

           END-IF.

      *

       3200-ESCRIBIR-SALIDA2-EXIT.

           EXIT.

      *

      *****************************************************

      * FIN                                               *

      *****************************************************

       8000-FIN.

      *

           IF  NO-ERROR

               DISPLAY ' ********************************* '

               DISPLAY ' ******************************************* '

               DISPLAY ' ** ESTADISTICAS DEL PROGRAMA : ' LT-PROGRAMA

                                                                ' **'

               MOVE  WC-CONTADOR            TO WS-CONTADOR2

               DISPLAY ' ** FILAS  LEIDAS EMPLEADO  :  ' WS-CONTADOR2

               MOVE  WC-CONTADOR-S1         TO WS-CONTADOR2

               DISPLAY ' ** FILAS ESCRITAS SALIDA1  : ' WS-CONTADOR2

               MOVE  WC-CONTADOR-S2         TO WS-CONTADOR2

               DISPLAY ' ** FILAS ESCRITAS SALIDA2  : ' WS-CONTADOR2

               DISPLAY ' ********************************* '

               DISPLAY ' ********************************* '

      *

               CLOSE EMPLE

      *

               IF  WS-FILE-STATUS = '00'

                    CONTINUE

               ELSE

                    DISPLAY '*************************'

                    DISPLAY '  FALLA  CLOSE EMPLE !! '

                    DISPLAY '*************************'

               END-IF

      *

               CLOSE SALIDA1

      *

               IF  WS-FILE-STATUS = '00'

                    CONTINUE

               ELSE

                    DISPLAY '*************************'

                    DISPLAY '  FALLA CLOSE SALIDA1 !! '

                    DISPLAY '*************************'

               END-IF

      *

               CLOSE SALIDA2

      *

               IF  WS-FILE-STATUS = '00'

                    CONTINUE

               ELSE

                    DISPLAY '*************************'

                    DISPLAY '  FALLA CLOSE SALIDA2 !! '

                    DISPLAY '*************************'

               END-IF



           END-IF.

      *

           STOP RUN.

      *

       8000-FIN-EXIT.

           EXIT.

      *****************************************************

      *  GESTION DE ERRORES                               *

      *****************************************************

       9100-GESTION-ERRORES.

           SET SI-ERROR TO TRUE

           DISPLAY '*************************'

           DISPLAY '*** E  R  R  O  R    ****'

           DISPLAY '*************************'

           DISPLAY '* PARRAFO      : '  WS-PARRAFO-ERR       '   *'

           DISPLAY '* FICHERO      : '  WS-FICHERO-ERR       '   *'

           DISPLAY '* OPERACION    : '  WS-OPERACION-ERR     '   *'

           DISPLAY '* FILE-STATUS  : '  WS-FILE-STATUS   '   *'

           DISPLAY '*************************'

      *

           PERFORM 8000-FIN

              THRU 8000-FIN-EXIT.

      *

       9100-GESTION-ERRORES-EXIT.

           EXIT.

      *

      ***************************************************************** *

      **         LECTURA DEL FICHERO EMPLEADO                       * *

      ***************************************************************** *

       9200-LEER-FICHERO.

      *

           READ EMPLE

                INTO WS-REG-EMPLEADO

                AT END

                     SET FIN-FICHERO TO TRUE

           END-READ.

      *

           IF  WS-FILE-STATUS = '00'

               ADD 1 TO WC-CONTADOR

           ELSE

              IF  WS-FILE-STATUS = '10'

                  IF  WC-CONTADOR = 0

                         DISPLAY '*************************'

                         DISPLAY '** FICHERO VACIO      ***'

                         DISPLAY '*************************'

                  ELSE

                      CONTINUE

                  END-IF

              ELSE

                     MOVE  LT-EMPLE              TO WS-FICHERO-ERR

                     MOVE  '9200-LEER-FICHERO'   TO WS-PARRAFO-ERR

                     MOVE  LT-READ               TO WS-OPERACION-ERR

                     PERFORM 9100-GESTION-ERRORES

                        THRU 9100-GESTION-ERRORES-EXIT

              END-IF

           END-IF.

       9200-LEER-FICHERO-EXIT.

           EXIT.
