      ***************************************************************** 00010020
      *                                                               * 00020020
      * PROGRAMA DE CRUCE DE FICHEROS                                 * 00030020
      *                                                               * 00040020
      * CRUZAMOS DOS FICHEROS DE EMPLEADOS.                           * 00050021
      * COMPARAMOS EL NUM. EMPLEADO, SI COINCIDE ESCRIBIMOS EN F. ERR.* 00050021
      * SI NO COINCIDEN ESCRIBIMOS EN FICHERO CORRECTOS.                00050021
      ***************************************************************** 00090020
       IDENTIFICATION DIVISION.                                         00110020
       PROGRAM-ID.  PROG09CB.                                           00130020

      ***************************************************************** 00150020
      * ENVIROMENT DIVISION                                           * 00160020
      ***************************************************************** 00150020
       ENVIRONMENT DIVISION.                                            00170020
       CONFIGURATION SECTION.                                           00190020
      *
       INPUT-OUTPUT SECTION.                                            00230020
       FILE-CONTROL.                                                    00240020
      *
           SELECT EMPLE
              ASSIGN EMPLE
              FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT CRUCE
              ASSIGN CRUCE
              FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT CORRECTO
              ASSIGN CORRECTO
              FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT ERRONEO
              ASSIGN ERRONEO
              FILE STATUS IS WS-FILE-STATUS.
      *
      ***************************************************************** 00250020
      * DATA DIVISION                                                 * 00260020
      ***************************************************************** 00250020
       DATA DIVISION.                                                   00270020
                                                                        00280020
       FILE SECTION.                                                    00290020
       FD  EMPLE
           DATA RECORD IS REG-EMPLE.
       01  REG-EMPLE                              PIC X(59).
      *
       FD  CRUCE
           DATA RECORD IS REG-CRUCE.
       01  REG-CRUCE                              PIC X(59).
      *
       FD  CORRECTO
           DATA RECORD IS REG-CORRECTO.
       01  REG-CORRECTO                           PIC X(60).
      *
       FD  ERRONEO
           DATA RECORD IS REG-ERRONEO.
       01  REG-ERRONEO                            PIC X(111).
      *
      ***************************************************************** 00250020
      * WORKING STORAGE SECTION                                       * 00260020
      ***************************************************************** 00250020
       WORKING-STORAGE SECTION.                                         00310020
      *
       01  WS-VARIABLES.
           05  WS-FILE-STATUS                    PIC 99.
           05  WS-PARRAFO-ERR                    PIC X(20).
           05  WS-FICHERO-ERR                    PIC X(20).
           05  WS-OPERAC-ERR                     PIC X(20).
       01  WC-CONTADORES.
           05  WC-LINEAS-EMPLE                   PIC 9(6).
           05  WC-LINEAS-CRUCE                   PIC 9(6).
           05  WC-LINEAS-CORRECTO                PIC 9(6).
           05  WC-LINEAS-ERRONEO                 PIC 9(6).
           05  WC-LINEAS-EDIT                    PIC ZZZ,ZZZ,ZZ9.
       01  LT-LITERALES.                                                00441122
           05 LT-PROGRAMA                  PIC X(8) VALUE 'PROG09CB'.
           05 LT-FI-EMPLE                  PIC X(8) VALUE 'EMPLE'.
           05 LT-FI-CRUCE                  PIC X(8) VALUE 'CRUCE'.
           05 LT-FI-CORRECTO               PIC X(8) VALUE 'CORRECTO'.
           05 LT-FI-ERRONEO                PIC X(8) VALUE 'ERRONEO'.
           05 LT-PROC-CRUCE                PIC X    VALUE 'C'.
           05 LT-PROC-EMPLE                PIC X    VALUE 'E'.
           05 LT-OPEN                      PIC X(8) VALUE 'ABRIR   '.
           05 LT-READ                      PIC X(8) VALUE 'LECTURA '.
           05 LT-WRITE                     PIC X(8) VALUE 'ESCRIBIR'.
           05 CT-UNO                       PIC 9    VALUE 1.
      ******************************************************************00310200
      **              VARIABLES  FICHERO ENTRADA                      **00310300
      ******************************************************************00310400
       01  WS-EMPLE-REG.
           05  WS-NUEMPL-EMP                 PIC X(6).
           05  WS-NOMBRE-EMP                 PIC X(12).
           05  WS-INICIAL-EMP                PIC X(1).
           05  WS-APELLIDO-EMP               PIC X(15).
           05  WS-DEPT-EMP                   PIC X(3).
           05  WS-SALARIO-EMP                PIC 9(9)V99.
           05  WS-COMISION-EMP               PIC 9(9)V99.
      *
       01  WS-CRUCE-REG.
           05  WS-NUEMPL-CRU                 PIC X(6).
           05  WS-APELLIDO-CRU               PIC X(13).
           05  WS-NOMBRE-CRU                 PIC X(15).
           05  WS-DEPT-CRU                   PIC X(3).
           05  WS-SALARIO-CRU                PIC 9(9)V99.
           05  WS-COMISION-CRU               PIC 9(9)V99.
      *
      ******************************************************************00310200
      **              VARIABLES  FICHERO SALIDA                       **00310300
      ******************************************************************00310400
       01  WS-CORRECTO-REG.
           05  WS-NUEMPL-CORR                PIC X(6).
           05  WS-NOMBRE-CORR                PIC X(12).
           05  WS-INICIAL-CORR               PIC X(1).
           05  WS-APELLIDO-CORR              PIC X(15).
           05  WS-DEPT-CORR                  PIC X(3).
           05  WS-SALARIO-CORR               PIC 9(9)V99.
           05  WS-COMISION-CORR              PIC 9(9)V99.
           05  WS-PROCEDENCIA-CORR           PIC X(1).
      *
       01  WS-ERRONEO-REG.
           05  WS-NUEMPL-ERR                  PIC X(6).
           05  WS-APELLIDO1-ERR               PIC X(15).
           05  WS-NOMBRE1-ERR                 PIC X(12).
           05  WS-APELLIDO2-ERR               PIC X(13).
           05  WS-NOMBRE2-ERR                 PIC X(15).
           05  WS-DEPT1-ERR                   PIC X(3).
           05  WS-SALARIO1-ERR                PIC 9(9)V99.
           05  WS-COMISION1-ERR               PIC 9(9)V99.
           05  WS-DEPT2-ERR                   PIC X(3).
           05  WS-SALARIO2-ERR                PIC 9(9)V99.
           05  WS-COMISION2-ERR               PIC 9(9)V99.
      *
      ******************************************************************00440422
      **              SWITCHES                                        **00440522
      ******************************************************************00440622
       01  SW-SWITCHES.                                                 00441122
           05 SW-FIN-EMPLE                      PIC X.
              88 FIN-EMPLE                         VALUE 'Y'.
              88 NO-FIN-EMPLE                      VALUE 'N'.
      *                                                                 00441922
           05 SW-FIN-CRUCE                      PIC X.
              88 FIN-CRUCE                         VALUE 'Y'.
              88 NO-FIN-CRUCE                      VALUE 'N'.
      *                                                                 00441922
      ******************************************************************00441922
      **           TABLAS DE MEMORIA / INTERNAS                       **00442022
      ******************************************************************00442122
      ******************************************************************00730200
      **              PROCEDURE  DIVISION.                            **00730300
      ******************************************************************00730400
       PROCEDURE DIVISION.                                              00730500
      *
           PERFORM 1000-INICIO                                          00730600
              THRU 1000-INICIO-EXIT.                                    00730720
      *                                                                 00730820
           PERFORM 3000-PROCESO                                         00730920
              THRU 3000-PROCESO-EXIT                                    00731020
             UNTIL FIN-EMPLE AND FIN-CRUCE.
      *                                                                 00732020
           PERFORM 8000-FIN                                             00733020
              THRU 8000-FIN-EXIT.                                       00734020
      *                                                                 00735020
      ******************************************************************00736020
      **              INICIO                                          **00737020
      ******************************************************************00738020
       1000-INICIO.                                                     00739020
      *
           DISPLAY ' **** INICIO ***** ' .
           INITIALIZE WS-VARIABLES
                      WC-CONTADORES.
      *
           OPEN INPUT  EMPLE.
      *
           IF  WS-FILE-STATUS = 00
               CONTINUE
           ELSE
               MOVE '1000-'          TO WS-PARRAFO-ERR
               MOVE LT-FI-EMPLE      TO WS-FICHERO-ERR
               MOVE LT-OPEN          TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
      *
           OPEN INPUT  CRUCE.
      *
           IF  WS-FILE-STATUS NOT = 00
               MOVE '1000-'          TO WS-PARRAFO-ERR
               MOVE LT-FI-CRUCE      TO WS-FICHERO-ERR
               MOVE LT-OPEN          TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
      *
           OPEN OUTPUT CORRECTO
      *
           IF  WS-FILE-STATUS NOT = 00
               MOVE '1000-'          TO WS-PARRAFO-ERR
               MOVE LT-FI-CORRECTO   TO WS-FICHERO-ERR
               MOVE LT-OPEN          TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
      *
           OPEN OUTPUT ERRONEO
      *
           IF  WS-FILE-STATUS NOT = 00
               MOVE '1000-'          TO WS-PARRAFO-ERR
               MOVE LT-FI-ERRONEO    TO WS-FICHERO-ERR
               MOVE LT-OPEN          TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
      *
           PERFORM 9200-READ-EMPLE
              THRU 9200-READ-EMPLE-EXIT
      *
           PERFORM 9300-READ-CRUCE
              THRU 9300-READ-CRUCE-EXIT
           .
      *
       1000-INICIO-EXIT.                                                00900520
           EXIT.                                                        00900620
                                                                        00900720
      *****************************************************             00900820
      * PROCESO                                           *             00900920
      *****************************************************             00901020
       3000-PROCESO.                                                    00902020
      *
           MOVE '3000-'          TO WS-PARRAFO-ERR
      *
           EVALUATE TRUE
              WHEN WS-NUEMPL-EMP = WS-NUEMPL-CRU
                PERFORM 3100-ESCRIBIR-ERRONEO
                   THRU 3100-ESCRIBIR-ERRONEO-EXIT
                PERFORM 9200-READ-EMPLE
                   THRU 9200-READ-EMPLE-EXIT
                PERFORM 9300-READ-CRUCE
                   THRU 9300-READ-CRUCE-EXIT
              WHEN WS-NUEMPL-EMP > WS-NUEMPL-CRU
      *          INFORMAMOS VARIABLES ESCRITURA
                MOVE WS-NUEMPL-CRU        TO   WS-NUEMPL-CORR
                MOVE WS-NOMBRE-CRU        TO   WS-NOMBRE-CORR
                MOVE SPACE                TO   WS-INICIAL-CORR
                MOVE WS-APELLIDO-CRU      TO   WS-APELLIDO-CORR
                MOVE WS-DEPT-CRU          TO   WS-DEPT-CORR
                MOVE WS-SALARIO-CRU       TO   WS-SALARIO-CORR
                MOVE WS-COMISION-CRU      TO   WS-COMISION-CORR
                MOVE LT-PROC-CRUCE        TO   WS-PROCEDENCIA-CORR
      *          ESCRIBO CRUCE
                PERFORM 3200-ESCRIBIR-CORRECTO
                   THRU 3200-ESCRIBIR-CORRECTO-EXIT
                PERFORM 9300-READ-CRUCE
                   THRU 9300-READ-CRUCE-EXIT
              WHEN WS-NUEMPL-EMP < WS-NUEMPL-CRU
      *          INFORMAMOS VARIABLES ESCRITURA
                MOVE WS-NUEMPL-EMP        TO   WS-NUEMPL-CORR
                MOVE WS-NOMBRE-EMP        TO   WS-NOMBRE-CORR
                MOVE WS-INICIAL-EMP       TO   WS-INICIAL-CORR
                MOVE WS-APELLIDO-EMP      TO   WS-APELLIDO-CORR
                MOVE WS-DEPT-EMP          TO   WS-DEPT-CORR
                MOVE WS-SALARIO-EMP       TO   WS-SALARIO-CORR
                MOVE WS-COMISION-EMP      TO   WS-COMISION-CORR
                MOVE LT-PROC-EMPLE        TO   WS-PROCEDENCIA-CORR
      *          ESCRIBO EMPLE
                PERFORM 3200-ESCRIBIR-CORRECTO
                   THRU 3200-ESCRIBIR-CORRECTO-EXIT
                PERFORM 9200-READ-EMPLE
                   THRU 9200-READ-EMPLE-EXIT
           END-EVALUATE.
      *
       3000-PROCESO-EXIT.                                               00910020
           EXIT.                                                        00920020
      *
      *****************************************************             01770520
      *  ESCRIBIMOS EL FICHERO ERRONEO                    *             01770620
      *****************************************************             01770720
       3100-ESCRIBIR-ERRONEO.                                           00910020
           MOVE '3100-'          TO WS-PARRAFO-ERR
      *
      * INFORMAMOS LAS VARIABLES A ESCRIBIR
           MOVE WS-NUEMPL-EMP          TO WS-NUEMPL-ERR
           MOVE WS-APELLIDO-EMP        TO WS-APELLIDO1-ERR
           MOVE WS-NOMBRE-EMP          TO WS-NOMBRE1-ERR
           MOVE WS-APELLIDO-CRU        TO WS-APELLIDO2-ERR
           MOVE WS-NOMBRE-CRU          TO WS-NOMBRE2-ERR
           MOVE WS-DEPT-EMP            TO WS-DEPT1-ERR
           MOVE WS-SALARIO-EMP         TO WS-SALARIO1-ERR
           MOVE WS-COMISION-EMP        TO WS-COMISION1-ERR
           MOVE WS-DEPT-CRU            TO WS-DEPT2-ERR
           MOVE WS-SALARIO-CRU         TO WS-SALARIO2-ERR
           MOVE WS-COMISION-CRU        TO WS-COMISION2-ERR
      *
      * ESCRIBIMOS
           WRITE REG-ERRONEO
            FROM WS-ERRONEO-REG
      *
           IF  WS-FILE-STATUS = 00
               ADD  1  TO WC-LINEAS-ERRONEO
           ELSE
               MOVE LT-FI-ERRONEO     TO WS-FICHERO-ERR
               MOVE LT-WRITE         TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
       3100-ESCRIBIR-ERRONEO-EXIT.                                      00910020
           EXIT.
      *****************************************************             01770520
      *  ESCRIBIMOS EL FICHERO CORRECTO                   *             01770620
      *****************************************************             01770720
       3200-ESCRIBIR-CORRECTO.                                          00910020
           MOVE '3200-'          TO WS-PARRAFO-ERR
      *
      * ESCRIBIMOS
           WRITE REG-CORRECTO
            FROM WS-CORRECTO-REG
      *
           IF  WS-FILE-STATUS = 00
               ADD  1  TO WC-LINEAS-CORRECTO
           ELSE
               MOVE LT-FI-CORRECTO    TO WS-FICHERO-ERR
               MOVE LT-WRITE          TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
       3200-ESCRIBIR-CORRECTO-EXIT.                                     00910020
           EXIT.
      *****************************************************             01770520
      * FIN                                               *             01770620
      *****************************************************             01770720
       8000-FIN.                                                        01770820
      *
           DISPLAY ' **** FINALLLLLL ***** ' .
           CLOSE EMPLE
                 CRUCE
                 CORRECTO
                 ERRONEO.
      *
           DISPLAY ' *************************************** ' .
           DISPLAY ' *   E S T A D I S T I C A S           * ' .
           DISPLAY ' *   FILAS TRATADAS POR CADA FICHERO:  * ' .
           DISPLAY ' * LEIDAS FICHERO EMPLE    : ' WC-LINEAS-EMPLE ' * '
           DISPLAY ' * LEIDAS FICHERO CRUCE    : ' WC-LINEAS-CRUCE ' * '
           DISPLAY ' * ESCRITAS FICHERO CORRECTO:' WC-LINEAS-CORRECTO
           DISPLAY ' * ESCRITAS FICHERO ERRONEO :' WC-LINEAS-ERRONEO
           ' *'.
           DISPLAY ' ************************************* ' .
      *
           STOP RUN.
      *
       8000-FIN-EXIT.                                                   01773120
           EXIT.                                                        01773200
      *****************************************************             01770520
      *  GESTION DE ERRORES                               *             01770620
      *****************************************************             01770720
       9100-ERROR-FICHERO.
           DISPLAY ' **********************************'
           DISPLAY ' * ERROR PROGRAMA : ' LT-PROGRAMA     '*'
           DISPLAY ' * ERROR PARRAFO  : ' WS-PARRAFO-ERR  '*'
           DISPLAY ' * ERROR FICHERO  : ' WS-FICHERO-ERR  '*'
           DISPLAY ' * TIPO OPERACION : ' WS-OPERAC-ERR    '*'
           DISPLAY ' * FILE STATUS    : ' WS-FILE-STATUS '*'
           DISPLAY ' **********************************'.
      *
           PERFORM 8000-FIN                                             00733020
              THRU 8000-FIN-EXIT.                                       00734020
      *
       9100-ERROR-FICHERO-EXIT.
           EXIT.                                                        01773200
      *****************************************************             01770520
      *  LEER EMPLEADO                                    *             01770620
      *****************************************************             01770720
       9200-READ-EMPLE.
           MOVE '9200-'          TO WS-PARRAFO-ERR
      *
           READ EMPLE
           INTO WS-EMPLE-REG
           AT END
              SET  FIN-EMPLE    TO TRUE
              MOVE HIGH-VALUES  TO WS-NUEMPL-EMP
           END-READ.
      *
           EVALUATE  WS-FILE-STATUS
              WHEN 00
                 ADD  1  TO WC-LINEAS-EMPLE
              WHEN +10
                   IF  WC-LINEAS-EMPLE = 0
                       DISPLAY ' ***** FICHERO VACIO **** '
                       DISPLAY ' ***** FICHERO VACIO **** '
                       DISPLAY ' ***** FICHERO VACIO **** '
                   END-IF
              WHEN OTHER
                   MOVE LT-FI-EMPLE      TO WS-FICHERO-ERR
                   MOVE LT-READ          TO WS-OPERAC-ERR
                   PERFORM 9100-ERROR-FICHERO
                      THRU 9100-ERROR-FICHERO-EXIT
           END-EVALUATE.
      *
       9200-READ-EMPLE-EXIT.
           EXIT.                                                        01773200
      *****************************************************             01770520
      *  LEER CRUCE                                       *             01770620
      *****************************************************             01770720
       9300-READ-CRUCE.
      *
           MOVE '9300-'          TO WS-PARRAFO-ERR
      *
           READ CRUCE
           INTO WS-CRUCE-REG
           AT END
              SET  FIN-CRUCE    TO TRUE
              MOVE HIGH-VALUES  TO WS-NUEMPL-CRU
           END-READ.
      *
           EVALUATE  WS-FILE-STATUS
              WHEN 00
                 ADD  1  TO WC-LINEAS-CRUCE
              WHEN +10
                   IF  WC-LINEAS-CRUCE = 0
                       DISPLAY ' ***** FICHERO VACIO **** '
                       DISPLAY ' ***** FICHERO VACIO **** '
                       DISPLAY ' ***** FICHERO VACIO **** '
                   END-IF
              WHEN OTHER
                   MOVE LT-FI-CRUCE      TO WS-FICHERO-ERR
                   MOVE LT-READ          TO WS-OPERAC-ERR
                   PERFORM 9100-ERROR-FICHERO
                      THRU 9100-ERROR-FICHERO-EXIT
           END-EVALUATE.
      *
       9300-READ-CRUCE-EXIT.
   
