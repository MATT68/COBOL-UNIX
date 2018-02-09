      ***************************************************************** 00010020
      *                                                               * 00020020
      * PROGRAMA DE FULANITO                                          * 00030020
      *                                                               * 00040020
      * ESTE PROGRAMA GENERA UN FICHERO DE SALIDA DE EMPLEA2          * 00050021
      ***************************************************************** 00090020
       IDENTIFICATION DIVISION.                                         00110020
       PROGRAM-ID.  PROG05CB.                                           00130020

      ***************************************************************** 00150020
      * ENVIROMENT DIVISION                                           * 00160020
      ***************************************************************** 00150020
       ENVIRONMENT DIVISION.                                            00170020
       CONFIGURATION SECTION.                                           00190020
                                                                        00220020
       INPUT-OUTPUT SECTION.                                            00230020
       FILE-CONTROL.                                                    00240020
           SELECT EMPLE
              ASSIGN EMPLEADO
              FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT EMPLE2
              ASSIGN EMPLEA2
              FILE STATUS IS WS-FILE-STATUS.
      *
      ***************************************************************** 00250020
      * DATA DIVISION                                                 * 00260020
      ***************************************************************** 00250020
       DATA DIVISION.                                                   00270020
                                                                        00280020
       FILE SECTION.                                                    00290020
       FD  EMPLE
           RECORD CONTAINS 59 CHARACTERS
           DATA RECORD IS REG-EMPLE.
       01  REG-EMPLE                              PIC X(59).
       FD  EMPLE2
           RECORD CONTAINS 47 CHARACTERS
           DATA RECORD IS REG-EMPLE.
       01  REG-EMPLE                              PIC X(47).
      ***************************************************************** 00250020
      * WORKING STORAGE SECTION                                       * 00260020
      ***************************************************************** 00250020
       WORKING-STORAGE SECTION.                                         00310020
      *
       01  WS-VARIABLES.
           05  WS-CLIENTE.
               10  WS-NOMBRE                     PIC X(20).
               10  WS-APELLIDO1                  PIC X(20).
               10  WS-APELLIDO2                  PIC X(20).
           05  WS-FILE-STATUS                    PIC 99.
           05  WS-GASTO-MES                      PIC S9(6)V99.
           05  WS-GASTO-MES-C                    PIC S9(6)V9(2) COMP-3.
           05  WS-PARRAFO-ERR                    PIC X(20).
           05  WS-FICHERO-ERR                    PIC X(20).
           05  WS-OPERAC-ERR                     PIC X(20).
           05  WS-APELLIDO-VALIDAR.
               10  WS-APELLIDO-PRIMERA           PIC X.
               10  WS-APELLIDO-RESTO             PIC X(14).
       01  WC-CONTADORES.
           05  WC-LINEAS                         PIC 9(6).
           05  WC-LINEAS-EDIT                    PIC ZZZZ9.
       01  LT-LITERALES.                                                00441122
           05 LT-PROGRAMA                  PIC X(8) VALUE 'PROG03CB'.
           05 LT-FI-EMPLEADO               PIC X(8) VALUE 'EMPLEADO'.
           05 LT-FI-EMPLEA2                PIC X(8) VALUE 'EMPLEA2'.
           05 LT-OPEN                      PIC X(8) VALUE 'ABRIR   '.
           05 LT-READ                      PIC X(8) VALUE 'LECTURA '.
           05 CT-UNO                       PIC 9    VALUE 1.
      ******************************************************************00310200
      **              VARIABLES  FICHERO ENTRADA                      **00310300
      ******************************************************************00310400
       01  WS-EMPLEADO-REG.
           05  WS-NUEMPL-EMP                 PIC X(6).
           05  WS-NOMBRE-EMP                 PIC X(12).
           05  WS-INICIAL-EMP                PIC X(1).
           05  WS-APELLIDO-EMP               PIC X(15).
           05  WS-DEPT-EMP                   PIC X(3).
           05  WS-SALARIO-EMP                PIC 9(9)V99.
           05  WS-COMISION-EMP               PIC 9(9)V99.
      *
      ******************************************************************00310200
      **              VARIABLES  FICHERO SALIDA                       **00310300
      ******************************************************************00310400
       01  WS-EMPLEADO-REG2.
           05  WS-NUEMPL-EMP2                PIC X(6).
           05  WS-NOMBRE-EMP2                PIC X(12).
           05  WS-APELLIDO-EMP2              PIC X(15).
           05  WS-DEPT-EMP2                  PIC X(3).
           05  WS-SALARIO-EMP2               PIC 9(9)V99.
      *
      *
      ******************************************************************00440422
      **              SWITCHES                                        **00440522
      ******************************************************************00440622
       01  SW-SWITCHES.                                                 00441122
           05 SW-FIN-FICHERO                    PIC X.
              88 FIN-FICHERO                       VALUE 'Y'.
              88 NO-FIN-FICHERO                    VALUE 'N'.
      *                                                                 00441922
      ******************************************************************00441922
      **              VARIABLES AUXILIARES                            **00442022
      ******************************************************************00442122
                                                                        00441122
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
             UNTIL FIN-FICHERO.
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
               MOVE LT-FI-EMPLEADO   TO WS-FICHERO-ERR
               MOVE LT-OPEN          TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
      *
           OPEN OUTPUT EMPLE2
      *
           IF  WS-FILE-STATUS = 00
               CONTINUE
           ELSE
               MOVE '1000-'          TO WS-PARRAFO-ERR
               MOVE LT-FI-EMPLEA2    TO WS-FICHERO-ERR
               MOVE LT-OPEN          TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF.
      *
           PERFORM 9200-LEER-EMPLE
              THRU 9200-LEER-EMPLE-EXIT
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
           ADD  1  TO WC-LINEAS-LEIDAS
      *
           IF  WC-LINEAS <= 30 AND
               ( WS-APELLIDO-EMP(1:1) = 'A' OR
                 WS-APELLIDO-EMP(1:1) = 'E' OR
                 WS-APELLIDO-EMP(1:1) = 'I' OR
                 WS-APELLIDO-EMP(1:1) = 'O' OR
                 WS-APELLIDO-EMP(1:1) = 'U')
      *
                PERFORM 3100-ESCRIBIR
                   THRU 3100-ESCRIBIR-EXIT
           END-IF.
      *
           PERFORM 9200-LEER-EMPLE
              THRU 9200-LEER-EMPLE-EXIT
           .
      *
       3000-PROCESO-EXIT.                                               00910020
           EXIT.                                                        00920020
      *****************************************************             01770520
      *  ESCRIBIMOS EL FICHERO DE SALIDA                  *             01770620
      *****************************************************             01770720
       3100-ESCRIBIR.                                                   00910020
      * INFORMAMOS LAS VARIABLES A ESCRIBIR
           MOVE WS-NUEMPL-EMP    TO WS-NUEMPL-EMP2
           MOVE WS-NOMBRE-EMP    TO WS-NOMBRE-EMP2
           MOVE WS-APELLIDO-EMP  TO WS-APELLIDO-EMP2
           MOVE WS-DEPT-EMP      TO WS-DEPT-EMP
           MOVE WS-SALARIO-EMP   TO WS-SALARIO-EMP
      * ESCRIBIMOS
           WRITE EMPLE2
            FROM WS-EMPLEADO-REG2
      *
           IF  WS-FILE-STATUS = 00
               ADD  1  TO WC-LINEAS-ESCRITAS
           ELSE
               MOVE LT-FI-EMPLEADO2  TO WS-FICHERO-ERR
               MOVE LT-WRITE         TO WS-OPERAC-ERR
               PERFORM 9100-ERROR-FICHERO
                  THRU 9100-ERROR-FICHERO-EXIT
           END-IF
       3100-ESCRIBIR-EXIT.                                              00910020
           EXIT.
      *****************************************************             01770520
      * FIN                                               *             01770620
      *****************************************************             01770720
       8000-FIN.                                                        01770820
      *
           IF  WS-PARRAFO-ERR NOT = SPACES
               CLOSE EMPLE
           END-IF
      *
           DISPLAY ' **** FINALLLLLL ***** ' .
           MOVE   WC-LINEAS    TO WC-LINEAS-EDIT
           DISPLAY ' ******************************************* ' .
           DISPLAY ' *   E S T A D I S T I C A S               * ' .
           DISPLAY ' *   FILAS TRATADAS POR FICHERO:           * ' .
           DISPLAY ' * LEIDAS FICHERO EMPLEADO : ' WC-LINEAS-EDIT ' * '
           DISPLAY ' ******************************************* ' .
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
           DISPLAY ' **********************************'
      *
           PERFORM 8000-FIN                                             00733020
              THRU 8000-FIN-EXIT                                        00734020
           .
       9100-ERROR-FICHERO-EXIT.
           EXIT.                                                        01773200
      *****************************************************             01770520
      *  LEER EMPLEADO                                    *             01770620
      *****************************************************             01770720
       9200-LEER-EMPLE.
           READ EMPLE
           INTO WS-EMPLEADO-REG
           AT END
              SET FIN-FICHERO TO TRUE
           END-READ
      *
           EVALUATE  WS-FILE-STATUS
              WHEN 00
                   CONTINUE
              WHEN +10
                   IF  WC-LINEAS > 0
                       CONTINUE
                   ELSE
                       DISPLAY ' ***** FICHERO VACIO **** '
                       DISPLAY ' ***** FICHERO VACIO **** '
                       DISPLAY ' ***** FICHERO VACIO **** '
                   END-IF
              WHEN OTHER
                   MOVE '9200-'          TO WS-PARRAFO-ERR
                   MOVE LT-FI-EMPLEADO   TO WS-FICHERO-ERR
                   MOVE LT-READ          TO WS-OPERAC-ERR
                   PERFORM 9100-ERROR-FICHERO
                      THRU 9100-ERROR-FICHERO-EXIT
           END-EVALUATE
      *
       .
       9200-LEER-EMPLE-EXIT.
           EXIT.
      *
