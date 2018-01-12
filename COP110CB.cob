      ******************************************************************
      ******************************************************************
      **            COPY DE COMUNICAION PARA LA RUTINA                **
      **                   RUT110CB.COB                               **
      ******************************************************************
      *
      *
       01  CP-RUT110CB.
           05  CP110-RETURN-CODE               PIC X(2).
           05  CP110-INPUT.
               10  CP110I-TIPO                 PIC X.
               10  CP110I-EMPNO                PIC X(6).
               10  CP110I-SALARY               PIC S9(7)V9(2) COMP-3.
           05  CP110-OUTPUT.
               10  CP110O-FIRSTNME             PIC X(12).
               10  CP110O-LASTNAME             PIC X(15).
               10  CP110O-SALARY               PIC S9(7)V9(2) COMP-3.
           05  CP110-ERROR.
               10  CP110E-MESSAGE              PIC X(40).
