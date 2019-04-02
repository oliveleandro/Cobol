       IDENTIFICATION DIVISION.
       PROGRAM-ID. cria-cbl.
       AUTHOR.     "LEANDRO O.C. 20190326".
       ENVIRONMENT DIVISION.
       FILE-CONTROL.

           SELECT ARQCBL ASSIGN TO DISK
               ORGANIZATION    IS LINE SEQUENTIAL
               FILE STATUS     IS W-FSCBL.

       DATA DIVISION.
       FILE SECTION.

           FD  ARQCBL
               VALUE OF FILE-ID IS W-ARQCBL.

           01  REG-CBL.
               05  DADOS-CBL   PIC X(70) VALUE SPACES.

       WORKING-STORAGE SECTION.

       01  W-FILE-STATUS.
           05  W-FSCBL     PIC X(02) VALUE SPACES.

       01  W-ARQS.
           05  W-ARQCBL    PIC X(50) VALUE SPACES.

       01  W-VAR.
           05  W-KK          PIC 9(02) VALUE ZEROS.
           05  W-ARQ         PIC 9(03) VALUE ZEROS.
           05  W-ARQ-TMP     PIC 9(03) VALUE ZEROS.
           05  W-SEQUENT     PIC 9(02) VALUE ZEROS.
           05  W-SEQUENT-TMP PIC 9(02) VALUE ZEROS.
           05  W-BOK         PIC 9(02) VALUE ZEROS.
           05  W-BOK-TMP     PIC 9(02) VALUE ZEROS.
           05  W-NUM-ARQ     PIC 9(03) OCCURS 999 TIMES.
           05  W-NOME-SEQ    PIC X(10) OCCURS 99 TIMES.
           05  W-NOME-BOK    PIC X(10) OCCURS 999 TIMES.
           05  W-NOME-CBL    PIC X(10) VALUE SPACES.

       PROCEDURE DIVISION.
       100-INICIO.

           DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***"  AT 0203
           DISPLAY "QUANTOS ARQUIVOS A ROTINA VAI LER?.:" AT 0403
           ACCEPT W-ARQ
           PERFORM 200-PEGA-ARQ W-ARQ TIMES

           DISPLAY ERASE
           DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***" AT 0203
           DISPLAY "QUANTOS ARQS SEQUENTIAL A ROTINA VAI TER?.:" AT 0403
           ACCEPT W-SEQUENT
           PERFORM 205-CRIA-SEQ W-SEQUENT TIMES

           DISPLAY ERASE
           DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***" AT 0203
           DISPLAY "QUANTOS BOK'S A ROTINA VAI LER?.:" AT 0403
           ACCEPT W-BOK
           PERFORM 210-PEGA-BOK W-BOK TIMES

      *>ESCREVE FONTE COBOL
           DISPLAY ERASE
           DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***" AT 0203
           DISPLAY "QUAL O NOME DO SEU FONTE, EX. PLA001?.: " AT 0403
           ACCEPT W-NOME-CBL

           IF W-NOME-CBL = SPACES
               DISPLAY ERASE
               DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***" AT 0203
               DISPLAY "NOME OBRIGATORIO!" AT 0503
               STOP RUN
           ELSE
               STRING W-NOME-CBL ".cbl" DELIMITED BY SPACES
                   INTO W-ARQCBL
               END-STRING
               OPEN OUTPUT ARQCBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       IDENTIFICATION DIVISION." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               STRING "       PROGRAM-ID. " DELIMITED BY SIZE
                   W-NOME-CBL "." DELIMITED BY SPACES INTO DADOS-CBL
               END-STRING
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       AUTHOR.     LEANDRO O.C." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       ENVIRONMENT DIVISION." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       FILE-CONTROL." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               IF W-ARQ NOT = ZEROS
                   MOVE W-ARQ TO W-ARQ-TMP
                   PERFORM UNTIL EXIT
                       MOVE SPACES TO DADOS-CBL
                       STRING '       COPY "sl' DELIMITED BY SIZE
                           W-NUM-ARQ(W-ARQ) '.bok".'
                           DELIMITED BY SPACES INTO DADOS-CBL
                       END-STRING
                       WRITE REG-CBL
                       SUBTRACT 1 FROM W-ARQ
                       IF W-ARQ = ZEROS EXIT PERFORM END-IF
                   END-PERFORM
                   MOVE W-ARQ-TMP TO W-ARQ
               END-IF

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               IF W-SEQUENT NOT = ZEROS
                   MOVE W-SEQUENT TO W-SEQUENT-TMP
                   PERFORM UNTIL EXIT
                       MOVE SPACES TO DADOS-CBL
                       STRING '       SELECT ' DELIMITED BY SIZE
                           W-NOME-SEQ(W-SEQUENT) DELIMITED BY SPACES
                           ' ASSIGN TO DISK' DELIMITED BY SIZE
                           INTO DADOS-CBL
                       END-STRING
                       WRITE REG-CBL

                       MOVE SPACES TO DADOS-CBL
                       MOVE "          ORGANIZATION IS LINE SEQUENTIAL"
                           TO DADOS-CBL
                       WRITE REG-CBL

                       MOVE SPACES TO DADOS-CBL
                       STRING "          FILE STATUS     IS "
                          DELIMITED BY SIZE "W-FS" W-NOME-SEQ(W-SEQUENT)
                           "." DELIMITED BY SPACES INTO DADOS-CBL
                       END-STRING
                       WRITE REG-CBL
                       SUBTRACT 1 FROM W-BOK
                       IF W-BOK = ZEROS EXIT PERFORM END-IF
                   END-PERFORM
                   MOVE W-SEQUENT-TMP TO W-SEQUENT
               END-IF

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       DATA DIVISION." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       FILE SECTION." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               IF W-ARQ NOT = ZEROS
                   MOVE W-ARQ TO W-ARQ-TMP
                   PERFORM UNTIL EXIT
                       MOVE SPACES TO DADOS-CBL
                       STRING '       COPY "fd' DELIMITED BY SIZE
                           W-NUM-ARQ(W-ARQ) '.bok".'
                           DELIMITED BY SPACES INTO DADOS-CBL
                       END-STRING
                       WRITE REG-CBL
                       SUBTRACT 1 FROM W-ARQ
                       IF W-ARQ = ZEROS EXIT PERFORM END-IF
                   END-PERFORM
                   MOVE W-ARQ-TMP TO W-ARQ
               END-IF

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               IF W-SEQUENT NOT = ZEROS
                   MOVE W-SEQUENT TO W-SEQUENT-TMP
                   PERFORM UNTIL EXIT
                       MOVE SPACES TO DADOS-CBL
                       STRING '       FD ' DELIMITED BY SIZE
                           W-NOME-SEQ(W-SEQUENT) DELIMITED BY SPACES
                           ' VALUE OF FILE-ID IS W-' DELIMITED BY SIZE
                           W-NOME-SEQ(W-SEQUENT) "." DELIMITED BY SPACES
                           INTO DADOS-CBL
                       END-STRING
                       WRITE REG-CBL

                       MOVE SPACES TO DADOS-CBL
                       STRING "          01 REG-" DELIMITED BY SIZE
                           W-NOME-SEQ(W-SEQUENT) DELIMITED BY SPACES "."
                           INTO DADOS-CBL
                       END-STRING
                       WRITE REG-CBL

                       MOVE SPACES TO DADOS-CBL
                       STRING "            05 W-" W-NOME-SEQ(W-SEQUENT)
                           " PIC X(50) VALUE SPACES." INTO DADOS-CBL
                       END-STRING
                       WRITE REG-CBL
                       SUBTRACT 1 FROM W-SEQUENT
                       IF W-SEQUENT = ZEROS EXIT PERFORM END-IF
                   END-PERFORM
                   MOVE W-SEQUENT-TMP TO W-SEQUENT
               END-IF

               MOVE SPACES TO DADOS-CBL
               MOVE "       WORKING-STORAGE SECTION." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       77 W-FS PIC X(02) VALUE SPACES."
                   TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "       PROCEDURE DIVISION." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "      *>--ESCREVA SEU FONTE..." TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "           EXIT PROGRAM" TO DADOS-CBL
               WRITE REG-CBL

               MOVE SPACES TO DADOS-CBL
               MOVE "           STOP RUN." TO DADOS-CBL
               WRITE REG-CBL

           END-IF.

           STOP RUN.

      *>--PEGA OS ARQUIVOS UTILIZADOS NO SIL
       200-PEGA-ARQ SECTION.

           DISPLAY ERASE
           DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***" AT 0203

           DISPLAY "NUMERO DO ARQ.:" AT 0403 ACCEPT W-NUM-ARQ(W-ARQ).
       200-FIM.

      *>--CRIA OS ARQUIVOS UTILIZADOS NO SIL
       205-CRIA-SEQ SECTION.

           DISPLAY ERASE
           DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***" AT 0203

           DISPLAY "NOME DO ARQ SEQUENTIAL:" AT 0403
               ACCEPT W-NOME-SEQ(W-SEQUENT).
       205-FIM.

      *>--PEGA OS BOK'S UTILIZADOS NO SIL
       210-PEGA-BOK SECTION.

           DISPLAY ERASE
           DISPLAY "*** ROTINA PARA CRIAR FONTE CBL ***" AT 0203

           DISPLAY "NUMERO/NOME DO BOK.:" AT 0402
               ACCEPT W-NOME-BOK(W-BOK).
       210-FIM.

