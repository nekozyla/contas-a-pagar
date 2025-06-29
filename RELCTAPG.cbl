       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELCTAPG.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CONTAS ASSIGN TO "CONTAPAGAR.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS CP-NUM-DOC
               FILE STATUS IS FS-CONT.

       DATA DIVISION.
       FILE SECTION.
       COPY "arquivos.cpy".

       WORKING-STORAGE SECTION.
       01 FS-CONT        PIC XX.
       01 FIM-PROGRAMA   PIC X VALUE "N".
       01 LINHA-CONTADOR PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARQ-CONTAS
           DISPLAY "====== CONTAS EM ABERTO ======"
           PERFORM ATÉ-FIM
               READ ARQ-CONTAS NEXT RECORD
                   AT END
                       MOVE "S" TO FIM-PROGRAMA
                   NOT AT END
                       IF CP-SITUACAO = "A"
                           ADD 1 TO LINHA-CONTADOR
                           DISPLAY "-----------------------------------"
                           DISPLAY "Documento     : " CP-NUM-DOC
                           DISPLAY "CNPJ Fornecedor: " CP-CNPJ-FORN
                           DISPLAY "Emissão       : " CP-DATA-EMISSAO
                           DISPLAY "Vencimento    : " CP-DATA-VENC
                           DISPLAY "Valor         : " CP-VALOR
                       END-IF
               END-READ
           END-PERFORM

           IF LINHA-CONTADOR = 0
               DISPLAY "Nenhuma conta em aberto encontrada."
           END-IF

           CLOSE ARQ-CONTAS
           STOP RUN.
