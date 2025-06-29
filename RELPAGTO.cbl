       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELPAGTO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-HISTPAGTO ASSIGN TO "HISTPAGTO.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-HIST.

       DATA DIVISION.
       FILE SECTION.
       COPY "arquivos.cpy".

       WORKING-STORAGE SECTION.
       01 FS-HIST           PIC XX.
       01 DATA-INICIO       PIC 9(8).
       01 DATA-FIM          PIC 9(8).
       01 FIM-PROGRAMA      PIC X VALUE "N".
       01 CONTADOR-REGS     PIC 9(4) VALUE ZERO.

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARQ-HISTPAGTO

           DISPLAY "====== RELATÓRIO DE PAGAMENTOS ======"
           DISPLAY "Digite a data inicial (AAAAMMDD):"
           ACCEPT DATA-INICIO
           DISPLAY "Digite a data final   (AAAAMMDD):"
           ACCEPT DATA-FIM

           PERFORM UNTIL FIM-PROGRAMA = "S"
               READ ARQ-HISTPAGTO NEXT RECORD
                   AT END
                       MOVE "S" TO FIM-PROGRAMA
                   NOT AT END
                       IF H-DATA-PGTO >= DATA-INICIO AND
                          H-DATA-PGTO <= DATA-FIM
                           ADD 1 TO CONTADOR-REGS
                           DISPLAY "-----------------------------------"
                           DISPLAY "Documento     : " H-NUM-DOC
                           DISPLAY "CNPJ Fornecedor: " H-CNPJ-FORN
                           DISPLAY "Data Pagamento: " H-DATA-PGTO
                           DISPLAY "Valor Pago    : " H-VALOR-PAGO
                       END-IF
               END-READ
           END-PERFORM

           IF CONTADOR-REGS = 0
               DISPLAY "Nenhum pagamento encontrado nesse período."
           END-IF

           CLOSE ARQ-HISTPAGTO
           STOP RUN.
