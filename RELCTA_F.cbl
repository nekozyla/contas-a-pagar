>>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELCTA_F.
       AUTHOR. carol.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FORNECEDORES-FILE
               ASSIGN TO 'FORNECEDOR.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS F-CNPJ
               FILE STATUS IS WS-STATUS-FORNECEDORES.

           SELECT CONTAPAGAR-FILE
               ASSIGN TO 'CONTAPAGAR.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CP-PRIMARY-KEY
               FILE STATUS IS WS-STATUS-CONTAPAGAR.

       DATA DIVISION.
       FILE SECTION.
       FD  FORNECEDORES-FILE.
       01  FORNECEDOR-REG.
           05 F-CNPJ           PIC 9(14).
           05 F-RAZAO-SOCIAL   PIC X(40).
           05 F-ENDERECO       PIC X(50).
           05 F-TELEFONE       PIC 9(11).
           05 F-EMAIL          PIC X(30).

       FD  CONTAPAGAR-FILE.
       01  CONTA-REG.
           05 CP-PRIMARY-KEY.
              10 CP-NUM-DOC    PIC 9(10).
              10 CP-CNPJ-FORN  PIC 9(14).
           05 CP-DATA-EMISSAO  PIC 9(08).
           05 CP-DATA-VENC     PIC 9(08).
           05 CP-VALOR         PIC 9(10)V99.
           05 CP-SITUACAO      PIC X(01).
           05 CP-DATA-PGTO     PIC 9(08).

       WORKING-STORAGE SECTION.
       01 WS-STATUS-FORNECEDORES PIC X(2).
           88 STATUS-OK-FORN     VALUE '00'.
       01 WS-STATUS-CONTAPAGAR   PIC X(2).
           88 STATUS-OK-CP       VALUE '00'.

       01 WS-CNPJ-CONSULTA       PIC 9(14).
       01 WS-CONTAS-ENCONTRADAS  PIC X(1) VALUE 'N'.
       01 WS-FIM-LEITURA         PIC X(1) VALUE 'N'.
       01 WS-PAUSA               PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           DISPLAY "--- Consulta de Contas por Fornecedor ---".
           OPEN INPUT FORNECEDORES-FILE.
           OPEN INPUT CONTAPAGAR-FILE.

           PERFORM 200-VALIDAR-FORNECEDOR.

           IF F-CNPJ NOT = ZERO
               PERFORM 300-LISTAR-CONTAS
           END-IF.

           CLOSE FORNECEDORES-FILE.
           CLOSE CONTAPAGAR-FILE.
           GOBACK.

       200-VALIDAR-FORNECEDOR.
           MOVE ZEROS TO F-CNPJ.
           DISPLAY "Digite o CNPJ do fornecedor: " WITH NO ADVANCING.
           ACCEPT WS-CNPJ-CONSULTA.
           MOVE WS-CNPJ-CONSULTA TO F-CNPJ.

           READ FORNECEDORES-FILE
               INVALID KEY
                   DISPLAY "ERRO: Fornecedor nao cadastrado."
                   MOVE ZEROS TO F-CNPJ
               NOT INVALID KEY
                   DISPLAY "Fornecedor: " F-RAZAO-SOCIAL
           END-READ.

       300-LISTAR-CONTAS.
           MOVE F-CNPJ TO CP-CNPJ-FORN.
           MOVE LOW-VALUES TO CP-NUM-DOC.

           START CONTAPAGAR-FILE KEY IS GREATER THAN OR EQUAL TO
               CP-PRIMARY-KEY
                   INVALID KEY
                       MOVE 'Y' TO WS-FIM-LEITURA
                   NOT INVALID KEY
                       PERFORM 400-LER-PROXIMA-CONTA
           END-START.

           PERFORM UNTIL WS-FIM-LEITURA = 'Y'
               PERFORM 400-LER-PROXIMA-CONTA
           END-PERFORM.

           IF WS-CONTAS-ENCONTRADAS = 'N'
               DISPLAY "Nenhuma conta a pagar encontrada para este fornecedor."
           END-IF.

           DISPLAY "-------------------- FIM DA CONSULTA --------------------".
           DISPLAY "Pressione <ENTER> para continuar..." WITH NO ADVANCING.
           ACCEPT WS-PAUSA.

       400-LER-PROXIMA-CONTA.
           READ CONTAPAGAR-FILE NEXT RECORD
               AT END
                   MOVE 'Y' TO WS-FIM-LEITURA
           END-READ.

           IF CP-CNPJ-FORN NOT = F-CNPJ OR WS-FIM-LEITURA = 'Y'
               MOVE 'Y' TO WS-FIM-LEITURA
           ELSE
               IF CP-SITUACAO = 'A'
                   IF WS-CONTAS-ENCONTRADAS = 'N'
                       PERFORM 500-IMPRIMIR-CABECALHO
                       MOVE 'Y' TO WS-CONTAS-ENCONTRADAS
                   END-IF
                   PERFORM 600-IMPRIMIR-LINHA
               END-IF
           END-IF.

       500-IMPRIMIR-CABECALHO.
           DISPLAY " ".
           DISPLAY "----------- CONTAS A PAGAR EM ABERTO -----------".
           DISPLAY "Num. Docto  Dt. Emissao  Dt. Vencim.      Valor".
           DISPLAY "----------- ----------   -----------  -----------".

       600-IMPRIMIR-LINHA.
           DISPLAY CP-NUM-DOC "    " CP-DATA-EMISSAO "   " CP-DATA-VENC
                   "  " CP-VALOR.
