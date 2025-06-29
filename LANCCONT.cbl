       >>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LANCCONT.
       AUTHOR. Neko.

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
       01 WS-STATUS-FORNECEDORES   PIC X(2).
          88 STATUS-OK-FORN         VALUE '00'.
       01 WS-STATUS-CONTAPAGAR     PIC X(2).
          88 STATUS-OK-CP           VALUE '00'.
          88 ARQUIVO-NAO-ENCONTRADO-CP VALUE '35'.

       01 WS-OPCAO                 PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           PERFORM 200-MENU-LANCAMENTO UNTIL WS-OPCAO = 'S'.
           GOBACK.

       200-MENU-LANCAMENTO.
           DISPLAY "--- Lancamento de Contas a Pagar ---".
           DISPLAY "L - Lancar nova conta".
           DISPLAY "S - Sair para o menu principal".
           ACCEPT WS-OPCAO.

           EVALUATE FUNCTION UPPER-CASE(WS-OPCAO)
               WHEN 'L'
                   PERFORM 300-LANCAR-CONTA
               WHEN 'S'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opcao invalida!"
           END-EVALUATE.

       300-LANCAR-CONTA.
           OPEN INPUT FORNECEDORES-FILE.
           IF NOT STATUS-OK-FORN
               DISPLAY "ERRO: Nao foi possivel abrir o cadastro de fornecedores."
               DISPLAY "Status: " WS-STATUS-FORNECEDORES
               GOBACK
           END-IF.

           DISPLAY "Digite o CNPJ do fornecedor: " WITH NO ADVANCING.
           ACCEPT CP-CNPJ-FORN.
           MOVE CP-CNPJ-FORN TO F-CNPJ.

           READ FORNECEDORES-FILE
               INVALID KEY
                   DISPLAY "ERRO: Fornecedor com este CNPJ nao encontrado."
                   CLOSE FORNECEDORES-FILE
                   EXIT PARAGRAPH
           END-READ.
           CLOSE FORNECEDORES-FILE.

           DISPLAY "Fornecedor: " F-RAZAO-SOCIAL.
           DISPLAY "Digite o Numero do Documento: " WITH NO ADVANCING.
           ACCEPT CP-NUM-DOC.

           OPEN I-O CONTAPAGAR-FILE.
           IF ARQUIVO-NAO-ENCONTRADO-CP
               OPEN OUTPUT CONTAPAGAR-FILE
           END-IF.

           IF NOT STATUS-OK-CP
               DISPLAY "ERRO: Nao foi possivel abrir o arquivo de contas."
               DISPLAY "Status: " WS-STATUS-CONTAPAGAR
               GOBACK
           END-IF.

           READ CONTAPAGAR-FILE
               NOT INVALID KEY
                   DISPLAY "ERRO: Documento ja lancado para este CNPJ."
                   CLOSE CONTAPAGAR-FILE
                   EXIT PARAGRAPH
           END-READ.

           DISPLAY "Digite a Data de Emissao (AAAAMMDD): "
                   WITH NO ADVANCING.
           ACCEPT CP-DATA-EMISSAO.
           DISPLAY "Digite a Data de Vencimento (AAAAMMDD): "
                   WITH NO ADVANCING.
           ACCEPT CP-DATA-VENC.

           IF CP-DATA-VENC < CP-DATA-EMISSAO
               DISPLAY "ERRO: Data de vencimento anterior a emissao."
               CLOSE CONTAPAGAR-FILE
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Digite o Valor da Conta: " WITH NO ADVANCING.
           ACCEPT CP-VALOR.

           IF CP-VALOR <= ZERO
               DISPLAY "ERRO: O valor da conta deve ser positivo."
               CLOSE CONTAPAGAR-FILE
               EXIT PARAGRAPH
           END-IF.

           MOVE 'A' TO CP-SITUACAO.
           MOVE ZEROS TO CP-DATA-PGTO.

           WRITE CONTA-REG
               INVALID KEY
                   DISPLAY "ERRO CRITICO ao gravar conta. Status: "
                           WS-STATUS-CONTAPAGAR
               NOT INVALID KEY
                   DISPLAY "Conta lancada com sucesso!"
           END-WRITE.

           CLOSE CONTAPAGAR-FILE.
