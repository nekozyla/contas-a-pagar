       >>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGTOCONT.
       AUTHOR. Neko.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTAPAGAR-FILE
               ASSIGN TO 'CONTAPAGAR.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CP-PRIMARY-KEY
               FILE STATUS IS WS-STATUS-CONTAPAGAR.

           SELECT HISTPAGTO-FILE
               ASSIGN TO 'HISTPAGTO.DAT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-HISTPAGTO.

       DATA DIVISION.
       FILE SECTION.
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

       FD  HISTPAGTO-FILE.
       01  HIST-REG.
           05 H-NUM-DOC        PIC 9(10).
           05 H-CNPJ-FORN      PIC 9(14).
           05 H-DATA-PGTO      PIC 9(08).
           05 H-VALOR-PAGO     PIC 9(10)V99.

       WORKING-STORAGE SECTION.
       01 WS-STATUS-CONTAPAGAR     PIC X(2).
          88 STATUS-OK-CP           VALUE '00'.
          88 ARQUIVO-NAO-ENCONTRADO-CP VALUE '35'.
       01 WS-STATUS-HISTPAGTO      PIC X(2).
          88 STATUS-OK-HIST         VALUE '00'.

       01 WS-OPCAO                 PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           PERFORM 200-MENU-PAGAMENTO UNTIL WS-OPCAO = 'S'.
           GOBACK.

       200-MENU-PAGAMENTO.
           DISPLAY "--- Pagamento de Contas ---".
           DISPLAY "P - Pagar uma conta".
           DISPLAY "S - Sair para o menu principal".
           ACCEPT WS-OPCAO.

           EVALUATE FUNCTION UPPER-CASE(WS-OPCAO)
               WHEN 'P'
                   PERFORM 300-PAGAR-CONTA
               WHEN 'S'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opcao invalida!"
           END-EVALUATE.

       300-PAGAR-CONTA.
           OPEN I-O CONTAPAGAR-FILE.
      *>--- Correção: Verifica se o arquivo não existe (Status 35) ---*
           IF ARQUIVO-NAO-ENCONTRADO-CP
               DISPLAY "AVISO: Nenhuma conta foi lancada no sistema ainda."
               CLOSE CONTAPAGAR-FILE
               EXIT PARAGRAPH
           END-IF.

           IF NOT STATUS-OK-CP
               DISPLAY "ERRO ao abrir arquivo de contas. Status: "
                       WS-STATUS-CONTAPAGAR
               GOBACK
           END-IF.

           DISPLAY "Digite o Numero do Documento a pagar: "
                   WITH NO ADVANCING.
           ACCEPT CP-NUM-DOC.
           DISPLAY "Digite o CNPJ do fornecedor: " WITH NO ADVANCING.
           ACCEPT CP-CNPJ-FORN.

           READ CONTAPAGAR-FILE
               INVALID KEY
                   DISPLAY "ERRO: Conta nao encontrada."
                   CLOSE CONTAPAGAR-FILE
                   EXIT PARAGRAPH
           END-READ.

           IF CP-SITUACAO NOT = 'A'
               DISPLAY "AVISO: Esta conta ja foi paga ou cancelada."
               CLOSE CONTAPAGAR-FILE
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Valor da Conta: " CP-VALOR.
           DISPLAY "Digite a Data do Pagamento (AAAAMMDD): "
                   WITH NO ADVANCING.
           ACCEPT CP-DATA-PGTO.

           MOVE 'P' TO CP-SITUACAO.

           REWRITE CONTA-REG
               INVALID KEY
                   DISPLAY "ERRO CRITICO ao atualizar conta. Status: "
                           WS-STATUS-CONTAPAGAR
                   CLOSE CONTAPAGAR-FILE
                   EXIT PARAGRAPH
               NOT INVALID KEY
                   DISPLAY "Conta atualizada para PAGA com sucesso."
                   PERFORM 400-GRAVAR-HISTORICO
           END-REWRITE.

           CLOSE CONTAPAGAR-FILE.

       400-GRAVAR-HISTORICO.
           OPEN EXTEND HISTPAGTO-FILE.
           IF NOT STATUS-OK-HIST
               DISPLAY "ERRO GRAVE ao abrir historico. Status: "
                       WS-STATUS-HISTPAGTO
               EXIT PARAGRAPH
           END-IF.

           MOVE CP-NUM-DOC  TO H-NUM-DOC.
           MOVE CP-CNPJ-FORN TO H-CNPJ-FORN.
           MOVE CP-DATA-PGTO TO H-DATA-PGTO.
           MOVE CP-VALOR     TO H-VALOR-PAGO.

           WRITE HIST-REG.
           IF NOT STATUS-OK-HIST
               DISPLAY "ERRO ao gravar no historico. Status: "
                       WS-STATUS-HISTPAGTO
           END-IF.

           CLOSE HISTPAGTO-FILE.
