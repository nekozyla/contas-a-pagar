       >>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSFORN.
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

       DATA DIVISION.
       FILE SECTION.
       FD  FORNECEDORES-FILE.
       01  FORNECEDOR-REG.
           05 F-CNPJ           PIC 9(14).
           05 F-RAZAO-SOCIAL   PIC X(40).
           05 F-ENDERECO       PIC X(50).
           05 F-TELEFONE       PIC 9(11).
           05 F-EMAIL          PIC X(30).

       WORKING-STORAGE SECTION.
       01 WS-STATUS-FORNECEDORES PIC X(2).
          88 STATUS-OK                VALUE '00'.
          88 ARQUIVO-NAO-ENCONTRADO   VALUE '35'.

       01 WS-CNPJ-CONSULTA       PIC 9(14).
       01 WS-OPCAO                 PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           PERFORM 200-MENU-CONSULTA UNTIL WS-OPCAO = 'S'.
           GOBACK.

       200-MENU-CONSULTA.
           DISPLAY "--- Consulta de Fornecedores ---".
           DISPLAY "C - Consultar por CNPJ".
           DISPLAY "S - Sair para o menu principal".
           ACCEPT WS-OPCAO.

           EVALUATE FUNCTION UPPER-CASE(WS-OPCAO)
               WHEN 'C'
                   PERFORM 300-CONSULTAR-FORNECEDOR
               WHEN 'S'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opcao invalida!"
           END-EVALUATE.

       300-CONSULTAR-FORNECEDOR.
           OPEN INPUT FORNECEDORES-FILE.
           IF ARQUIVO-NAO-ENCONTRADO
               DISPLAY "AVISO: Nao ha fornecedores cadastrados."
               CLOSE FORNECEDORES-FILE
               EXIT PARAGRAPH
           END-IF.

           IF NOT STATUS-OK
               DISPLAY "ERRO ao abrir arquivo de fornecedores. Status: "
                       WS-STATUS-FORNECEDORES
               GOBACK
           END-IF.

           DISPLAY "Digite o CNPJ a consultar: " WITH NO ADVANCING.
           ACCEPT WS-CNPJ-CONSULTA.
           MOVE WS-CNPJ-CONSULTA TO F-CNPJ.

           READ FORNECEDORES-FILE
               INVALID KEY
                   DISPLAY "------------------------------------------------"
                   DISPLAY "Fornecedor com CNPJ " F-CNPJ " nao encontrado."
                   DISPLAY "------------------------------------------------"
               NOT INVALID KEY
                   DISPLAY "----------------- DADOS DO FORNECEDOR -----------------"
                   DISPLAY "CNPJ:          " F-CNPJ
                   DISPLAY "Razao Social:  " F-RAZAO-SOCIAL
                   DISPLAY "Endereco:      " F-ENDERECO
                   DISPLAY "Telefone:      " F-TELEFONE
                   DISPLAY "Email:         " F-EMAIL
                   DISPLAY "-----------------------------------------------------"
           END-READ.

           CLOSE FORNECEDORES-FILE.
