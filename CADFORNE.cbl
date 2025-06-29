       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADFORNE.
       AUTHOR. Lilian.

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

       01 WS-FORNECEDOR-REG.
           05 WS-F-CNPJ           PIC 9(14).
           05 WS-F-RAZAO-SOCIAL   PIC X(40).
           05 WS-F-ENDERECO       PIC X(50).
           05 WS-F-TELEFONE       PIC 9(11).
           05 WS-F-EMAIL          PIC X(30).

       01 WS-OPCAO-CAD            PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           PERFORM 200-MOSTRAR-MENU-CADASTRO UNTIL WS-OPCAO-CAD = 'S'.
           GOBACK.

       200-MOSTRAR-MENU-CADASTRO.
           DISPLAY "--- Cadastro de Fornecedores ---".
           DISPLAY "I - Incluir".
           DISPLAY "A - Alterar (Nao implementado)".
           DISPLAY "E - Excluir (Nao implementado)".
           DISPLAY "S - Sair para o menu principal".
           ACCEPT WS-OPCAO-CAD.

           EVALUATE FUNCTION UPPER-CASE(WS-OPCAO-CAD)
               WHEN 'I'
                   PERFORM 300-INCLUIR-FORNECEDOR
               WHEN 'A'
                   DISPLAY "Rotina de Alteracao a ser implementada."
               WHEN 'E'
                   DISPLAY "Rotina de Exclusao a ser implementada."
               WHEN 'S'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opcao de cadastro invalida!"
           END-EVALUATE.

       300-INCLUIR-FORNECEDOR.
           DISPLAY "--- Inclusao de Novo Fornecedor ---".
           OPEN I-O FORNECEDORES-FILE.

           IF ARQUIVO-NAO-ENCONTRADO
               OPEN OUTPUT FORNECEDORES-FILE
           END-IF.

           IF NOT STATUS-OK
               DISPLAY "Erro ao abrir arquivo de fornecedores: "
                       WS-STATUS-FORNECEDORES
               GOBACK
           END-IF.

           DISPLAY "Digite o CNPJ (14 digitos): " WITH NO ADVANCING
           ACCEPT F-CNPJ.

           READ FORNECEDORES-FILE
               INVALID KEY
                   DISPLAY "Digite a Razao Social: " WITH NO ADVANCING
                   ACCEPT F-RAZAO-SOCIAL
                   DISPLAY "Digite o Endereco: " WITH NO ADVANCING
                   ACCEPT F-ENDERECO
                   DISPLAY "Digite o Telefone (11 digitos): "
                           WITH NO ADVANCING
                   ACCEPT F-TELEFONE
                   DISPLAY "Digite o E-mail: " WITH NO ADVANCING
                   ACCEPT F-EMAIL

                   WRITE FORNECEDOR-REG
                       INVALID KEY
                           DISPLAY "ERRO CRITICO ao gravar. Status: "
                                   WS-STATUS-FORNECEDORES
                       NOT INVALID KEY
                           DISPLAY "Fornecedor cadastrado com sucesso!"
                   END-WRITE
               NOT INVALID KEY
                   DISPLAY "ERRO: CNPJ ja cadastrado no sistema."
           END-READ.

           CLOSE FORNECEDORES-FILE.
