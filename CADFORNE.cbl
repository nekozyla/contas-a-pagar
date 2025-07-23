       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADFORNE.
       AUTHOR. lilyaragao.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FORNECEDORES-FILE *> Como o codigo vai se referir ao arquivo
           ASSIGN TO 'FORNECEDOR.DAT' *> Nome do arquivo no computador
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC *> Alterna entre sequencial e random access
           RECORD KEY IS F-CNPJ *> O que vai ser usado pra achar os dados
           FILE STATUS IS WS-STATUS-FORNECEDORES. *> Na variavel WS-STATUS-FORNECEDORES
                                                    *> vai ser armazenado um código de
                                                    *> 2 digitos dizendo se falhou ou teve sucesso
                                                    *> em acessar o arquivo.


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
           88 STATUS-OK                VALUE '00'.   *> Se o arquivo abrir corretamente
           88 ARQUIVO-NAO-ENCONTRADO   VALUE '35'.   *> Erro ao abrir o arquivo
           88 REGISTRO-NAO-ENCONTRADO  VALUE '23'.   *> Nao achou o registro

       01 WS-FORNECEDOR-REG.
           05 WS-F-CNPJ           PIC 9(14).
           05 WS-F-RAZAO-SOCIAL   PIC X(40).
           05 WS-F-ENDERECO       PIC X(50).
           05 WS-F-TELEFONE       PIC 9(11).
           05 WS-F-EMAIL          PIC X(30).

       01 WS-OPCAO-CAD            PIC X(1) VALUE SPACES. *> Uma variavel com um " ", evita lixo de sistema
       01 WS-CONFIRMACAO          PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           PERFORM 200-MOSTRAR-MENU-CADASTRO UNTIL WS-OPCAO-CAD = 'S'.
           GOBACK.

       200-MOSTRAR-MENU-CADASTRO.
           DISPLAY "--- Cadastro de Fornecedores ---".
           DISPLAY "I - Incluir".
           DISPLAY "A - Alterar".
           DISPLAY "E - Excluir".
           DISPLAY "S - Sair para o menu principal".
           ACCEPT WS-OPCAO-CAD.

           EVALUATE FUNCTION UPPER-CASE(WS-OPCAO-CAD)
               WHEN 'I'
                   PERFORM 300-INCLUIR-FORNECEDOR
               WHEN 'A'
                   PERFORM 400-ALTERAR-FORNECEDOR
               WHEN 'E'
                   PERFORM 500-EXCLUIR-FORNECEDOR
               WHEN 'S'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opcao de cadastro invalida!"
           END-EVALUATE.

       *> FUNCAO PARA INCLUIR UM NOVO FORNECEDOR
       300-INCLUIR-FORNECEDOR.
           DISPLAY "--- Inclusao de Novo Fornecedor ---".
           OPEN I-O FORNECEDORES-FILE.
           IF ARQUIVO-NAO-ENCONTRADO
               OPEN OUTPUT FORNECEDORES-FILE    *> Tenta abrir o arquivo dnv
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

      *> FUNCAO PARA ALTERAR DADOS DE UM FORNECEDOR JA EXISTENTE
       400-ALTERAR-FORNECEDOR.
           DISPLAY "--- Alteracao de Fornecedor ---".
           OPEN I-O FORNECEDORES-FILE.
           IF NOT STATUS-OK
               DISPLAY "Erro ao abrir arquivo de fornecedores: "
                       WS-STATUS-FORNECEDORES
               GOBACK
           END-IF.

           DISPLAY "Digite o CNPJ do fornecedor a alterar: "
                   WITH NO ADVANCING.
           ACCEPT F-CNPJ.

           READ FORNECEDORES-FILE
               INVALID KEY
                   DISPLAY "ERRO: Fornecedor com CNPJ " F-CNPJ " nao encontrado."
               NOT INVALID KEY
                   DISPLAY "Dados atuais:"
                   DISPLAY "Razao Social: " F-RAZAO-SOCIAL
                   DISPLAY "Endereco:     " F-ENDERECO
                   DISPLAY "Telefone:     " F-TELEFONE
                   DISPLAY "Email:        " F-EMAIL
                   DISPLAY "--- Digite os novos dados ---"
                   DISPLAY "Digite a nova Razao Social: " WITH NO ADVANCING
                   ACCEPT F-RAZAO-SOCIAL
                   DISPLAY "Digite o novo Endereco: " WITH NO ADVANCING
                   ACCEPT F-ENDERECO
                   DISPLAY "Digite o novo Telefone: " WITH NO ADVANCING
                   ACCEPT F-TELEFONE
                   DISPLAY "Digite o novo E-mail: " WITH NO ADVANCING
                   ACCEPT F-EMAIL

                   REWRITE FORNECEDOR-REG
                       INVALID KEY
                           DISPLAY "ERRO ao alterar o fornecedor. Status: "
                                   WS-STATUS-FORNECEDORES
                       NOT INVALID KEY
                           DISPLAY "Fornecedor alterado com sucesso!"
                   END-REWRITE
           END-READ.
           CLOSE FORNECEDORES-FILE.

      *> FUNCAO PARA EXCLUIR UM FORNECEDOR
       500-EXCLUIR-FORNECEDOR.
           DISPLAY "--- Exclusao de Fornecedor ---".
           OPEN I-O FORNECEDORES-FILE.
           IF NOT STATUS-OK
               DISPLAY "Erro ao abrir arquivo de fornecedores: "
                       WS-STATUS-FORNECEDORES
               GOBACK
           END-IF.

           DISPLAY "Digite o CNPJ do fornecedor a excluir: "
                   WITH NO ADVANCING.
           ACCEPT F-CNPJ.

           READ FORNECEDORES-FILE
               INVALID KEY
                   DISPLAY "ERRO: Fornecedor com CNPJ " F-CNPJ " nao encontrado."
               NOT INVALID KEY
                   DISPLAY "Fornecedor encontrado: " F-RAZAO-SOCIAL
                   DISPLAY "Tem certeza que deseja excluir? (S/N): "
                           WITH NO ADVANCING
                   ACCEPT WS-CONFIRMACAO
                   IF FUNCTION UPPER-CASE(WS-CONFIRMACAO) = 'S'
                       DELETE FORNECEDORES-FILE RECORD
                           INVALID KEY
                               DISPLAY "ERRO ao excluir. Status: "
                                       WS-STATUS-FORNECEDORES
                           NOT INVALID KEY
                               DISPLAY "Fornecedor excluido com sucesso!"
                       END-DELETE
                   ELSE
                       DISPLAY "Operacao de exclusao cancelada."
                   END-IF
           END-READ.
           CLOSE FORNECEDORES-FILE.
