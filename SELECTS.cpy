       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *--- Arquivo de Fornecedores, indexado pela chave CNPJ ---*
           SELECT FORNECEDORES-FILE
           ASSIGN TO 'FORNECEDOR.DAT'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS F-CNPJ
           FILE STATUS IS WS-STATUS-FORNECEDORES.

      *--- Arquivo de Contas a Pagar, chave primária composta ---*
           SELECT CONTAPAGAR-FILE
           ASSIGN TO 'CONTAPAGAR.DAT'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CP-PRIMARY-KEY
           FILE STATUS IS WS-STATUS-CONTAPAGAR.

      *--- Arquivo de Histórico, sequencial para adicionar pagamentos ---*
           SELECT HISTPAGTO-FILE
           ASSIGN TO 'HISTPAGTO.DAT'
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-HISTPAGTO.