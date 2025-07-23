>>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELFORN.
       AUTHOR. carol&lilyargao.

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

       01 WS-EOF                  PIC X(1) VALUE 'N'.
           88 FIM-DE-ARQUIVO        VALUE 'Y'.

       01 WS-PAUSA                PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           OPEN INPUT FORNECEDORES-FILE.
           IF ARQUIVO-NAO-ENCONTRADO
               DISPLAY "AVISO: Nao ha fornecedores cadastrados para listar."
               PERFORM 900-FINALIZAR
           END-IF.
           IF NOT STATUS-OK
               DISPLAY "ERRO ao abrir arquivo de fornecedores. Status: "
                       WS-STATUS-FORNECEDORES
               PERFORM 900-FINALIZAR
           END-IF.

           PERFORM 200-PROCESSAR-RELATORIO.
           PERFORM 900-FINALIZAR.
           GOBACK.

       200-PROCESSAR-RELATORIO.
           PERFORM 300-IMPRIMIR-CABECALHO.
           MOVE LOW-VALUES TO F-CNPJ.
           START FORNECEDORES-FILE KEY IS GREATER THAN F-CNPJ.
           
           PERFORM UNTIL FIM-DE-ARQUIVO
               READ FORNECEDORES-FILE NEXT RECORD
                   AT END
                       SET FIM-DE-ARQUIVO TO TRUE
                   NOT AT END
                       PERFORM 400-IMPRIMIR-LINHA
               END-READ
           END-PERFORM.

       300-IMPRIMIR-CABECALHO.
           DISPLAY " ".
           DISPLAY "----------------- RELATORIO DE FORNECEDORES -----------------".
           DISPLAY "CNPJ            RAZAO SOCIAL                            EMAIL".
           DISPLAY "--------------- --------------------------------------- ------------------------------".

       400-IMPRIMIR-LINHA.
           DISPLAY F-CNPJ " " F-RAZAO-SOCIAL " " F-EMAIL.

       900-FINALIZAR.
           DISPLAY "-------------------- FIM DO RELATORIO ---------------------".
           DISPLAY "Pressione <ENTER> para continuar..." WITH NO ADVANCING.
           ACCEPT WS-PAUSA.
           CLOSE FORNECEDORES-FILE.
