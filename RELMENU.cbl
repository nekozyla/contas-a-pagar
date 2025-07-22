>>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELMENU.
       AUTHOR. Neko.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPCAO-REL    PIC X(1).

       PROCEDURE DIVISION.
       100-INICIAR.
           PERFORM 200-MOSTRAR-MENU-RELATORIO UNTIL WS-OPCAO-REL = 'S'.
           GOBACK.

       200-MOSTRAR-MENU-RELATORIO.
           DISPLAY "=============================================".
           DISPLAY "           Modulo de Relatorios".
           DISPLAY "=============================================".
           DISPLAY "1 - Listagem de Fornecedores".
           DISPLAY "2 - Consultar Contas por Fornecedor".
           DISPLAY "S - Sair para o menu principal".
           DISPLAY "Escolha uma opcao: " WITH NO ADVANCING.
           ACCEPT WS-OPCAO-REL.

           EVALUATE FUNCTION UPPER-CASE(WS-OPCAO-REL)
               WHEN '1'
                   CALL 'RELFORN'
               WHEN '2'
                   CALL 'RELCTA_F'
               WHEN 'S'
                   CONTINUE
               WHEN OTHER
                   DISPLAY "Opcao invalida!"
           END-EVALUATE.
