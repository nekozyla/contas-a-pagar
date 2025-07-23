       >>SOURCE FORMAT IS FREE *> Basicamente a flag -F
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
       AUTHOR. Neko&lilyaragao&carol.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPCAO PIC 9(1).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL WS-OPCAO = 6
           CALL "SYSTEM" USING "clear" *> Limpa a tela.
               DISPLAY "============================================="
               DISPLAY "    SISTEMA DE CONTAS A PAGAR"
               DISPLAY "============================================="
               DISPLAY "1 - Cadastro de Fornecedores"
               DISPLAY "2 - Lancamento de Contas"
               DISPLAY "3 - Pagamento de Contas"
               DISPLAY "4 - Consultas"
               DISPLAY "5 - Relatorios"
               DISPLAY "6 - Sair"
               DISPLAY "Escolha uma opcao: " WITH NO ADVANCING *> Não pular linha
               ACCEPT WS-OPCAO

               EVALUATE WS-OPCAO *> Basicamente um switch case.
                   WHEN 1
                       CALL 'CADFORNE'
                   WHEN 2
                       CALL 'LANCCONT'
                   WHEN 3
                       CALL 'PGTOCONT'
                   WHEN 4
                       CALL 'CONSFORN'
                   WHEN 5
                       CALL 'RELMENU'
                   WHEN 6
                       DISPLAY "Sistema finalizado."
                   WHEN OTHER
                       DISPLAY "Opcao invalida!"
               END-EVALUATE
           END-PERFORM.

           STOP RUN.
