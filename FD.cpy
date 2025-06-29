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

       FD  HISTPAGTO-FILE.
       01  HIST-REG.
           05 H-NUM-DOC        PIC 9(10).
           05 H-CNPJ-FORN      PIC 9(14).
           05 H-DATA-PGTO      PIC 9(08).
           05 H-VALOR-PAGO     PIC 9(10)V99.