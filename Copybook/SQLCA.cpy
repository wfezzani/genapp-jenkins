       01  SQLCA.
           05  SQLCAID               PIC X(8).
           05  SQLCABC               PIC S9(9) COMP-5.
           05  SQLCODE               PIC S9(9) COMP-5.
           05  SQLERRM.
               10 SQLERRML          PIC S9(4) COMP.
               10 SQLERRMC          PIC X(70).
           05  SQLERRP               PIC X(8).
           05  SQLERRD.
               10 SQLERRD1          PIC S9(9) COMP.
               10 SQLERRD2          PIC S9(9) COMP.
               10 SQLERRD3          PIC S9(9) COMP.
               10 SQLERRD4          PIC S9(9) COMP.
               10 SQLERRD5          PIC S9(9) COMP.
               10 SQLERRD6          PIC S9(9) COMP.
           05  SQLWARN               PIC X(11).
           05  SQLSTATE              PIC X(5).
