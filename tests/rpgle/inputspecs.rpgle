
     FMASTER    IP   F   96        DISK
     FOUTMAST   UF   F  256     5AIDISK    KEYLOC(3)

0004 IMASTER    NS  01    1 CA    2 CJ   10 CG                                  ADCHAR
0005 I                                  3    7 0CUSNR                           ADCHAR
     I                             A   11   16  INVNBRA
     I                             S   11   16 0INVNBR
0006 I                                 28   34 2INVAM                           ADCHAR
0007 I          NS  02    1 CA    2 CK                                          ADCHAR
     I                             A   11   16  INVNBRA
     I                             S   11   16 0INVNBR
0008 I                                  3    7 0CUSNR                           ADCHAR
0009 I                                 28   34 2TTLCR                           ADCHAR
0010 I          NS  03    1 CA    2 CL                                          ADCHAR
0011 I                                  3    7 0CUSNR                           ADCHAR
     I                             A   11   16  INVNBRA
     I                             S   11   16 0INVNBR
0012 I                                 28   34 2TDCRA                           ADCHAR
0013 I          NS  04    1 CA    2 CD                                          ADCHAR
0014 I                                  3    7 0CUSNR                           ADCHAR
     I                             A   11   16  INVNBRA
     I                             S   11   16 0INVNBR
0015 I                                 28   34 2CRAMT                           ADCHAR
0016 I          NS  05                                                          ADCHAR
0017 IOUTMAST   NS  06                                                          ADCHAR
0018 I                                244  250 2TOT                             ADCHAR
     C  N99              Z-ADD     *ZEROS        FLD7              7 2
     C  N99              SETON                                        99
0019 C                   Z-ADD     0             TOTAL                                         ADCHA
0020 C                   SETOFF                                       50                       ADCHA
0021 C   05              GOTO      ENDETL                                                      ADCHA
0022 C     CUSNR         CHAIN     AIDMASAR                           90                       ADCHA
0023 C   90              GOTO      ENDETL                                                      ADCHA
0024 C   01TOT           ADD       INVAM         TOTAL             7 2                         ADCHA
0025 C   02TOT           SUB       TTLCR         TOTAL                                         ADCHA
0026 C   03TOT           ADD       TDCRA         TOTAL                                         ADCHA
0027 C   04TOT           ADD       CRAMT         TOTAL                                         ADCHA
0028 C                   SETON                                        50                       ADCHA
0029 C     ENDETL        TAG                                                                   ADCHA
0030 OOUTMAST   D    50                                                         ADCHAR
0031 O                       TOTAL              250                             ADCHAR
     O                       FLD7               254P