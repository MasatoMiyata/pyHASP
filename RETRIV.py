def RETRIV(LO,QNAM,M):
    """
    CODE NAME RETRIEVAL
    文字列を数値化
    """
    QLIT = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=+-*/(),.:"

    # 文字列の数値化 NNAM
    # 例えば、 N___ であれば 15010101、 E___  であれば 6010101
    NNAM = 0
    for i in range(0,4):
        N = QLIT.find(QNAM[i])+1
        if N==0:
            N=1
        NNAM = 100*NNAM + N

    # EXPSの場合、L0=100
    LC=LO
    LD=LO

    while True:

        if M[int(LC+1)] == NNAM:
            return NNAM,LC,LD

        LD = M[int(LC)]
        if LD == 0:       # M(LC)=0であれば空いている。
            return NNAM,LC,LD

        LC = LD