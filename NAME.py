def NAME(NNAM):
    """
    RETRIVの逆処理
    数値を文字列化

    NAME("15010101")
    """
    QLIT = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=+-*/(),.:"

    QNAM = ""
    NN = NNAM

    for I in [1,2,3,4]:

        # 冒頭の2桁を取り出す
        N=int(int(NN)/100**(4-I))

        # 文字列の連結演算子
        QNAM = QNAM + QLIT[N-1]

        # 処理済みの数値を削除
        NN = int(NN)-N*100**(4-I)

    return QNAM