from NAME import NAME
from RETRIV import RETRIV

from mprint import mprint


def RTVADJ(LSZSPC, L, M):
    """SPACデータのうち、次のIWALデータを探して先頭ポインタ等を返す

    # INTEGER     LSZSPC(0:4)  XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,(3):WNDW, (4):INFL の変数の数
    # INTEGER     L            SPACデータのうち、OWAL, IWAL等の先頭ポインタ（検索開始点）
    #                          同 検索されたIWAL(adjacent)の先頭ポインタ(ISTAT=1,-2のときのみ有効)
    # INTEGER     JZ           検索されたIWAL(adjacent)の隣接スペースは、当該グループの何スペース目か(ISTAT=1のときのみ有効）
    # INTEGER     ISTAT        =1 : IWAL(adjacent)が見つかった=0 : IWAL(adjacent)は見つからずに正常終了=-1: 異常終了=-2: adjacent wallにも関わらず隣接SPACが見つからない
    # M(L)                     =1:OWAL, =2:IWAL, =3:WNDW, 4:INFL, 5:SPAC終了
    # M(L+1) (IWALデータ)      隣室モード(=3のときadjacent wall)
    # Q(L+2) (IWALデータ)      隣室SPAC名(M(L+1)=3のとき有効)
    """

    JZ = 0
    ISTAT = 0

    # メインループ
    for II in range(1, 10000):
        
        # mprint("RTVADJ II", II)
        # mprint("RTVADJ L", L)
        # mprint("RTVADJ M[L]", M[L])
        # mprint("RTVADJ M[L+1]", M[L+1])

        if M[L] < 1 or M[L] > 5:
            # mprint("RTVADJ --aルート---", "")
            # mprint("RTVADJ M[L]:", M[L])
            raise Exception("RTVADJでエラーが発生しました")

        elif M[L] == 5:  # SPACデータ終了
            # mprint("RTVADJ --bルート---", "")
            ISTAT = 0
            return L, JZ, ISTAT
        
        elif M[L] != 2:  # IWAL以外
            # mprint("RTVADJ --cルート---", "")
            L += LSZSPC[int(M[L])]

        elif M[L+1] != 3:  # IWALだがadjacent wallではない
            # mprint("RTVADJ --dルート---", "")
            L += LSZSPC[int(M[L])]

        else:  # IWAL で adjacent wall
            # mprint("RTVADJ --eルート---", "")

            QSP = NAME(M[int(L+2)])
            # mprint("RTVADJ QSP", QSP)

            (NNAM,LC,LD) = RETRIV(106,QSP,M)
            # mprint("RTVADJ NNAM", NNAM)
            # mprint("RTVADJ LC", LC)
            # mprint("RTVADJ LD", LD)
            # mprint("RTVADJ M[int(LC+101)] ", M[int(LC+101)] )

            if LD != LC:
                ISTAT = -2  # adjacent wallだが隣接SPACは見つからない（ERROR5相当。ただしL だけは正しく返せる）
            else:
                JZ = M[int(LC+101)]  # ! 隣接スペースは当該グループのうち何番目に登録されているか
                ISTAT = 1       # IWAL(adjacent)とその隣接スペースが見つかった
            
            return L, JZ, ISTAT

    # 異常終了
    ISTAT = -1
    
    return L, JZ, ISTAT