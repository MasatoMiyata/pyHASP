from NAME import NAME
from RETRIV import RETRIV

#-----------------------------------------------------------------------
#     SPACデータのうち、次のIWALデータを探して先頭ポインタ等を返す
#-----------------------------------------------------------------------
def RTVADJ(LSZSPC, L, M):

    #       INTEGER     LSZSPC(0:4)          ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
    #                                        !     (3):WNDW, (4):INFL の変数の数
    #       INTEGER     L                    ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ
    #                                        !     （検索開始点）
    #                                        ! O   同 検索されたIWAL(adjacent)の先頭ポインタ
    #                                        !     (ISTAT=1,-2のときのみ有効)
    #       INTEGER     JZ                   ! O   検索されたIWAL(adjacent)の隣接スペースは、当該
    #                                        !     グループの何スペース目か(ISTAT=1のときのみ有効）
    #       INTEGER     ISTAT                ! O   =1 : IWAL(adjacent)が見つかった
    #                                        !     =0 : IWAL(adjacent)は見つからずに正常終了
    #                                        !     =-1: 異常終了
    #                                        !     =-2: adjacent wallにも関わらず隣接SPACが見つからない
    # C     M(L)                             I   =1:OWAL, =2:IWAL, =3:WNDW, 4:INFL, 5:SPAC終了
    # C     M(L+1) (IWALデータ)              I   隣室モード(=3のときadjacent wall)
    # C     Q(L+2) (IWALデータ)              I   隣室SPAC名(M(L+1)=3のとき有効)

    # ローカル変数
    JZ = 0
    ISTAT = 0

    # メインループ
    for II in range(1, 10000):
        
        # print(f"II: {II}")
        # print(f"L:{L}")
        # print(f"M[L]:{M[L]}")
        # print(f"M[L+1]:{M[L+1]}")
        # print(f"LSZSPC[int(M[L])]:{LSZSPC[int(M[L])]}")

        if M[L] < 1 or M[L] > 5:
            # print("--aルート---")
            print(f"M[L]: {M[L]}")
            raise Exception("RTVADJでエラーが発生しました")

        elif M[L] == 5:  # SPACデータ終了
            # print("--bルート---")
            ISTAT = 0
            return L, JZ, ISTAT
        
        elif M[L] != 2:  # IWAL以外
            # print("--cルート---")
            L += LSZSPC[int(M[L])]

        elif M[L+1] != 3:  # IWALだがadjacent wallではない
            # print("--dルート---")
            L += LSZSPC[int(M[L])]

        else:  # IWAL で adjacent wall
            # print("--eルート---")

            QSP = NAME(LSZSPC[L + 2])
            # print(f"QSP: {QSP}")

            (NNAM,LC,LD) = RETRIV(106,QSP,M)

            if LD != LC:
                ISTAT = -2  # adjacent wallだが隣接SPACは見つからない（ERROR5相当。ただしL だけは正しく返せる）
            else:
                JZ = M[LC+101]  # ! 隣接スペースは当該グループのうち何番目に登録されているか
                ISTAT = 1       # IWAL(adjacent)とその隣接スペースが見つかった
            
            return L, JZ, ISTAT

    # 異常終了
    ISTAT = -1
    
    return L, JZ, ISTAT