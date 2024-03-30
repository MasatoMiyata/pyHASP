import numpy as np

def EXTRC0(JHR,LOPC,LC,ISEAS,KSCH,X,M):
    """除去熱量計算のための前処理（その1）

    引数
    INTEGER     JHR                 I   時刻
    INTEGER     LOPC                I   OPCOデータへのポインタ(L)
    INTEGER     LC                  I   SPACデータへのポインタ(L)
    INTEGER     ISEAS(2)            I   本日、翌日の季節 値=1:夏期、2:冬期、3:中間期
    INTEGER     KSCH(2)             I   本日、翌日のスケジュール値(1～3、3は空調は停止)
    INTEGER     IOPTWK              0   空調運転状態フラグ、=0:停止中、
                                        =1:運転中、=2:起動、=3:停止
    X(119,120,...,143)              I   空調用ダミースケジュール（=-1,0,0,..,0）
    X(LOPC+14,15,...,163)           I   空調発停操作（=1:起動、=-1:停止、=0:状態継続）
    M(LC+60)                        I   1時間前から現時刻までの運転状態（0 or 1)
    M(LC+60)                        O   現時刻から1時間後までの運転状態（0 or 1)

    ローカル変数
    INTEGER     LPNT(2)             !     今日、明日の空調スケジュールポインタ(0:00の操作)
    INTEGER     ISTATB              !     1時間前から現時刻までの運転状態（0 or 1)
    INTEGER     ISTATA              !     現時刻から1時間後までの運転状態（0 or 1)
    INTEGER     ISW                 !     =1:空調停止時はon, -1:空調時はoff, 0:状態継続
    INTEGER     I
    """

    LPNT = np.zeros(3)

    # 今日、明日の空調スケジュールポインタ(0:00の操作)
    for I  in [1,2]:
        if ( KSCH[I] == 3 ): # 1日中休止の場合
            LPNT[I] = 119   # ダミースケジュールを参照
        else:
            LPNT[I] = LOPC + 14 + (ISEAS[I]-1)*50 + (KSCH[I]-1)*25

    # 空調運転モード ( IOPTG )

    # 直前の空調運転状態
    ISTATB = M[int(LC+60)]

    # 現時刻のon-off操作
    if ( JHR == 24 ) and ( int(X[ int(LPNT[2]) ]) != 0 ) :
        ISW = int(X[ int(LPNT[2]) ])   # 翌日「0時」のon-off操作とする
    else:
        ISW = int(X[ int(LPNT[1]+JHR) ])

    if ( ISTATB == 0 ) and ( ISW == 1 ):
        IOPTWK = 2   # 起動
        ISTATA = 1
    elif ( ISTATB == 1 ) and ( ISW == -1 ):
        IOPTWK = 3   # 停止
        ISTATA = 0
    else:
        IOPTWK = ISTATB   # 停止中あるいは運転中
        ISTATA = ISTATB

    M[LC+60] = ISTATA   # 直後の空調運転状態

    return IOPTWK, M