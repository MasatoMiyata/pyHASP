C-----------------------------------------------------------------------
C     SPACデータのうち、次のIWALデータを探して先頭ポインタ等を返す
C     LATEST REVISION   2012.03.05
C-----------------------------------------------------------------------
      SUBROUTINE RTVADJ(LSZSPC,L,JZ,ISTAT)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     引数
      INTEGER     LSZSPC(0:4)          ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                       !     (3):WNDW, (4):INFL の変数の数
      INTEGER     L                    ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ
                                       !     （検索開始点）
                                       ! O   同 検索されたIWAL(adjacent)の先頭ポインタ
                                       !     (ISTAT=1,-2のときのみ有効)
      INTEGER     JZ                   ! O   検索されたIWAL(adjacent)の隣接スペースは、当該
                                       !     グループの何スペース目か(ISTAT=1のときのみ有効）
      INTEGER     ISTAT                ! O   =1 : IWAL(adjacent)が見つかった
                                       !     =0 : IWAL(adjacent)は見つからずに正常終了
                                       !     =-1: 異常終了
                                       !     =-2: adjacent wallにも関わらず隣接SPACが見つからない
C     M(L)                               I   =1:OWAL, =2:IWAL, =3:WNDW, 4:INFL, 5:SPAC終了
C     M(L+1) (IWALデータ)                I   隣室モード(=3のときadjacent wall)
C     Q(L+2) (IWALデータ)                I   隣室SPAC名(M(L+1)=3のとき有効)

C     ローカル変数
      INTEGER     II
      CHARACTER   QSP*4                !     隣室SPAC名(M(L+1)=3のとき有効)
      INTEGER     NNAM
      INTEGER     LC
      INTEGER     LD

      DO II = 1, 9999
         IF( (M(L).LT.1).OR.(M(L).GT.5) ) THEN
            CALL ERROR2(191,2)
            ISTAT = -1   ! 異常終了
            RETURN
         ELSE IF( M(L).EQ.5 ) THEN   ! SPACデータ終了
            ISTAT = 0   ! 見つからずに正常終了
            RETURN
         ELSE IF( M(L).NE.2 ) THEN   ! IWAL以外
            L = L + LSZSPC(M(L))
         ELSE IF( M(L+1).NE.3 ) THEN ! IWALだがadjacent wallではない
            L = L + LSZSPC(M(L))
         ELSE   ! IWALでadjacent wall
            CALL NAME(QSP,M(L+2))
            CALL RETRIV(106,QSP,NNAM,LC,LD)
            IF(LD.NE.LC) THEN
               ISTAT = -2   ! adjacent wallだが隣接SPACは見つからない
                            ! （ERROR5相当。ただしL だけは正しく返せる）
            ELSE
               JZ = M(LC+101)   ! 隣接スペースは当該グループのうち何番目に登録されているか
               ISTAT = 1   ! IWAL(adjacent)とその隣接スペースが見つかった
            END IF
            RETURN
         END IF
      END DO
      CALL ERROR2(192,2)
      ISTAT = -1   ! 異常終了

      RETURN
      END
