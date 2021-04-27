C-----------------------------------------------------------------------
C     除去熱量計算のための前処理（その1）
C     LATEST REVISION   2005.01.14 (extrc1.for 2004.02.21より）
C-----------------------------------------------------------------------
      SUBROUTINE EXTRC0(JHR,LOPC,LC,ISEAS,KSCH,IOPTWK)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     引数
      INTEGER     JHR                  ! I   時刻
      INTEGER     LOPC                 ! I   OPCOデータへのポインタ(L)
      INTEGER     LC                   ! I   SPACデータへのポインタ(L)
      INTEGER     ISEAS(2)             ! I   本日、翌日の季節 値=1:夏期、2:冬期、3:中間期
      INTEGER     KSCH(2)              ! I   本日、翌日のスケジュール値(1～3、3は空調は停止)
      INTEGER     IOPTWK               ! 0   空調運転状態フラグ、=0:停止中、
                                       !     =1:運転中、=2:起動、=3:停止
C     X(119,120,...,143)                 I   空調用ダミースケジュール（=-1,0,0,..,0）
C     X(LOPC+14,15,...,163)              I   空調発停操作（=1:起動、=-1:停止、=0:状態継続）
C     M(LC+60)                           I   1時間前から現時刻までの運転状態（0 or 1)
C                                        O   現時刻から1時間後までの運転状態（0 or 1)

C     ローカル変数
      INTEGER     LPNT(2)              !     今日、明日の空調スケジュールポインタ(0:00の操作)
      INTEGER     ISTATB               !     1時間前から現時刻までの運転状態（0 or 1)
      INTEGER     ISTATA               !     現時刻から1時間後までの運転状態（0 or 1)
      INTEGER     ISW                  !     =1:空調停止時はon, -1:空調時はoff, 0:状態継続
      INTEGER     I


C     今日、明日の空調スケジュールポインタ(0:00の操作)
      DO I = 1, 2
         IF ( KSCH(I).EQ.3 ) THEN   ! 1日中休止の場合
            LPNT(I) = 119   ! ダミースケジュールを参照
         ELSE
            LPNT(I) = LOPC + 14 + (ISEAS(I)-1)*50 + (KSCH(I)-1)*25
         END IF
      END DO


C     空調運転モード ( IOPTG )

      ! 直前の空調運転状態
      ISTATB = M(LC+60)

      ! 現時刻のon-off操作
      IF ( ( JHR.EQ.24 ).AND.( NINT(X(LPNT(2))).NE.0 ) ) THEN
        ISW = NINT(X(LPNT(2)))   ! 翌日「0時」のon-off操作とする
      ELSE
        ISW = NINT(X(LPNT(1)+JHR))
      END IF

      IF ( ( ISTATB.EQ.0 ).AND.( ISW.EQ.1 ) ) THEN
         IOPTWK = 2   ! 起動
         ISTATA = 1
      ELSE IF ( ( ISTATB.EQ.1 ).AND.( ISW.EQ.-1 ) ) THEN
         IOPTWK = 3   ! 停止
         ISTATA = 0
      ELSE
         IOPTWK = ISTATB   ! 停止中あるいは運転中
         ISTATA = ISTATB
      END IF
      M(LC+60) = ISTATA   ! 直後の空調運転状態

      RETURN
      END
