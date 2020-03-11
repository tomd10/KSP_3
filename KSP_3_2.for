C     PROGRAM NA RESENI KSP Z3 ULOHA 2
C     AUTOR: TOMAS CHABADA P4
      
C     POMOCNE METODY
      MODULE METODY
      IMPLICIT NONE
      
C     GLOBALNI PROMENNA PRO REKURZI
C     VZDALENOST OD NEJMENSIHO JIZ NALEZENEHO
      INTEGER, PUBLIC :: VZDALENOST = 0
      CHARACTER(LEN=1), PUBLIC :: PISMENO = '.'
      INTEGER :: XMAX, YMAX
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: PROHLEDANE
      INTEGER :: VX, VY
      CONTAINS
      
C     METODA PRO PROHLEDANI MAPY     
      RECURSIVE SUBROUTINE PROHLEDEJ(MAPA_VST, MAPA_VYST, X, Y)
      IMPLICIT NONE
      CHARACTER(LEN=XMAX), DIMENSION(YMAX), INTENT(INOUT) :: MAPA_VST
      CHARACTER(LEN=XMAX), DIMENSION(YMAX), INTENT(INOUT) :: MAPA_VYST
      INTEGER, INTENT(IN) :: X, Y
      
      INTEGER :: VZD
      
      
      VZD = ABS(VX-X) + ABS(VY-Y)
      
C     ZKONTROLUJEME, ZDA JSME POLE JIZ NEPROHLEDAVALI
      IF (PROHLEDANE(X,Y) /= 0) THEN
          RETURN
      ELSE
          PROHLEDANE(X,Y) = 1
      END IF
      
C     ZKONTROLUJEME, ZDA NEJSME DALE, NEZ NALEZENE PISMENO
      IF (VZDALENOST < VZD) THEN
          RETURN
      END IF
      
C     NARAZILI JSME NA VYCHOZI PISMENO     
      IF (MAPA_VST(Y)(X:X) /= '.') THEN
C         NEMAME SHODU?
          IF (VZDALENOST == VZD .AND. PISMENO /= MAPA_VST(Y)(X:X)) THEN
              PISMENO = '.'
              GO TO 10
          END IF
C         NASLI JSME LEPSI
          IF (VZDALENOST > VZD) THEN
              PISMENO = MAPA_VST(Y)(X:X)
              VZDALENOST = VZD
          END IF
 10   END IF
      
C     REKURZE NA DALSI PRVKY
      
      IF (X < XMAX) THEN
          CALL PROHLEDEJ(MAPA_VST, MAPA_VYST, X+1, Y)
      END IF
      IF (X > 1) THEN
          CALL PROHLEDEJ(MAPA_VST, MAPA_VYST, X-1, Y)
      END IF
      IF (Y < YMAX) THEN
          CALL PROHLEDEJ(MAPA_VST, MAPA_VYST, X, Y+1)
      END IF
      IF (Y > 1) THEN
          CALL PROHLEDEJ(MAPA_VST, MAPA_VYST, X, Y-1)
      END IF
      END SUBROUTINE PROHLEDEJ
      END MODULE METODY
      
C     HLAVNI PROGRAM
      PROGRAM MAIN
      USE METODY
      IMPLICIT NONE
      
C     DEKLARACE PROMENNYCH
      INTEGER :: X, Y
      CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: MAPA_VSTUP
      
      CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: MAPA_VYSTUP
      INTEGER :: I = 0, J =0
      
      
C     CTENI ZE SOUBORU, ALOKACE POLI
      OPEN(10, FILE="vstup.txt", STATUS="OLD")
      READ(10, *) Y, X
      ALLOCATE(MAPA_VSTUP(Y), MOLD=REPEAT(".", X))
      ALLOCATE(MAPA_VYSTUP(Y), MOLD=REPEAT(".", X))
      ALLOCATE(PROHLEDANE(X,Y))
      
      DO I=1, Y
          READ(10, "(A)") MAPA_VSTUP(I)  
      END DO      
      CLOSE(10)
      

      XMAX = X
      YMAX = Y
C     VYPOCET POLI 
      DO I=1,Y
          DO J=1,X
C             INICIALIZACE PARAMETRU PRO KAZDE VOLANI             
              VZDALENOST = HUGE(VZDALENOST)
              PROHLEDANE = 0
              PISMENO = '.'
              VX = J
              VY = I
              
C             VOLANI REK. METODY + ZAPIS DO POLE VYSTUPU             
              CALL PROHLEDEJ(MAPA_VSTUP, MAPA_VYSTUP, J, I)
              MAPA_VYSTUP(I)(J:J) = PISMENO
          END DO
      END DO
      

C     VYPIS VYSTUPU
      OPEN(11, FILE="vystup.txt", STATUS="REPLACE")
      DO I=1,Y
          WRITE(11, "(A)") MAPA_VYSTUP(I)
      END DO
      
      PAUSE
      END PROGRAM MAIN
      