C     PROGRAM NA RESENI KSP Z3 ULOHA 2
C     AUTOR: TOMAS CHABADA P4
      
C     POMOCNE METODY
      MODULE METODY
      INTEGER :: XMAX, YMAX
      INTEGER :: VYSLEDEK
      CONTAINS
      
C     REKURZIVNI METODA PRO HLEDANI CESTY DALNICI
      RECURSIVE SUBROUTINE CESTA(X, Y, MAPA)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: X, Y
      CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: MAPA
      
C     NARAZILI JSME NA JIZ OBSAZENE POLICKO
      IF (MAPA(Y)(X:X) == "X" .OR. MAPA(Y)(X:X) == "#") THEN
          RETURN
      ELSE IF (VYSLEDEK == 1) THEN
          RETURN
      ELSE
C         VYNULUJEME SOUCASNE POLICKO
          MAPA(Y)(X:X) = "X"
      END IF
      
C     DORAZILI JSME NA KONEC     
      IF (Y == 1) THEN
          VYSLEDEK = 1
          RETURN
      END IF
               
C     POKRACUJEME PRIORITNE DOLEVA, NAHORU, DOPRAVA, DOLU
C     PRED KAZDYM VOLANIM KONTROLUJEME SPLNENI              
      IF (X > 1) THEN
          CALL CESTA(X-1, Y, MAPA)
      END IF
      
      IF (VYSLEDEK == 1) THEN
          RETURN
      END IF
      
      IF (Y > 1) THEN
          CALL CESTA(X, Y-1, MAPA)
      END IF
      
      IF (VYSLEDEK == 1) THEN
          RETURN
      END IF
      
      IF (X < XMAX) THEN
          CALL CESTA(X+1, Y, MAPA)
      END IF
      
      IF (VYSLEDEK == 1) THEN
          RETURN
      END IF
      
      IF (Y < YMAX) THEN
          CALL CESTA(X, Y+1, MAPA)
      END IF

      END SUBROUTINE CESTA
      END MODULE METODY
      
C     HLAVNI PROGRAM
      PROGRAM MAIN
      USE METODY
      IMPLICIT NONE
      
C     DEKLARACE PROMENNYCH     
      INTEGER :: DELKA, SIRKA
      CHARACTER(LEN=:), DIMENSION(:), ALLOCATABLE :: MAPA
      INTEGER :: POCET_CEST
      
      INTEGER :: I,J

C     INICIALIZACE PROMENNYCH
      POCET_CEST = 0
      
C     CTENI VSTUPU
      OPEN(10, FILE="vstup.txt", STATUS="OLD")
      READ(10,*) DELKA, SIRKA
      
      ALLOCATE(MAPA(DELKA), MOLD=REPEAT(" ", SIRKA))
      DO I=1,DELKA
          READ(10, "(A)") MAPA(I)
      END DO
      CLOSE(10)
      
C     ZPRACOVANI VSTUPU
      XMAX = SIRKA
      YMAX = DELKA
      
      DO I=1, SIRKA
          VYSLEDEK = 0
          CALL CESTA(I, DELKA, MAPA)
          POCET_CEST = POCET_CEST + VYSLEDEK
      END DO
      
      OPEN(11, FILE="vystup.txt", STATUS="REPLACE")
      WRITE(11, "(I0)") POCET_CEST
      PAUSE
      END PROGRAM MAIN