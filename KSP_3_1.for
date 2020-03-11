C     PROGRAM NA RESENI KSP Z3 ULOHA 1
C     AUTOR: TOMAS CHABADA P4
      
C     POMOCNE METODY
      MODULE METODY
      CONTAINS
      
      END MODULE METODY
      
C     HLAVNI PROGRAM
      PROGRAM MAIN
      USE METODY
      IMPLICIT NONE
      
C     DEKLARACE PROMENNYCH
      INTEGER :: POCET_DVOJIC
      CHARACTER(LEN=10000000) :: VSTUP
      
      INTEGER, DIMENSION(0:676) :: POLE_DVOJIC
      INTEGER :: INDEX
      CHARACTER(LEN=2) :: DVOJICE
      INTEGER :: DELKA_VSTUPU, I
      INTEGER :: J, MAX
      
C     INICIALIZACE PROMENNYCH
      POLE_DVOJIC = 0
      INDEX = 0
      
C     CTENI ZE SOUBORU
      OPEN(10, FILE="vstup.txt", STATUS="OLD")
      READ(10, *) POCET_DVOJIC
      READ(10, "(A)") VSTUP
      CLOSE(10)
      
C     OTEVRENI SOUBORU PRO ZAPIS
      OPEN(11, FILE="vystup.txt", STATUS="REPLACE")

C     ZPRACOVANI VSTUPU
      DELKA_VSTUPU = LEN(TRIM(VSTUP))      
      DO I = 1, DELKA_VSTUPU - 1
          IF (MOD(I, 10000) == 0) THEN
              PRINT*, I
          END IF
C         KONTROLA, ZDA PROVERUJEME DVOJICI PISMEN
          IF (VSTUP(I:I) == ' ' .OR. VSTUP(I+1:I+1) == ' ') THEN
              CYCLE
          END IF
          
C         VYPOCET INDEXU V POLI
C         EMULACE 26KOVE SOUSTAVY, NEBUDE FUNGOVAT PRO LOWERCASE
C         1. PISMENO - MSB         
          INDEX =  26 *(IACHAR(VSTUP(I:I)) - 97)
          INDEX = INDEX + IACHAR(VSTUP(I+1:I+1)) - 97
          POLE_DVOJIC(INDEX) = POLE_DVOJIC(INDEX) + 1
      END DO

C     HLEDANI MAXIM 
      DO I = 1, POCET_DVOJIC
          MAX = 1
          DO J = 1, 676
              IF (POLE_DVOJIC(J) > POLE_DVOJIC(MAX)) THEN
                  MAX = J
              END IF
          END DO
          
C         VYPIS NALEZENE DVOJICE + ODSTAVENI DVOJICE
          POLE_DVOJIC(MAX) = -1
          DVOJICE(1:1) = ACHAR((MAX / 26) +97)
          DVOJICE(2:2) = ACHAR(MOD(MAX, 26) + 97)
          WRITE(11, "(A)") DVOJICE
      END DO
      
      CLOSE(11)
      PAUSE
      END PROGRAM MAIN
      