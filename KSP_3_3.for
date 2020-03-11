C     PROGRAM NA RESENI KSP Z3 ULOHA 2
C     AUTOR: TOMAS CHABADA P4
      
C     POMOCNE METODY
      MODULE METODY
      CONTAINS

C     ROZDELENI STRINGU NA POLE CISEL
      SUBROUTINE SPLITSTRING(VSTUPSTRING, VYSTUPPOLE)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT) :: VSTUPSTRING
      INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: VYSTUPPOLE
      
C     POMOCNE PROMENNE
      CHARACTER(LEN=:), ALLOCATABLE :: VSTUPSTRING_
      INTEGER :: POCETMEZER
      INTEGER :: DELKAVSTUPU 
      INTEGER :: I
      INTEGER :: INDEXMEZERY
      INTEGER :: PORADISLOVA
      
C     INICIALIZACE PROMENNYCH     
      POCETMEZER = 0
      INDEXMEZERY = 1
      PORADISLOVA = 1

      VSTUPSTRING_ = TRIM(VSTUPSTRING)
      DELKAVSTUPU = LEN(VSTUPSTRING_)

C     VYPOCET POCTU MEZER
      DO I = 1,DELKAVSTUPU
          IF (VSTUPSTRING_(I:I) == " ") THEN
              POCETMEZER = POCETMEZER + 1
          END IF
      END DO
      POCETMEZER = POCETMEZER + 1

      ALLOCATE(VYSTUPPOLE(POCETMEZER))

C     PROCHAZENI STRINGU + HLEDANI MEZERY
      DO I = 1, DELKAVSTUPU
          IF(VSTUPSTRING_(I:I) == " ") THEN
              READ(VSTUPSTRING_(INDEXMEZERY:I), *) 
     &        VYSTUPPOLE(PORADISLOVA)
              INDEXMEZERY = I
              PORADISLOVA = PORADISLOVA +1
          END IF
      END DO
      READ(VSTUPSTRING_(INDEXMEZERY:DELKAVSTUPU), *)
     &VYSTUPPOLE(PORADISLOVA)
      
      END SUBROUTINE SPLITSTRING
      END MODULE METODY
      
C     HLAVNI PROGRAM
      PROGRAM MAIN
      USE METODY
      IMPLICIT NONE

C     DEKLARACE PROMENNYCH
      INTEGER :: POCET_DNU
      CHARACTER(LEN=1000000) :: DEN1S, DEN2S
      INTEGER, DIMENSION(:), ALLOCATABLE :: DEN1, DEN2
 
      INTEGER :: I, J
      INTEGER :: IND1, IND2
      INTEGER :: MAX1, MAX2
      INTEGER :: START
      
C     INICIALIZACE PROMENNYCH
      I = 1
      J = 1
      MAX1 = 0
      MAX2 = 0
      
C     CTENI VSTUPU
      OPEN(10, FILE="vstup.txt", STATUS="OLD")
      READ(10, *) POCET_DNU
      READ(10, "(A)") DEN1S
      READ(10, "(A)") DEN2S
      CLOSE(10)
      
      CALL SPLITSTRING(DEN1S, DEN1)
      CALL SPLITSTRING(DEN2S, DEN2)
 
C     HLEDANI NEJDELSIHO SPOJITEHO USEKU
C     POMOCI "POSOUVANI" OBOU POLI
!DIR$ PARALLEL ALWAYS
      DO I=1, POCET_DNU
          J = 1
          DO WHILE( J <= POCET_DNU )
              IF (DEN1(I) == DEN2(J)) THEN
C             ZACINAME HLEDAT KUPREDU         
                  IND1 = I    
                  IND2 = J
                  START = IND1
                  
                  DO WHILE(DEN1(IND1) == DEN2(IND2))
C                 INKREMENTUJE "POSOUVACI" POCITADLA   
                      IND1 = IND1 + 1
                      IND2 = IND2 + 1
C                     KONTROLA PRETECENI (PRETECE-LI, VYHODNOTIME VYSLEDEK)             
                      IF (IND1 > POCET_DNU .OR. IND2 > POCET_DNU) THEN
                          IND1 = IND1 - 1
                          IND2 = IND2 - 1
                          EXIT
                      END IF
                  END DO
C                 VYBER MAXIMA
                  IF (ABS(MAX2-MAX1) < ABS(START-IND1+1)) THEN
                      MAX1 = START
                      MAX2 = IND1 -1
                  END IF
                  J = IND2
          END IF
      J = J + 1
      END DO
      PRINT*, I
      END DO
 
 

      
C     VYPIS VYSTUPU     
140   OPEN(11, FILE="vystup.txt", STATUS="REPLACE")
      WRITE(11, "(I0)") ABS(MAX1-MAX2)+1
      DO I=MAX1, MAX2
          WRITE(11,"(I0, 1X)", ADVANCE="NO") DEN1(I)
      END DO
      CLOSE(11)
      PAUSE
      END PROGRAM MAIN