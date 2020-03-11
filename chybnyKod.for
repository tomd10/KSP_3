C     SHODA NALEZENA
 10   IF (DEN1(I) == DEN2(J)) THEN
C         ZACINAME HLEDAT KUPREDU         
          IND1 = I    
          IND2 = J
          START = IND1
C         NALEZENA (DALSI) SHODA         
 30   IF (DEN1(IND1) == DEN2(IND2)) THEN
C             INKREMENTUJE "POSOUVACI" POCITADLA   
              IND1 = IND1 + 1
              IND2 = IND2 + 1
C             KONTROLA PRETECENI (PRETECE-LI, VYHODNOTIME VYSLEDEK)             
              IF (IND1 > POCET_DNU .OR. IND2 > POCET_DNU) THEN
                  IND1 = IND1 - 1
                  IND2 = IND2 - 1
                  GO TO 35
              END IF    
              GO TO 30
      ELSE
C             DALSI SHODA NENALEZENA - POROVNAME SEKVENCI S NEJDELSI
 
C             UKONCENI SEKVENCE - JAKO BYCHOM NIC NENASLI        
              GO TO 80
          END IF
      ELSE
C         INKREMENTACE NEJPRVE DRUHEHO A PAK I HORNIHO UKAZATELE
 80       J = J + 1
          IF (J > POCET_DNU) THEN
              PRINT*, I
              J = 1
              I = I + 1
              IF (I > POCET_DNU) THEN
C                 DOSLI JSME NA KONEC                 
                  GO TO 140
              END IF
          END IF
      END IF
      GO TO 10