      PROGRAM if2case

c     This program contains a DO-loop and writes messages according
c     to the loop index.  The exercise is to rewrite the conditional
c     statements as a single CASE construct.

      INTEGER :: ii

      DO ii=15,-7,-1
c
c        IF (ii==0) THEN
c          WRITE(*,*) " The index ",ii," is zero."
c        ELSE IF (ii<0) THEN
c          WRITE(*,*) " The index ",ii," is negative."
c        ELSE IF ( ii==1 .OR. ii==2 .OR. ii==3 .OR. ii==5 .OR. 
c     &            ii==7 .OR. ii==11 .OR. ii==13 ) THEN
c          WRITE(*,*) " The index ",ii," is a positive prime."
c        ELSE IF ( ii >= 10 ) THEN
c          WRITE(*,*) " The index ",ii,
c     &               " is a factorable positive greater than 9."
c        ELSE
c          WRITE(*,*) " The index ",ii,
c     &               " is a factorable positive less than 10."
c        ENDIF

        SELECT CASE (ii)
        CASE (0)
          WRITE(*,*) " The index ",ii," is zero."
        CASE (:-1)
          WRITE(*,*) " The index ",ii," is negative."
        CASE (1:3,5,7,11,13)
          WRITE(*,*) " The index ",ii," is a positive prime."
        CASE (10,12,14:)
          WRITE(*,*) " The index ",ii,
     &                " is a factorable positive greater than 9."
        CASE DEFAULT
          WRITE(*,*) " The index ",ii,
     &               " is a factorable positive less than 10."

        END SELECT           
      ENDDO


      END PROGRAM if2case
