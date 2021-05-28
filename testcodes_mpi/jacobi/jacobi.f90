PROGRAM arrayProg
  IMPLICIT NONE


  !rank is 2, but size not known
  REAL, DIMENSION (6,6):: a
  REAL, DIMENSION (6):: b,x0,x

  INTEGER :: n,max_iter,iter
  INTEGER :: i, j,k
  REAL :: tol,sum,max_error,error
  max_iter=100
  tol=1.0E-06
  n=6
  !Assign values to A,b,x0
  OPEN(unit=10,file="A_matrix.dat")
  OPEN(unit=15,file="b_vector.dat")
  OPEN(unit=20,file="initial_vector.dat")
  DO i=1,n
     READ(10,*) (A(i,j),j=1,n)
     ! PRINT *,a(i,1:n);
     READ(15,*) b(i)
     ! PRINT *,b(i);
     READ(20,*) x0(i)
     PRINT *,x0(i);
  END DO
  CLOSE(10)
  CLOSE(15)
  CLOSE(20)
  !Execute main loop

  DO k = 1, max_iter
     max_error=0
     DO i=1,n
        sum=0
        DO j=1,n
           IF (j .NE. i) THEN
              sum=sum+a(i,j)*x0(j)
           ENDIF
        END DO
        x(i)=(b(i)-sum)/a(i,i)
        error=ABS(x(i)-x0(i))
        PRINT *,x(i);
        IF ( error .GT. max_error ) THEN
           max_error=error
           !
        END IF
        x0(i)=x(i)
     END DO

     PRINT *,"maximum error for this iteration",max_error;
     IF ( max_error .LT. tol ) EXIT
  END DO
  iter=k
  WRITE(*,*),"last_iter=",iter
  IF ( max_error .GT. tol ) THEN
     WRITE(*,*),"not converged yet"
  END IF
  ! if ( iter==max_iter ) then
  !     print*, "the system failed to converge"
  ! end if





END PROGRAM arrayProg
