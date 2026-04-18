SUBROUTINE readMatrix(nombre, nrow, ncol) 
IMPLICIT NONE  
	CHARACTER(LEN=*), INTENT(IN):: nombre
	INTEGER, INTENT(OUT):: nrow
	INTEGER, INTENT(OUT):: ncol


	CALL contarfilas_nativo(nombre, nrow)	
	CALL contarcolumnas_nativo(nombre, ncol)

	!PRINT*, " > nrow =", nrow
	!PRINT*, " > ncol =", ncol

RETURN
END SUBROUTINE readMatrix
