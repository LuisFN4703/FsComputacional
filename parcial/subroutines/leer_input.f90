subroutine leer_input(nf, nc)
implicit none
	integer, intent(out) :: nf, nc

	call system("pwd")
	call readMatrix(trim("input/bahia2011.dat"), nf, nc)
	nf = nf -1
	nc = nc -1

end subroutine leer_input
