PROGRAM mruv
implicit none

!input files:
	open(unit = 10, file = "mruv.inp", status = "old")
!output files
	open(unit = 20, file = "mruv.out", status = "unknow")
	open(unit = 30, file = "t.dat", status = "unknow")	
	open(unit = 40, file = "x.dat", status = "unknow")
	open(unit = 50, file = "v.dat", status = "unknow")

	call read_input
	call propagation
	call save_data

 
END PROGRAM mruv
