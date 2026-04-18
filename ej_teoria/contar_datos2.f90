PROGRAM contar_datos
!!Abre un archivo y cuenta la cantidad de datos cuando tiene un encabezado de un renglón
implicit none
integer :: nd, io
character*20  :: nombre_archivo
print*, 'Ingresar el nombre del archivo de datos'


read(*,*) nombre_archivo
print*,'Se va abrir el archivo: ', nombre_archivo

!Acá cuento cuantos puntos de datos hay en el archivo
nd=0;	io=0

open(10,file=nombre_archivo, status='OLD')
read (10,*)

do while(io >= 0)
	read(10,*,iostat=io)
	 if (io == 0) then
        nd=nd+1
	 endif
enddo

print*,'Hay',nd,'datos'
close(10)

END PROGRAM























