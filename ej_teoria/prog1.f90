program Promedio
implicit none
real*8 :: a,b,c,d

print*, ' Lo que hace este programa es calcular el promedio entre a y b'
print*
print*, 'Escribir a:'
read*, a
print*, 'Escribir b:'
read*, b
c = (a+b)/2
print*, 'El promedio entre a y b es:'
print*, c

end program
