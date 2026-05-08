program prueba_lagrange
    ! use interpolacion ! Descomentá esto si usás tu propio módulo
    implicit none
    integer, parameter :: dp = kind(1.0d0)

    integer :: i, n_datos, ios
    real(dp), allocatable :: x_datos(:), y_datos(:)
    real(dp) :: x_prueba, y_prueba

    ! =================================================================
    ! 1. LECTURA DEL ARCHIVO DE DATOS
    ! =================================================================
    ! Primero contamos cuántas líneas (puntos) tiene el archivo
    n_datos = 0
    open(unit=10, file='input/funcion.dat', status='old', iostat=ios)
    if (ios /= 0) then
        print *, "Error: No se pudo abrir 'funcion.dat'."
        stop
    end if
    
    do
        read(10, *, iostat=ios) ! Lee una línea sin guardar los datos
        if (ios /= 0) exit      ! Sale del bucle al llegar al final del archivo
        n_datos = n_datos + 1
    end do
    rewind(10) ! Volvemos el puntero al inicio del archivo

    ! Asignamos memoria a los vectores dinámicos
    allocate(x_datos(n_datos), y_datos(n_datos))

    ! Leemos los pares (x, y)
    do i = 1, n_datos
        read(10, *) x_datos(i), y_datos(i)
    end do
    close(10)

    print *, "Lectura exitosa: se leyeron", n_datos, "puntos."

    ! =================================================================
    ! 2. PRUEBA DE INTERPOLACIÓN
    ! =================================================================
    ! Elegimos un punto x_prueba que esté justo en el medio del primer 
    ! y segundo punto de los datos originales.
    x_prueba = x_datos(1) + (x_datos(2) - x_datos(1)) / 2.0_dp
    
    ! Llamamos a la subrutina de Lagrange
    call calcular_lagrange(n_datos, x_datos, y_datos, x_prueba, y_prueba)

    print *, "--- Resultados de la Prueba ---"
    print *, "x evaluado:     ", x_prueba
    print *, "y interpolado:  ", y_prueba

    ! Liberamos la memoria
    deallocate(x_datos, y_datos)

contains

    ! =================================================================
    ! SUBRUTINA DE LAGRANGE (Algoritmo estándar)
    ! =================================================================
    subroutine calcular_lagrange(n, x, y, xint, yint)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n), y(n)
        real(dp), intent(in) :: xint
        real(dp), intent(out) :: yint
        
        integer :: i, j
        real(dp) :: L_i, suma
        
        suma = 0.0_dp
        
        do i = 1, n
            L_i = 1.0_dp
            do j = 1, n
                if (i /= j) then
                    ! Se construye el polinomio base Li(x)
                    L_i = L_i * (xint - x(j)) / (x(i) - x(j))
                end if
            end do
            ! Se suma la contribución del punto i
            suma = suma + y(i) * L_i
        end do
        
        yint = suma
    end subroutine calcular_lagrange

end program prueba_lagrange
