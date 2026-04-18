module mod_meses
!este es un modulo que define un vector cuyas componentes son los meses del
!año, por ejemplo meses(1) = Enero
implicit none
character(len=10), dimension(12), parameter :: meses = &
        ["Enero     ", "Febrero   ", "Marzo     ", &
        "Abril     ", "Mayo      ", "Junio     ", &
         "Julio     ", "Agosto    ", "Septiembre", &
         "Octubre   ", "Noviembre ", "Diciembre "]

end module mod_meses
