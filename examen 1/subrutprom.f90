subroutine promedio(datos, n , promedio_tmed) 

        implicit none
        real, dimension(n, 5), intent(in) :: datos
        integer, intent(in) :: n
        real, intent(out) :: promedio_tmed
        integer :: i
        real :: suma

        ! Inicializamos la suma
        suma = 0.0

        ! Sumar los valores de la columna tmed (columna 1)
        do i = 1, n
            suma = suma + datos(i, 1)
        end do

        ! Calcular el promedio
        promedio_tmed = suma / n
        

end subroutine promedio
