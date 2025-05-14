subroutine calcular_producto_escalar(x, y, N, resultado)
        
        implicit none
        real, dimension(N) :: x, y
        integer :: N
        real :: resultado
        integer :: i

        resultado = 0.0
        do i = 1, N
                resultado = resultado + x(i) * y(i)
        end do

end subroutine calcular_producto_escalar
