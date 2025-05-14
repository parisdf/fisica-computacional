program producto_escalar
        
        implicit none
        integer :: N
        real, allocatable :: x(:), y(:)
        real :: resultado_calculado, resultado_intrinseco

        !sol tamaño vector
        print *, "introduce el tamaño del vector N:"
        read *, N

        ! asignar memoria a los vectores
        allocate(x(N), y(N))

        !ingresar los elementos de los vectores
        
        print*, "introduce los elem del vector x:"
        read *, x
        print *, "introduce los elementos del vector y:"
        read *, y
        
        ! llamar a subrutina para calcular el producto

        call calcular_producto_escalar(x, y, N, resultado_calculado)

        !calcular el producto escalar usando la funcion intrinseca

        resultado_intrinseco = dot_product(x,y)

        !mostrar los resultados

        print *, "producto escalar calculado con la subrutina: ", resultado_calculado
        print *, "producto escalar calculado con dot_product: ", resultado_intrinseco

        !ver si los resultados coinciden

        if (resultado_calculado == resultado_intrinseco) then
                print *, "los resultados coinciden"
        else
                print *, "los resultados no ociniden"
        end if

end program producto_escalar

