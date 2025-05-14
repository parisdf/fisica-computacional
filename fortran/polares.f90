program convertir_polares
        implicit none
        integer :: i, N
        real, allocatable :: x(:), y(:), r(:), theta(:)
        character(len=100) :: fname_x, fname_y
        fname_x = "x.dat"
        fname_y = "y.dat"

        ! primero, contar cuantos datos hay
        open(unit=10, file=fname_x, status= 'old')
        N = 0
        do
                read(10, *, iostat=i)
                if (i /= 0) exit
                N = N + 1
        end do
        close(10)

        !reservar espacio
        allocate(x(N), y(N), r(N), theta(N))
       
        !leer datos
        open(unit=10, file=fname_x, status='old')
        open(unit=11, file=fname_y, status='old')
        do i = 1, N
                read(10, *) x(i)
                read(11, *) y(i)
        end do
        close(10)
        close(11)

        !convertir a coordenadas polares
        do i = 1, N
                call cartesianas_a_polares(x(i), y(i), r(i), theta(i))
        end do

        !imprimir resultados
        open(unit=12, file="polares.dat", status="replace")
        do i = 1, N
                write(12, '(F10.4, F10.4)') r(i), theta(i)
        end do
        close(12)

        print *, "conversion completada. resultados en polares.dat"

end program convertir_polares
