program vector
        implicit none
        real, allocatable :: vec(:)
        integer :: i, n, ios
        real :: valor
        character(len=100) :: archivo
        archivo = 'vector.dat'

        ! abrimos el archivo para contar numeros
        open(unit=10, file=archivo, status='old', action= 'read', iostat=ios)
        if (ios /= 0) then
                print *, "error al abrir el archivo"
                stop
        end if

        !contar cuantos numeros hay
        n = 0

        do
                read(10, *, iostat=ios) valor
                if (ios /= 0) exit
                n = n+1
        end do

        rewind(10) ! volver al inicio del archivo

        ! reservar espacio para el vector
        allocate(vec(n))

        ! leer los valores al vector
        do i = 1, n
                read(10, *) vec(i)
        end do
        close(10)

        !revisar elementos pares

        print *, "elementos con parte entera par:"
        do i = 1, n
                if (mod(int(vec(i)), 2) == 0) then
                        print *, "Posicion:", i, "Valor:", vec(i)
                end if 
        end do
end program vector
