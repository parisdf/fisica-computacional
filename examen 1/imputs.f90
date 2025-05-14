subroutine contar_filas(archivo, n)
        implicit none
        character(len=100), intent(in) :: archivo
        integer, intent(out) :: n
        integer :: ios
        character(len=100) :: linea
        integer :: count

        ! Inicializamos el contador
        count = 0

       
        open(unit=10, file=archivo, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'No se pudo abrir el archivo.'
            stop
        endif
        ! cuento filas
        do
            read(10, '(A)', iostat=ios) linea
            if (ios /= 0) exit  
            count = count + 1  
        end do

        ! Asignamos el número de filas a n
        n = count

        close(10)
    end subroutine contar_filas


