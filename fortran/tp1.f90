program temp
        implicit none
        
        integer, parameter :: n = 7
        character(len=15) :: dia
        character(len=15), dimension(n) :: dias
        real :: Tmin(n), Tmax(n)
        real :: prom_min, prom_max
        integer :: i
        
        open(unit=10, file='datos.dat', status='old', action='read')
        
        ! leer los datos
        
        do i = 1, n
                read(10, *) dia, Tmin(i), Tmax(i)
                dias(i) = trim (dia)
        end do

        close(10)

        ! imprimir en pantalla

        do i = 1, n
                print *, trim(dias(i)) // ':', Tmin(i), Tmax(i)
        end do

        !calcular prom

        prom_min = sum(Tmin) / n
        prom_max = sum(Tmax) / n

        !escribir prom en un archivo

        open(unit=20, file= 'promedios.dat', status='replace', action='write')
        write(20, *) 'Promedio minimo: ', prom_min
        write(20, *) 'Promedio minimo: ', prom_max
        close(20)
end program temp
