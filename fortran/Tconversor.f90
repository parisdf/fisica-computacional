program tconversor

        implicit none
        real :: temp_in, temp_out
        character(len=1) :: tipo

        !solicitar tipo de conversion

        print *, "ingrese 'C' si tiene una temperatura en Celsius o 'F' si esta en fahrenheit"
        read *, tipo

        !convertir a mayuscula si es necesario
        tipo = adjustl(tipo)
        tipo = achar(iachar(tipo) - 32 * merge(1, 0, tipo >= 'a' .and. tipo <= 'z'))

        if (tipo == 'C') then
                print *, "ingrese la temperatura en Celsius:"
                read *, temp_in
                call celsius_a_fahrenheit(temp_in, temp_out)
                print *, "Temperatura en Fahrenheit:", temp_out
        else if (tipo == 'F') then
                print *, "Ingrese la temperatura en Fahrenheit:"
                read *, temp_in
                call fahrenheit_a_celsius(temp_in, temp_out)
                print *, "Temperatura en Celsius:", temp_out
        else
                print *, "Entrada no válida. Use 'C' o 'F'."
        end if

end program tconversor
