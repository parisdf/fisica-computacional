program bisiesto
        implicit none
        integer :: anio

        print *, 'años bisiestos entre 2000 y 2500:'

        do anio = 2000, 2500
         if (mod(anio, 4) == 0) then
                 if (mod(anio, 100) /= 0 .or. mod(anio, 400) == 0) then
                         print *, anio
                 end if
         end if
        end do
end program bisiesto
