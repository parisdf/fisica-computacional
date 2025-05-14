program examen
    implicit none

    integer :: n
    integer :: i
    integer :: ios
    character(len=100) :: archivo
    character(len=100) :: linea
    character(len=20) :: mes
    character(len=20), allocatable :: meses(:)  ! vector de los meses
    character(len=20) mestempmaxima, mestempminima, meshrelminima
    real :: tmed, tmax, tmin, prec, hrel, promedio_tmed, tempmaxima, tempminima, hrelminima
    real, allocatable :: datos(:,:)  ! Matriz de datos
    real :: m !filtro para minima temp
    real :: t !filtro de temperatura
    real :: tol
    archivo = 'bahia2017.txt'
    
    print*, " ingrese valor filtro para tmin"
    read*, m

    print*, "ingrese el valor de filtro de temperatura"
    read*, t

    tol = 3.0

    ! Llamada a la subrutina para contar filas
    call contar_filas(archivo, n)

    ! uso n encontrado como parametro
    allocate(meses(n), datos(n, 5))

    ! Abrir el archivo
    open(unit=10, file=archivo, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'No se pudo abrir el archivo.'
        stop
    endif

    ! Leer el archivo línea por línea
    do i = 1, n
        read(10, '(A)', iostat=ios) linea
        if (ios /= 0) exit  ! Salir si ya no hay más datos

        ! Leer cada línea y separar el mes y los números
        read(linea, *) mes, tmed, tmax, tmin, prec, hrel

        ! Almacenar el mes y los números
        meses(i) = mes
        datos(i, 1) = tmed
        datos(i, 2) = tmax
        datos(i, 3) = tmin
        datos(i, 4) = prec
        datos(i, 5) = hrel
    end do

    close(10)

    ! Mostrar los resultados
    print *, "Meses y datos leídos del archivo:"
    do i = 1, n
        print *, "Mes: ", meses(i), " Datos: ", datos(i, :)
    end do
    
    !calcular maxtmax y su mes
    call maximat(datos,meses, n, tempmaxima, mestempmaxima)
    print *, "La maxima temperatura registrada entre las maximas es:", tempmaxima," durante el mes de ", mestempmaxima
    

    !calcular mintmin y su mes
    call filtrotempmin(datos, meses, n, m)
    

    !calc promedio
    call promedio(datos, n, promedio_tmed)
    print *, "El promedio de las temperaturas medias durante el año es:", promedio_tmed

    
    !calcular humedad relativa minima y el mes donde ocurre
    call minimahrel(datos, meses, n, hrelminima, meshrelminima)
    print *, "La humedad relativa minima durante el año fue: ", hrelminima, "durante el mes de ", meshrelminima
        
    !estimar cuando un valor de la temperatura es un numero testim
    call estimacion(datos, meses, t, n, tol)
    



       
        
end program examen
