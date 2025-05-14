program proyectil
    implicit none
    real :: x0, y0, v0, theta0
    real :: alcance, altura_max
    integer :: i
    real, allocatable :: x(:), y(:), t(:)
    real :: dt, tmax
    integer :: N

    ! Datos de entrada
    print *, "Ingrese x0, y0, v0, theta0 (grados):"
    read *, x0, y0, v0, theta0

    ! Convertir grados a radianes
    theta0 = theta0 * acos(-1.0) / 180.0
program proyectil
    implicit none
    real :: x0, y0, v0, theta0
    real :: x(10000), y(10000), t(10000)
    real :: dt, alcance, alt_max
    integer :: i, N
    real :: pi
    character(len=100) :: filename

    ! Inicializar
    pi = acos(-1.0)
    dt = 0.01
    N = 10000

    ! Pedir datos al usuario
    print *, "Ingrese x0, y0, v0 y ángulo inicial en grados:"
    read(*,*) x0, y0, v0, theta0

    ! Convertir ángulo a radianes
    theta0 = theta0 * pi / 180.0

    ! Inicializar variables
    alcance = 0.0
    alt_max = y0

    ! Abrir archivo para escribir trayectoria
    filename = "trayectoria.dat"
    open(unit=10, file=filename, status="replace", action="write")

    ! Calcular trayectoria
    do i = 1, N
        t(i) = (i - 1) * dt
        call posicion(t(i), x0, y0, v0, theta0, x(i), y(i))

        if (y(i) >= 0.0) then
            write(10, '(F8.3, 1X, F8.3)') x(i), y(i)
            alcance = x(i)
            if (y(i) > alt_max) alt_max = y(i)
        else
            exit  ! El proyectil ya cayó
        end if
    end do

    close(10)

    ! Mostrar resultados
    print *, "Alcance:", alcance
    print *, "Altura máxima:", alt_max
    print *, "Trayectoria escrita en trayectoria.dat"
end program proyectil

