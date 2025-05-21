program spline_valen
    implicit none

    integer, parameter :: n = 4
    integer :: i, idx, info
    real(8), dimension(n)   :: x, y, h, alpha, diag, RHS, Ccoef, Acoef
    real(8), dimension(n-1) :: subdiag, superdiag, Bcoef, Dcoef
    real(8) :: x_eval, yspline, yexact, error
    character(len=100) :: linea

    external dgtsv

    ! Leer datos desde archivo
    open(unit=10, file='datos.dat', status='old')
    read(10, '(A)') linea  ! Saltar encabezado

    do i = 1, n
        read(10, *) x(i), y(i)
    end do
    close(10)

    ! Calcular diferencias h
    do i = 1, n-1
        h(i) = x(i+1) - x(i)
    end do

    ! Armar lado derecho del sistema (alpha)
    alpha = 0.0d0
    do i = 2, n-1
        alpha(i) = (3.0d0/h(i))*(y(i+1)-y(i)) - (3.0d0/h(i-1))*(y(i)-y(i-1))
    end do

    ! Construir matrices del sistema tridiagonal
    subdiag = 0.0d0
    superdiag = 0.0d0
    diag = 0.0d0

    diag(1) = 1.0d0
    diag(n) = 1.0d0
    alpha(1) = 0.0d0
    alpha(n) = 0.0d0

    do i = 2, n-1
        subdiag(i-1) = h(i-1)
        diag(i)      = 2.0d0 * (x(i+1) - x(i-1))
        superdiag(i) = h(i)
    end do

    RHS = alpha

    ! Resolver sistema tridiagonal con LAPACK
    call dgtsv(n, 1, subdiag, diag, superdiag, RHS, n, info)
    if (info /= 0) then
        print *, "Error al resolver sistema tridiagonal. info =", info
        stop
    end if

    Ccoef = RHS

    ! Calcular coeficientes A, B, D
    do i = 1, n-1
        Acoef(i) = y(i)
        Bcoef(i) = (y(i+1) - y(i))/h(i) - h(i)*(Ccoef(i+1) + 2.0d0*Ccoef(i))/3.0d0
        Dcoef(i) = (Ccoef(i+1) - Ccoef(i)) / (3.0d0 * h(i))
    end do

    ! Evaluar spline en x = 0.25
    x_eval = 0.25d0
    do i = 1, n-1
        if (x_eval >= x(i) .and. x_eval <= x(i+1)) then
            idx = i
            exit
        end if
    end do

    yspline = Acoef(idx) + Bcoef(idx)*(x_eval - x(idx)) + &
              Ccoef(idx)*(x_eval - x(idx))**2 + &
              Dcoef(idx)*(x_eval - x(idx))**3

    yexact = x_eval*dcos(x_eval) - 2.0d0*x_eval**2 + 3.0d0*x_eval - 1.0d0
    error = abs(yexact - yspline)

    ! Mostrar resultados
    print *, "x_eval    = ", x_eval
    print *, "Spline(x) = ", yspline
    print *, "f(x) real = ", yexact
    print *, "Error abs = ", error

end program spline_valen

