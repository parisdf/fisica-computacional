program spline_cubico_DeFrancesco

        implicit none
        integer, parameter :: n = 4
        real(8), dimension(n) :: x, y, h, alpha, l, mu, z, c, b, d, a
        integer :: i, punto
        real(8) :: x_interp, y_interp, y_real, error
        character(len=100) :: linea
        character(len=20) :: dummy

        !leer datos desde archivo
        open(unit=10, file='datos.dat', status='old')
        read(10, '(A)') linea  !salto primer renglon del archivo


        do i = 1, n
                read(10, *) x(i), y(i)
        end do
        close(10)

        ! calculo h y alpha
        do i = 1, n-1
                h(i) = x(i+1) -x(i)
        end do

        do i = 2, n-1
                alpha(i) = (3.0d0/h(i))*(y(i+1)-y(i)) - (3.0d0/h(i-1))*(y(i)-y(i-1))
        end do

        !tridiagonal

        l(i) = 1.0d0
        mu(i) = 0.0d0
        z(1) = 0.0d0

        do i = 2, n-1
                l(i) = 2.0d0*(x(i+1)-x(i-1)) - h(i-1)*mu(i-1)
                mu(i) = h(i)/l(i)
                z(i) = (alpha(i) - h(i-1)*z(i-1)) / l(i)
        end do

        l(n) = 1.0d0
        z(n) = 0.0d0
        c(n) = 0.0d0


        ! sustitucion

        do i = n-1, 1, -1
                c(i) = z(i) - mu(i)*c(i+1)
                b(i) = (y(i+1)-y(i))/h(i) - h(i)*(c(i+1) + 2.0d0*c(i))/3.0d0
                d(i) = (c(i+1) - c(i)) / (3.0d0*h(i))
                a(i) = y(i)
        end do

        ! interpolacion en x=0.25

        x_interp = 0.25d0

        ! encontrar intervalo

        do i = 1, n-1
                if (x_interp >= x(i) .and. x_interp <= x(i+1)) then
                        punto = i
                        exit
                end if
        end do

        y_interp = a(punto) + b(punto)*(x_interp - x(punto)) + &
                c(punto)*(x_interp - x(punto))**2 + &
                d(punto)*(x_interp - x(punto))**3

        y_real = x_interp*dcos(x_interp) - 2.0d0*x_interp**2 + 3.0d0*x_interp -1.0d0
        error = abs(y_real - y_interp)

        ! Resultados
        
        print *, "x = ", x_interp
        print *, "spline(x) = ", y_interp
        print *, "real f(x) = ", y_real
        print *, "error abs. = ", error

end program spline_cubico_DeFrancesco
   
