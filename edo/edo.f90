program rk4_rlc
        implicit none
        integer, parameter :: dp = kind(1.0d0)
        real(dp) :: t, h, tf
        real(dp) :: y1, y2, k1, k2, k3, k4
        real(dp) :: l1, l2, l3, l4
        integer :: i, n

        ! ctes del problema
        real(dp), parameter :: L = 15.0d0
        real(dp), parameter :: C = 4.2d-6
        real(dp), parameter :: R0 = 200.0d0
        real(dp), parameter :: R1 = 250.0d0
        real(dp), parameter :: V0 = 1000.0d0

        ! paso del tiempo y cond iniciales
        h = 0.02d0
        tf = 0.3d0
        n = int(tf/h)
        t = 0.0d0
        y1 = 0.0d0 ! Q(0)
        y2 = 0.0d0 ! dQ/dt(0)

        open(unit=10, file="resultados.txt")
        write(10,*) "# t     Q(t)          I(t) = dQ/dt"

        do i = 0, n
           write(10,'(f6.3,2X,E12.5,2X,E12.5)') t, y1, y2

           ! Runge-Kutta para sist. de 2 ec.
           k1 = h * y2
           l1 = h * (V0/L - (R0/L)*y2 - (R1/L)*y2**3 - (1.0d0/(L*C))*y1)

           k2 = h * (y2 + l1/2.0d0)
           l2 = h * (V0/l - (R0/L)*(y2 + l1/2.0d0) - (R1/L)*(y2 + l1/2.0d0)**3 - (1.0d0/(L*C))*(y1 + k1/2.0d0))

           k3 = h * (y2 + l2/2.0d0)
           l3 = h * (V0/L - (R0/L)*(y2 + l2/2.0d0) - (R1/L)*(y2+ l2/2.0d0)**3 - (1.0d0/(L*C))*(y1 + k2/2.0d0))

           k4 = h * (y2 + l3)
           l4 = h * (V0/l - (R0/L)*(y2 + l3) - (R1/L)*(y2 + l3)**3 - (1.0d0/(L*C))*(y1 + k3))
           
           y1 = y1 + (k1 + 2.0d0*k2 + 2.0d0*k3 + k4) / 6.0d0
           y2 = y2 + (l1 + 2.0d0*l2 + 2.0d0*l3 + l4) / 6.0d0
           t = t + h
        end do


        close(10)
end program rk4_rlc
