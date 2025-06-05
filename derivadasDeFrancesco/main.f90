program practico_fsc
  use lector
  use interpolacion
  use fourier
  use derivada
  use integracion
  use utilidades
  implicit none

  integer, parameter :: nmax = 100000
  real(8), dimension(nmax) :: x, y, xi
  real(8), dimension(nmax) :: yilag, yinew, yifourier
  real(8), dimension(nmax) :: dy_lag, dy_new, dy_fourier
  real(8), dimension(nmax) :: int_lag, int_new, int_fourier
  integer :: n, np, i, k
  real(8) :: h, xmax, xmin
  character(len=60) :: archivo
  character(len=5) :: hs

  ! -------------------------
  ! Leer datos analíticos
  ! -------------------------
  call leer_datos("data/funcion.dat", x, y, n)
  xmin = x(1)
  xmax = x(n)

  ! Guardar función original para graficar
  open(1, file="resultados/funcion.dat", status="replace")
  do i = 1, n
    write(1,*) x(i), y(i)
  end do
  close(1)

  ! -------------------------
  ! Para h = 0.2 y h = 0.04
  ! -------------------------
  do k = 1, 2
        if (k == 1) then
                h = 0.2
        else
                h = 0.04
        end if
 
    write(hs, '(F4.2)') h
    np = int((xmax - xmin) / h) + 1
    do i = 1, np
      xi(i) = xmin + (i - 1) * h
    end do

    ! -------------------------
    ! Punto 1: Interpolaciones
    ! -------------------------
    do i = 1, np
      yilag(i) = lagrange(x, y, n, xi(i))
      yinew(i) = newton(x, y, n, xi(i))
    end do
    call interpolar_fourier(x, y, n, xi, yifourier, np)

    ! Guardar interpolaciones
    call guardar("resultados/lagrange_h"//trim(hs)//".dat", xi, yilag, np)
    call guardar("resultados/newton_h"//trim(hs)//".dat", xi, yinew, np)
    call guardar("resultados/fourier_h"//trim(hs)//".dat", xi, yifourier, np)

    ! -------------------------
    ! Punto 3: Derivadas
    ! -------------------------
    dy_lag     = derivada_centrada(xi, yilag, np, h)
    dy_new     = derivada_centrada(xi, yinew, np, h)
    dy_fourier = derivada_centrada(xi, yifourier, np, h)

    call guardar("resultados/der_lag_h"//trim(hs)//".dat", xi, dy_lag, np)
    call guardar("resultados/der_new_h"//trim(hs)//".dat", xi, dy_new, np)
    call guardar("resultados/der_fourier_h"//trim(hs)//".dat", xi, dy_fourier, np)

    ! -------------------------
    ! Punto 4: Integración de derivadas
    ! -------------------------
    int_lag     = trapecio_acumulado(xi, dy_lag, np)
    int_new     = trapecio_acumulado(xi, dy_new, np)
    int_fourier = trapecio_acumulado(xi, dy_fourier, np)

    call guardar("resultados/int_lag_h"//trim(hs)//".dat", xi, int_lag, np)
    call guardar("resultados/int_new_h"//trim(hs)//".dat", xi, int_new, np)
    call guardar("resultados/int_fourier_h"//trim(hs)//".dat", xi, int_fourier, np)

    ! -------------------------
    ! Punto 2: Error relativo (sobre Lagrange como ejemplo)
    ! -------------------------
    open(10, file="resultados/error_lagrange_h"//trim(hs)//".dat", status="replace")
    do i = 1, np
      if (abs(y(i)) > 1e-12) then
        write(10,*) xi(i), abs((y(i) - yilag(i)) / y(i))
      else
        write(10,*) xi(i), 0.0
      end if
    end do
    close(10)
  end do

  print *, "Archivos generados correctamente en /resultados/"
end program practico_fsc


