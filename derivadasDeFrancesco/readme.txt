compilar: gfortran -o derivadas main.f90 modules/*.f90

graficar: grafico 1: xmgrace \
  resultados/funcion.dat \
  resultados/lagrange_h0.20.dat resultados/lagrange_h0.04.dat \
  resultados/newton_h0.20.dat resultados/newton_h0.04.dat \
  resultados/fourier_h0.20.dat resultados/fourier_h0.04.dat \
  resultados/int_lag_h0.20.dat resultados/int_lag_h0.04.dat \
  resultados/int_new_h0.20.dat resultados/int_new_h0.04.dat \
  resultados/int_fourier_h0.20.dat resultados/int_fourier_h0.04.dat

tiene funcion analitica + interp + integrales

graficar: gafico 2 : xmgrace \
  resultados/der_lag_h0.20.dat resultados/der_lag_h0.04.dat \
  resultados/der_new_h0.20.dat resultados/der_new_h0.04.dat \
  resultados/der_fourier_h0.20.dat resultados/der_fourier_h0.04.dat

tiene derivadas de cada interpolador.

el codigo esta hecho con funcion.dat 
