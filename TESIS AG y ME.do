gen trimestres= tq(2000Q1)+_n-1 
format %tq trimestres 
tsset trimestres 

**** ESTACIONARIEDAD ****

dfuller tasa_pib
** Es estacionaria 
dfuller ingresos_petroleros
** No es estacionaria 
dfuller gasto
** No es estacionaria 
dfuller precio_wti
** No es estacionaria 
dfuller consumo_hogares
** No es estacionaria 
dfuller inversion
** No es estacionaria 
dfuller tasa
** Es estacionaria 

**** GENERAR LOG ****

gen log_ingresos_petroleros = log(ingresos_petroleros)
gen log_gasto = log(gasto)
gen log_consumo_hogares = log(consumo_hogares)
gen log_inversion = log(inversion)
gen log_precio_wti = log(precio_wti)

**** GRÁFICOS ****

twoway (tsline tasa_pib, yaxis (1))
twoway (tsline log_ingresos_petroleros, yaxis (1))
twoway (tsline log_gasto, yaxis (1))
twoway (tsline log_consumo_hogares, yaxis (1))
twoway (tsline log_inversion, yaxis (1))
twoway (tsline tasa, yaxis (1))
twoway (tsline log_precio_wti, yaxis (1))

**** AUTOCORRELACIÓN ****

ac tasa_pib, name(ac1)
ac log_ingresos_petroleros, name(ac2)
ac log_gasto, name(ac3)
ac log_consumo_hogares, name(ac4)
ac log_inversion, name(ac5)
ac log_precio_wti, name(ac6)
ac tasa, name(ac7)
graph combine ac1 ac2 ac3 ac4 ac5 ac6 ac7

**** ESTACIONARIEDAD ****

dfuller tasa_pib
** Es estacionario 
dfuller log_ingresos_petroleros
** No es estacionario 
dfuller log_gasto
** Es estacionario 
dfuller log_consumo_hogares
** No es estacionario 
dfuller log_inversion
** No es estacionario 
dfuller dtasa
** Es estacionario 
dfuller log_precio_wti
** No es estacionario 

**** FILTRO HP **** 
*La primera variable es el ciclo, la segunda es tendencia

ssc install hprescott
hprescott log_ingresos_petroleros, stub(HP) smooth (1600)
hprescott log_gasto, stub(HP) smooth (1600)
hprescott log_consumo_hogares, stub(HP) smooth (1600)
hprescott log_inversion, stub(HP) smooth (1600)
hprescott log_precio_wti, stub(HP) smooth (1600)


**** GRÁFICOS ****

ac HP_log_ingresos_petroleros_1, recast(line)
dfuller HP_log_ingresos_petroleros_1 
** Es estacionaria
ac HP_log_gasto_1, recast(line)
dfuller HP_log_gasto_1 
** Es estacionaria
ac HP_log_consumo_hogares_1, recast(line)
dfuller HP_log_consumo_hogares_1 
** Es estacionaria
ac HP_log_inversion_1, recast(line)
dfuller HP_log_inversion_1 
** Es estacionaria
ac HP_log_precio_wti_1, recast(line)
dfuller HP_log_precio_wti_1 
** Es estacionaria

**** RENOMBRAR VARIABLES ****

rename HP_log_ingresos_petroleros_1 ingresos_petroleros_2
rename HP_log_gasto_1 gasto_2
rename HP_log_consumo_hogares_1 consumo_hogares_2
rename HP_log_inversion_1 inversion_2
rename HP_log_precio_wti_1 precio_wti_2

**** NÚMERO DE REZAGOS ****

varsoc tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa , exog(precio_wti_2)

*** TEST DE COINTEGRACIÓN ****

vecrank tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa , trend(none) max lags(1)
vecrank tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa , trend(none) max lags(4)
ssc install egranger
egranger tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa 

**** MODELO VAR ****

** VAR 1 REZAGO **

var tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa, lags(1) exog(precio_wti_2)

* TEST DE WALD CONJUNTO 

varwle
** Hay autocorrelación en los residuos del modelo 

* AUTOCORRELACIÓN 

 varlmar
 
* NORMALIDAD

varnorm, jbera skewness kurtosis

* CAUSALIDAD

vargranger

* ESTABILIDAD DEL MODELO 

varstable, graph


** VAR 4 REZAGOS **

var tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa, lags(1/4) exog(precio_wti_2)


* TEST DE WALD CONJUNTO 
varwle

* AUTOCORRELACIÓN 

 varlmar
 
* NORMALIDAD

varnorm, jbera skewness kurtosis

* CAUSALIDAD

vargranger

* ESTABILIDAD DEL MODELO 

varstable, graph

******************************************************************************************************

*** FUNCIONES IMPULSO RESPUESTA ****

** VAR 1 REZAGO **

var tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa, lags(1) exog(precio_wti_2)
irf create MODELO, replace  step(13) set(MODELO)

* RESPUESTA DEL INGRESO PETROLERO 

irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(tasa_pib)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(gasto_2)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(consumo_hogares_2)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(inversion_2)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(tasa)

** VAR 4 REZAGOS **

var tasa_pib ingresos_petroleros_2 gasto_2 consumo_hogares_2 inversion_2 tasa, lags(1/4) exog(precio_wti_2)
irf create MODELO, replace  step(13) set(MODELO)

* RESPUESTA DEL INGRESO PETROLERO 

irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(tasa_pib)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(gasto_2)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(consumo_hogares_2)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(inversion_2)
irf graph irf, irf(MODELO) impulse(ingresos_petroleros_2) response(tasa)

*** DESCOMPOSICIÓN DE LA VARIANZA ****

irf table sfevd, noci
irf graph sfevd, irf(MODELO) impulse(ingresos_petroleros_2) response(tasa_pib)


******************************************************************************************************

*** ESTACIONARIEDAD ****

dfuller tasa_pib

dfuller log_ingresos_petroleros
gen d_log_ingresos_petroleros = log_ingresos_petroleros-l4.log_ingresos_petroleros
dfuller d_log_ingresos_petroleros

dfuller log_gasto
gen d_log_gasto = log_gasto-l4.log_gasto
dfuller d_log_gasto

dfuller log_consumo_hogares
gen d_log_consumo_hogares = log_consumo_hogares-l4.log_consumo_hogares
dfuller d_log_consumo_hogares

dfuller log_inversion
gen d_log_inversion = log_inversion-l4.log_inversion
dfuller d_log_inversion

dfuller log_precio_wti
gen d_log_precio_wti = log_precio_wti-l4.log_precio_wti
dfuller d_log_precio_wti

dfuller tasa

**** RENOMBRAR VARIABLES ****

rename  d_log_ingresos_petroleros ingresos_petroleros_3
rename  d_log_gasto gasto_3
rename  d_log_consumo_hogares consumo_hogares_3
rename  d_log_inversion inversion_3
rename  d_log_precio_wti precio_wti_3

**** NÚMERO DE REZAGOS ****

varsoc tasa_pib ingresos_petroleros_3 gasto_3 consumo_hogares_3 inversion_3 tasa , exog(precio_wti_3)

*** TEST DE COINTEGRACIÓN ****

vecrank tasa_pib ingresos_petroleros_3 gasto_3 consumo_hogares_3 inversion_3 tasa , trend(none) max lags(1)
vecrank tasa_pib ingresos_petroleros_3 gasto_3 consumo_hogares_3 inversion_3 tasa , trend(none) max lags(4)
ssc install egranger
egranger tasa_pib ingresos_petroleros_3 gasto_3 consumo_hogares_3 inversion_3 tasa 

** VAR 1 REZAGO **

var tasa_pib ingresos_petroleros_3 gasto_3 consumo_hogares_3 inversion_3 tasa, lags(1) exog(precio_wti_3)

* TEST DE WALD CONJUNTO 

varwle
** Hay autocorrelación en los residuos del modelo 

* AUTOCORRELACIÓN 

 varlmar
 
* NORMALIDAD

varnorm, jbera skewness kurtosis

* CAUSALIDAD

vargranger

* ESTABILIDAD DEL MODELO 

varstable, graph

**** FORECAST ****
*Predicción para los siguientes 13 trimestes

fcast compute v, step(13) dynamic(q(2023q4)) bsp

** GRÁFICOS **

fcast graph vtasa_pib
fcast graph vingresos_petroleros_3
fcast graph vgasto_3
fcast graph vconsumo_hogares_3
fcast graph vinversion_3
fcast graph vtasa

**** FORECAST IN SAMPLE ****
*Predicción para los anteriores 13 trimestes

fcast compute o, step(12) dynamic(q(2020q4)) bsp 

** GRÁFICOS **

fcast graph otasa_pib
fcast graph oingresos_petroleros_3
fcast graph ogasto_3
fcast graph oconsumo_hogares_3
fcast graph oinversion_3
fcast graph otasa


**** GRÁFICOS DE RESULTADOS ****
*Comparación forecast in sample con datos reales
tsline tasa_pib otasa_pib vtasa_pib 
tsline ingresos_petroleros_3 oingresos_petroleros_3  vingresos_petroleros_3
tsline gasto_3 ogasto_3 vgasto_3
tsline consumo_hogares_3 oconsumo_hogares_3 vconsumo_hogares_3
tsline inversion_3 oinversion_3 vinversion_3
tsline tasa otasa vtasa

*** VAR 4 REZAGOS ****

var tasa_pib ingresos_petroleros_3 gasto_3 consumo_hogares_3 inversion_3 tasa, lags(1/4) exog(precio_wti_3)

* TEST DE WALD CONJUNTO 
varwle

* AUTOCORRELACIÓN 

 varlmar
 
* NORMALIDAD

varnorm, jbera skewness kurtosis

* CAUSALIDAD

vargranger

* ESTABILIDAD DEL MODELO 

varstable, graph

**** FORECAST ****
*Predicción para los siguientes 13 trimestes

fcast compute l, step(13) dynamic(q(2023q4)) bsp

** GRÁFICOS **

fcast graph ltasa_pib
fcast graph lingresos_petroleros_3
fcast graph lgasto_3
fcast graph lconsumo_hogares_3
fcast graph linversion_3
fcast graph ltasa

**** FORECAST IN SAMPLE ****
*Predicción para los anteriores 13 trimestes

fcast compute e, step(12) dynamic(q(2020q4)) bsp

** GRÁFICOS **

fcast graph etasa_pib
fcast graph eingresos_petroleros_3
fcast graph egasto_3
fcast graph econsumo_hogares_3
fcast graph einversion_3
fcast graph etasa

**** GRÁFICOS DE RESULTADOS ****
*Comparación forecast in sample con datos reales

gen start_quarter = tq(2020q4)  
gen end_quarter = tq(2026q4) 
keep if trimestres >= start_quarter & trimestres <= end_quarter

tsline ltasa_pib tasa_pib etasa_pib 
tsline lingresos_petroleros_2 ingresos_petroleros_2 eingresos_petroleros_2 
tsline lgasto_2 log_gasto_2 elog_gasto_2 
tsline lconsumo_hogares_2 consumo_hogares_2 econsumo_hogares_2 
tsline linversion_2 inversion_2 einversion_2
tsline ltasa tasa ftasa


*Comparación forecast in sample con 1 y 4 rezagos 

tsline tasa_pib etasa_pib otasa_pib 
tsline ingresos_petroleros_3 eingresos_petroleros_3 oingresos_petroleros_3
tsline gasto_3 egasto_3 ogasto_3 
tsline consumo_hogares_3 econsumo_hogares_3 oconsumo_hogares_3 
tsline inversion_3 einversion_3 oinversion_3 
tsline tasa etasa otasa

*Se escoge el modelo var con 4 rezagos
tsline tasa_pib ltasa_pib etasa_pib  
tsline ingresos_petroleros_3 lingresos_petroleros_3 eingresos_petroleros_3
tsline gasto_3 lgasto_3 egasto_3
tsline consumo_hogares_3 lconsumo_hogares_3 econsumo_hogares_3
tsline inversion_3 linversion_3 einversion_3
tsline tasa ltasa etasa



