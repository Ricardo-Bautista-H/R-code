Sesión 04 - Tarea
================
Ricardo Bautista

Primero cargamos la serie y creamos el objeto de series de tiempo que se
encuentra en frecuencia mensual desde agosto del 2010 hasta diciembre
del 2015.

``` r
library(readxl)
Series_base <- read_excel(~/Tarea/01-ARCHIVOS UTILIZADOS-DATA/ECONR-AVA-SESION 4-TAREA-1.1-DATA.xlsx")
```

``` r
Series.ts <- ts(Series_base$tleg, start=c(2010,8), end=c(2015, 12), frequency = 12)
```

# Ejercicio 1

Estimamos los dos modelos ARMA que están indicados en el ejercicio

Estiammos modelo ARMA(1,2)

``` r
ARMA12 <- arima(Series.ts, order = c(1,0,2))
ARMA12
```

    ## 
    ## Call:
    ## arima(x = Series.ts, order = c(1, 0, 2))
    ## 
    ## Coefficients:
    ##          ar1      ma1      ma2  intercept
    ##       0.9462  -0.1923  -0.0085     2.2159
    ## s.e.  0.0554   0.1402   0.1238     0.1798
    ## 
    ## sigma^2 estimated as 0.01299:  log likelihood = 48.01,  aic = -86.01

Estiammos modelo ARMA(1,2)

``` r
ARMA10 <- arima(Series.ts, order = c(1,0,0))
ARMA10
```

    ## 
    ## Call:
    ## arima(x = Series.ts, order = c(1, 0, 0))
    ## 
    ## Coefficients:
    ##          ar1  intercept
    ##       0.8970     2.2466
    ## s.e.  0.0638     0.1283
    ## 
    ## sigma^2 estimated as 0.01338:  log likelihood = 47.15,  aic = -88.3

Para elegir el mejor modelo se debe usar el criterio del AIC y BIC menor

``` r
AIC(ARMA12) #-86.01414
```

    ## [1] -86.01414

``` r
AIC(ARMA10) #-88.2964
```

    ## [1] -88.2964

``` r
BIC(ARMA12) #-75.1422
```

    ## [1] -75.1422

``` r
BIC(ARMA10) #-81.77324
```

    ## [1] -81.77324

AIC(ARMA10)&lt;AIC(ARMA12) & BIC(ARMA10)&lt;BIC(ARMA12)

Según los dos criterios el modelo que se ajusta mejor es el ARMA(1,0)

# Ejercicio 2

Estimamos los correlogramas para tener una idea de que orden podría ser
la parte MA y la parte AR del modelo.

Para la parte MA usamos la función de autocorrelación simple para saber
su orden, este será igual al último rezago que muestre significancia
estadística

``` r
acf(Series_base$tleg)
```

![](TAREA_ECONR_AVA_S4_BAUTISTA_HUERTA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
En este caso se sugiere un MA 5

Para la parte AR usamos la función de autocorrelación parcial para saber
su orden, este será igual al último rezago que muestre significancia
estadística

``` r
pacf(Series_base$tleg)
```

![](TAREA_ECONR_AVA_S4_BAUTISTA_HUERTA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
En este caso se sugiere un AR 1

# Ejercicio 3

Se pide estimar predicciones de dos años en adelante, como la serie de
tiempo se encuentra en frecuencia mensual, esto equivale a 24 meses
(n=24)

``` r
predic.ARMA12 <- predict(ARMA12, n.ahead = 24)
predic.ARMA10 <- predict(ARMA10, n.ahead = 24)
```

Pasando en formato a series de tiempo

``` r
Predic1 <- ts(predic.ARMA12$pred, start = c(2016,1) , frequency = 12)
Predic2 <- ts(predic.ARMA10$pred, start = c(2016,1) , frequency = 12)
```

Graficando la serie trabajada junto a las predicciones hechas

``` r
ts.plot(cbind(Series.ts,Predic1), lty =c(1:2))
```

![](TAREA_ECONR_AVA_S4_BAUTISTA_HUERTA_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ts.plot(cbind(Series.ts,Predic2), lty =c(1:2))
```

![](TAREA_ECONR_AVA_S4_BAUTISTA_HUERTA_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->
