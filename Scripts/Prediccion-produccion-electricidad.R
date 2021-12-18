##########       1. LIBRERIAS         #########################################


library(tidyverse)  #collección de las principales bibliotecas 

library(forecast)   #métodos y herramientas para analizar st univariadas

library(tseries)    #análisis de st y finanzas computacionales

library(xts)     #extensiones para manejar st

library(here)       #Para optimizar el manejo de rutas a archivos




#########      2. LECTURA DE DATOS[1]     #####################################


datos <- read.csv(here("Datos","IPG2211A2N.csv"),
                  col.names = c('fecha','produccion'))


#  2.1 Creación de la serie de tiempo     --------------------------------------


produccion <- ts(data = datos$produccion, frequency=12, 
               start=c(1939,1), end=c(2021,8))


#   2.2 Subconjunto de la serie, a partir de 1980     --------------------------


produccion <- window(produccion, start=1980, end=2021)


#########  3. ANÁLISIS EXPLORATORIO    ############################################


# 3.1 Longitud y periodicidad de la serie       --------------------------------


length(produccion)

periodicity(produccion)

# 3.2 ¿Es la Serie Estacionaria?                --------------------------------

                    #   Test Dickey-Fuller Aumentado

#  Ho: La serie no es estacionaria 
#  (con un p-value menor a 0.05 la Ho se rechaza)

adf.test(produccion, k = 12)

# la serie no es estacionaria


# 3.3 Visualización de la ST     -----------------------------------------------

plot(produccion, main= 'Producción de Electricidad y Gas',
            col="dodgerblue3")


# Observamos que la ST tiene Tendencia (T), temporalidad (S) y ruido (R).


# 3.3 Descomposición de la serie      ------------------------------------------

                      #   y_t = T_t + S_t + R_t   #


produccion_decompos <- decompose(produccion)

plot(produccion_decompos, col="dodgerblue3")  # visualizar la descomposición

plot(produccion, main='Producción de Elasicidad y Gas', col="dodgerblue3")
lines(produccion_decompos$trend, col="red", lwd=3) 



# ########## 4. SERIE ESTACIONARIA: COMPONENTE DE RUIDO ##################################


ruido_produccion <- produccion_decompos$random

# borramos los puntos con valores nulos
ruido_produccion <- ts(ruido_produccion[!is.na(ruido_produccion)])  


# 4.1 Prueba de estacionaridad    ----------------------------------------------


#Ho: La serie no es estacionaria (con un p-value menor a 0.05 la Ho se rechaza)

adf.test(ruido_produccion)

# Es estacionaria


# 4.1 Visualización     --------------------------------------------------------

plot(ruido_produccion, main = "Componente Ruido en la ST Producción")

# El ruido ¿se ve como ruido blanco?
ggtsdisplay(ruido_produccion, plot.type = 'partial', main="ruido_produccion") 



# 4.2 Modelo ARMA       -------------------------------------------------------



#      ======  4.2.1 Entrenar el modelo   =====

# Selección de modelo con menor AIC (Akaike Information Criteria)
# https://otexts.com/fpp2/arima.html

c(0, 0, 0)

arma <- auto.arima(ruido_produccion, stationary=TRUE, 
                   seasonal=FALSE,  trace=TRUE )

#     ======  4.2.1  Predecir con  el modelo   =====

pred <- forecast(arma, h=12, level = c(99.5))

plot(pred)
lines(ruido_produccion, col='red') 



## 5. MODELAR LA  PRODUCCCIÓN DE ELECTRICIDAD (ARIMA) #######


      #    ARIMA        Temporalidad
      #  (p, d, q)      (P, D, Q)m
  

# DIFERENCIAR TENDENCIA

diff(produccion, 1)

ggtsdisplay(diff(produccion, 1), plot.type = 'partial', 
            main="Producción 1 diff") 

# DIFERENCIAR TENDENCIA Y TEMPORALIDAD

ggtsdisplay(diff(produccion, 12), plot.type = 'partial', 
            main="Producción 12 diff") 


#      ======  5.1 Entrenar el modelo   =====

# Selección de modelo con menor AIC (Akaike Information Criteria)
# https://otexts.com/fpp2/arima.html

sarima <- auto.arima(produccion, stationary=FALSE, seasonal=TRUE)
sarima

#     ======  5.2  Predecir con  el modelo   =====

produccion_pred <- forecast(sarima, h=12, level = c(80.5))

plot(produccion_pred)
lines(produccion, col='grey') 


# ##### REFERENCIAS ###################################################


# diferencias entre ts y xts 
# https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781788629157/4/ch04lvl1sec35/xts-zoo-or-ts-which-one-to-use

