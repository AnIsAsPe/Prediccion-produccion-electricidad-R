library(forecast)
library(tseries)

######### 1. SIMULACION DE DATOS A EN REGRESIÓN A PARTIR DE UNA RECTA  #########
 
                     ###   y = B0 + B1x + error   ###

n = 500  # datos a generar

# 1.1 Simulación de errores gausianos -----------------------------------------

sigma = 3
error <- rnorm(n, mean=0, sd = sigma)
hist(error, col='dodgerblue')

# 1.2 Definición de la ecuación de la recta ------------------------------------

B0 = 2       #  b  (ordenada al origen)
B1 = 3       #  m (pendiente)

# 1.3 Simulación de las variables x & y ----------------------------------------
x = seq(-1, 1, length.out = n)
y <- B1 * x + B0 + error

# 1.4 Visualización la recta y los datos generados -----------------------------
plot(x, y, pch=16, col="dodgerblue") + abline(a=B0, b=B1, col="red", lwd=2)

# 1.5. EJERCICIO. Distintas rectas y errores -----------------------------------

genera_puntos_regresion <- function(B1, B0, sigma, n){
  error <- rnorm(n, mean=0, sd = sigma)
  x = seq(-1, 1, length.out = n)
  y <- B1*x + B0 + error
  
  par(mfrow= c(1,2))
  plot(x, y, pch=16, col="dodgerblue",  
       main = paste("y = ",B0," + ", B1, "x + error")) + abline(a=B0, b=B1, col="red", lwd=2)
  hist(error, col='dodgerblue')
  par(mfrow= c(1,1))
}

# Utiliza la función anterior para simular puntos en regresión a partir 
# de distintas rectas: (1) recta sin pendiente, (2)con pendiente negativa, 
# (3) con pendiente positiva, (4) con errores grandes, (5)con errores pequeños.

genera_puntos_regresion(B0 = 6, B1=4, sigma=2, n=500) 

#################### 2. SIMULACION DE RUIDOS BLANCOS  ########################

#    - media constante
#    - varianza constante
#    - no tiene autocorrelaciones significativas

# 2.1 Proceso iid --------------------------------------------------------------

n <- 1000
set.seed(7)
iid <- ts(rnorm(n, mean=0, sd = 4))

ggtsdisplay(iid,  main='Ruido Blanco iid',
            plot.type = 'partial') 


# 2.2 Ejemplos de otros ruidos blancos   ----------------------------------------


# 1) Sea (Xt) un proceso i.i.d. e Y una variable aleatoria de Bernoulli con
#    parámetro p = 0.5 . Entonces la variable aleatoria Zt = XtY - Xt (Y - 1)^2
#    es un ruido blanco.


set.seed(5)
X1 <- rnorm(n)
Y1 <- rbinom(n, size = 1 , prob=0.5)
Z1 <- X1*Y1 - X1*(Y1-1)^2


ggtsdisplay(Z1, main='Ruido Blanco Z1', plot.type = 'partial') 

# 2) Sea (Xt ) un proceso i.i.d. el proceso estocástico definido por
#    Zt = Xt, cuando t es par y 
#    Zt  =  (1/sqrt(2)) * (X^2-1), cuando t es impar

set.seed(43)
X2 = rnorm(n/2)
Z2 = rep(0, n )  #serie de 1000 ceros

idx_par = seq(2, length(Z2),2)
idx_impar = seq(3,length(Z2),2)

Z2[1] <- rnorm(1)
Z2[idx_par] <- X2
Z2[idx_impar] <- (1/sqrt(2)) * (X2[1:499]^2-1)


ggtsdisplay(Z2, main='Ruido Blanco Z2', plot.type = 'partial') 



########### 3.MODELO AUTOREGRESIVO (AR) (parametro p)  #######################

# 3.1. AR(1)  -----------------------------------------------------------------

#usaremos la función arima.sim, para lo que debemos especificar el modelo primero

coef = -0.9
AR <- list(order = c(1, 0, 0), ar = coef)  #ar es el valor del coeficiente 
#(entre -1 y 1)

set.seed(3)
AR1 <- arima.sim(n = 50, model = AR)

ggtsdisplay(AR1, main=paste('AR1 coef =', coef), plot.type = 'partial') 

# 3.2. Modeloa AR con creciente orden p ----------------------------------------

set.seed(123)

AR_coef <- c(0.7,0.2, -0.1, -0.3)  #fi_1, fi_2, fi_3, fi4

AR_mods <- list()  #lista vacía que contendrá los modelos

AR_coef[1:4]
for (p in 1:4) {
  AR_mods[[p]] <- arima.sim(n = 10000, list(ar = AR_coef[1:p]))
}

#Visualización de los 4 modelos 

par(mfrow = c(4, 3),# número de gráficas (3 por cada modelo)
    mar = c(1,1,1,1)) # disminuir márgenes 

for (p in 1:4) {
  plot.ts(AR_mods[[p]][1:50])
  acf(AR_mods[[p]], lag.max = 12)
  pacf(AR_mods[[p]], lag.max = 12)
}

########### 4.MODELO MEDIA MOVIL (MA) (parametro q)  #######################

# 4.1 MA(1) --------------------------------------------------------------------
MA <- list(order = c(0, 0, 2), ma =  c(0.7, 0.2))  #ma es el valor del coeficiente 
#(entre -1 y 1)

set.seed(7)
MA1 <- arima.sim(n = 1000, model = MA)

ggtsdisplay(MA1, main='MA1', plot.type = 'partial') 

# 4.2. Modeloa MA con creciente orden q -----------------------------------------

set.seed(123)

MA_coef <- c(0.7, 0.8, -0.9, -0.5)

MA_mods <- list()

for (q in 1:4) {
  
  MA_mods[[q]] <- arima.sim(n = 1000, list(ma = MA_coef[1:q]))
}

# Visualización 

par(mfrow = c(4, 3))

for (q in 1:4) {
  plot.ts(MA_mods[[q]][1:50])
  acf(MA_mods[[q]], lag.max = 12)
  pacf(MA_mods[[q]], lag.max = 12)
}


#################  5.MODELO ARMA (parametro q y q)  #######################


set.seed(123)

ARMA22 <- list(order = c(2, 0, 2),
               ar = c(-0.4, 0.2),
               ma = c(0.4, 0.2))

## media 
mu <- 5

## simulación del proceso arma(2,2) 
ARMA_sim <- arima.sim(n = 1000, model = ARMA22) + mu

# También podemos encontrar los parametros a partir de la serie simulada

arima(x=ARMA_sim, order=c(2, 0 ,2))
