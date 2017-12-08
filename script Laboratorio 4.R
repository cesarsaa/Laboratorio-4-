#Kevin Steven Garcia - 1533173
#Diana Carolina Arias -
#Cesar Andres Saavedra - 1628466
#Laboratorio 4

#Prueba de Hipotesis
#2
#Datos
xbarra <- 12000
M2  <- 13200
N2  <- 40
d.e. <- 1320
#Estadistico de Prueba
zp= ((xbarra-M2)/(d.e./sqrt(N2)))
#Contraste del Estadistico de Prueba
qnorm(0.05)
#Valor-P
vp = pnorm(-5.7495)

#7
#Datos
Ciudad<-c('Cali', 'Medellin', 'Pereira', 'Manizales', 'Cucuta', 'Barranquilla', 
          'Arauca', 'Popayan', 'Ibague', 'Neiva', 'Zipaquira', 'Tunja', 'Valledupar', 'Santa Marta')
DFEB <- c(0.15, 0.12, 0.145, 0.12, 0.138, 0.143, 0.13, 0.142, 0.12, 0.12, 0.115, 0.12, 0.135, 0.14)
DMAYO <- c(0.11, 0.08, 0.118, 0.12, 0.134, 0.148, 0.13, 0.15, 0.135, 0.09, 0.105, 0.09, 0.112, 0.13)
Diferencia= DFEB - DMAYO
#Tabla de Datos:
Tablap7<- data.frame(Ciudad,DFEB,DMAYO,Diferencia)
#Tama??o de la muestra
np7<-14
#Media de la Diferencia
Mediap7<-mean(Diferencia)*100
#Desviaci??n Est??ndar
Desvp7<-sd(Diferencia)*100
###Cuantil de la T-Student
Cuantilp7<-qt(0.975, 13)
#Intervalo de Confianza para la media de observaciones relacionadas
Lim_Inf <- Mediap7-Cuantilp7*(Desvp7/sqrt(np7))
Lim_Sup <- Mediap7+Cuantilp7*(Desvp7/sqrt(np7))

ICp7 <- c(Lim_Inf,Lim_Sup)
print(ICp7)
#Prueba de Hipotesis
M7<-0.02
N7<-14
t.test(Diferencia, mu = 0.02, alternative = 'less', conf.level = 0.95)
#Estadistico de Prueba
TP<-((Mediap7-M7)/(Desvp7/sqrt(N7)))
#Cuantil de la T-Student
qt(0.95,13)
#Prueba de Normalidad Shapiro-Wilk
diferencia.test <- shapiro.test(Diferencia)
print(diferencia.test)
x <- Diferencia
plotn <- function(x,main="Histograma de frecuencias y distribuci??n normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  
  hist(x,freq=F, main=main, xlab=xlab, ylab=ylab, col="grey" )
  curve(dnorm(x,media,dt), min, max, add = T, col="blue")
}
plotn(x, main="Histograma de Frecuencias para la diferencia y Curva de normalidad")

#Regresion
#1 
x<-c(3,7,11,15,18,27,29,30,31,32,33,36,37,38,39,40,42,43,45,47,50)
y<-c(5,11,21,16,16,28,27,35,30,32,32,37,36,38,45,39,40,37,46,49,51)

#1.b
x11()
plot(y ~ x, xlab = "Reducci?n porcentual del total de s?lidos", ylab = "Reducci?n porcentual de demanda bioqu?mica de oxigeno", main="Diagrama de dispersi?n",pch=16)
Regresion<- lm(y ~ x)
abline(Regresion, col = "red")
cor(y,x)
summary(Regresion)

#2.a
x1<-c(10.06,10.32,10.33,10.66,10.86,10.86,10.88,10.91,11.05,11.14,11.20,11.35,11.42,11.64,11.78,11.78,11.83,11.88,11.98,11.98,12,12.09,12.10,12.18,12.23,12.50,12.51,12.53,12.64,12.64,12.7,12.8,12.8,13.01,13.02,13.06,13.16,13.56,13.62,14.02)
y1<-c(52.37,52.85,51.61,66.65,69.39,72.67,74.37,71.73,70.72,81.20,79.26,89.28,97.84,100.37,104.37,103.29,115.19,110.99,118.09,115.47,128.10,127.48,124.67,136.36,137.65,157.08,157.02,152.87,165.23,179.12,176.31,178.82,182.64,199.46,207.17,202.03,215.11,265.30,267.35,327.05)

x11()
plot(y1 ~ x1, xlab = "Desempleo (%)", ylab = "Tasa de homicidios", main="Diagrama de dispersi?n",pch=16)
Regresion1<- lm(y1 ~ x1)
abline(Regresion1, col = "red")
cor(y1,x1)
summary(Regresion1)
