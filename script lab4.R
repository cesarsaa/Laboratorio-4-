#Kevin Steven García - 1533173
#Diana Arias -
#Cesar Saavedra -
# Laboratorio4

#Regresión
#1 
x<-c(3,7,11,15,18,27,29,30,31,32,33,36,37,38,39,40,42,43,45,47,50)
y<-c(5,11,21,16,16,28,27,35,30,32,32,37,36,38,45,39,40,37,46,49,51)

#1.b
x11()
plot(y ~ x, xlab = "Reducción porcentual del total de sólidos", ylab = "Reducción porcentual de demanda bioquímica de oxigeno", main="Diagrama de dispersión",pch=16)
Regresion<- lm(y ~ x)
abline(Regresion, col = "red")
cor(y,x)
summary(Regresion)

#2.a
x1<-c(10.06,10.32,10.33,10.66,10.86,10.86,10.88,10.91,11.05,11.14,11.20,11.35,11.42,11.64,11.78,11.78,11.83,11.88,11.98,11.98,12,12.09,12.10,12.18,12.23,12.50,12.51,12.53,12.64,12.64,12.7,12.8,12.8,13.01,13.02,13.06,13.16,13.56,13.62,14.02)
y1<-c(52.37,52.85,51.61,66.65,69.39,72.67,74.37,71.73,70.72,81.20,79.26,89.28,97.84,100.37,104.37,103.29,115.19,110.99,118.09,115.47,128.10,127.48,124.67,136.36,137.65,157.08,157.02,152.87,165.23,179.12,176.31,178.82,182.64,199.46,207.17,202.03,215.11,265.30,267.35,327.05)

x11()
plot(y1 ~ x1, xlab = "Desempleo (%)", ylab = "Tasa de homicidios", main="Diagrama de dispersión",pch=16)
Regresion1<- lm(y1 ~ x1)
abline(Regresion1, col = "red")
cor(y1,x1)
summary(Regresion1)
