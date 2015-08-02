################### TRABAJO MODELOS LINEALES #####################
############# DANIEL FREIRE #################

# 2.1 Leer los archivos y analizar las dimensiones

library(readxl)

poblacion1 <- read_excel("poblacion1.xlsx",sheet = 1,col_names = TRUE,na = "")
poblacion2 <- read_excel("poblacion2.xlsx",sheet = 1,col_names = TRUE,na = "")
dim(poblacion1)
dim(poblacion2)
# Analizamos las datas leidas.
str(poblacion1)
str(poblacion2)
View(poblacion1)
View(poblacion2)
names(poblacion1)
names(poblacion2)
summary(poblacion1)
summary(poblacion2)

#2.2 unir las dos datas

poblacion <- merge(x = poblacion1,y = poblacion2,by="identificador", suffixes = c("","")) 

names(poblacion)
View(poblacion)
summary(poblacion)

#2.3 idetificar la clase de cada variable y graficas
# Codigo j sera nuestra columna que cumpla la condicon

for(j in 2:dim(poblacion)[2])
  
{
  if(is.numeric(poblacion[,j])==TRUE)
    
  {
    
    boxplot(poblacion[,j], xlab = "x", ylab="y", main="Diagrama cajas ",
            col="black", border="red")
  }
  else 
  {
    
    barplot(table(poblacion[,j]),xlab = "x", ylab="y", main="Diagrama de barras",
            col="black", border="red")
  }
}

# str(poblacion) 

# 2.4 variables categoricas (frecuencia) y numericas (minimo ,media, maximo,desviacion estandar, primer cuartil  )

for(j in 2:dim(poblacion)[2])
{
  if(is.numeric(poblacion[,j])==TRUE)
    
  {
    
    print(names(poblacion)[j])
    
    print(min(poblacion[,j]))
    print(mean(poblacion[,j]))
    print(max(poblacion[,j]))
    print(sd(poblacion[,j], na.rm = FALSE))
    print(quantile(poblacion[,j],probs=seq(0,1,0.25),na.rm = FALSE))
  }
  else 
  {
    
    print(table(poblacion[,j]))
    print(names(poblacion)[j])
  }
  
}

#### Comprabamos algunos calculos
# variables numericas
#summary(poblacion) 
#sd(poblacion$poblacion, na.rm = FALSE)
#sd(poblacion$var.pobl.mayor, na.rm = FALSE)
#sd(poblacion$menores.18, na.rm = FALSE)
#sd(poblacion$part.almz.escl, na.rm = FALSE)
#sd(poblacion$var.ingresos, na.rm = FALSE)
#sd(poblacion$tasa.crimen, na.rm = FALSE)
#sd(poblacion$var.tasa.crimen, na.rm = FALSE)
#primer cuartil
#quantile(poblacion$poblacion, probs = seq(0, 1, 0.25), na.rm = FALSE)
#quantile(poblacion$var.poblacion.mayor, probs = seq(0, 1, 0.25), na.rm = FALSE)
#quantile(poblacion$menores.18, probs = seq(0, 1, 0.25), na.rm = FALSE)
#quantile(poblacion$part.almz.escl, probs = seq(0, 1, 0.25), na.rm = FALSE)
#quantile(poblacion$var.ingresos, probs = seq(0, 1, 0.25), na.rm = FALSE)
#quantile(poblacion$tasa.crimen, probs = seq(0, 1, 0.25), na.rm = FALSE)
#quantile(poblacion$var.tasa.crimen, probs = seq(0, 1, 0.25), na.rm = FALSE)

### variables categoricas frecuencias
#frecregion<-poblacion$region!="A"
#table(frecregion) # 23 A , 17 B

#frecserv<-poblacion$serv.bas.compl!="SI"
#table(frecserv)  # 16 SI , 24 NO


# 2.5    Correlacion

# Podemos generar un codigo para calcular la correlacion o calcular una por una
# haremos de las dos formas y asi podremos comprobar nuestros resultados.

# codigo

for(j in 3:dim(poblacion)[2])
{
  
  if(is.numeric(poblacion[,j])==TRUE)
    
  {
    print(cor(x=poblacion[,"poblacion"],y=poblacion[,j]))
    
  }
}

# forma alternativa

cor(x = poblacion$poblacion, y= poblacion$var.pobl.mayor)
cor(x = poblacion$poblacion, y= poblacion$menores.18)
cor(x = poblacion$poblacion, y= poblacion$part.almz.escl)
cor(x = poblacion$poblacion, y= poblacion$var.ingresos)
cor(x = poblacion$poblacion, y= poblacion$tasa.crimen)
cor(x = poblacion$poblacion, y= poblacion$var.tasa.crimen)
cor(x = poblacion$poblacion, y= poblacion$poblacion)


# 2.6  test t la media de la poblacion 

x<-subset(poblacion, subset=serv.bas.compl == 'SI') #poblacion en el grupo ser.bas.coml:SI
x1<-x[,-c(1,3,4,5,6,7,8,9,10)] # data eliminamos las variables categorica
View(x)
View(x1)

y<-subset(poblacion, subset=serv.bas.compl == 'NO') #poblacion en el grupo ser.bas.coml:NO
y1<-y[,-c(1,3,4,5,6,7,8,9,10)]
View(y)
View(y1)

(testt<-t.test(x1, y1, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE,conf.level = 0.90))
# se concluye que las medias no son iguales


# 2.7 regresion lineal multiple

reg<- lm(poblacion ~ var.pobl.mayor + menores.18  + tasa.crimen+part.almz.escl+var.ingresos+var.tasa.crimen, poblacion)
summary(reg) 
# realizamos la regresion con los regresores significativos
reg1<-step(reg,direction="backward")
summary(reg1)

# primero notamos que solo tasa.crimen es el regresor mas significativo, ademas que el 
# coeficiente esta cerca de cero pero no llega a ser cero, ademas vemos que la probabilidad
# es menor que 0.05 al igual que la probabilidad del intercepto.
# si se aumenta en una unidad la tasa de crimen entonces la poblacion disminuye 
# en promedio 0.012469

# 2.8  interpretacion de Rˆ2
summary(reg1)["r.squared"]
# La variabilidad de la regresion es del 14.056%

# 2.9 significancia  y parametros individuales
anova <- aov(reg1)
summary(anova)[[1]]

#la regresion es significativa puesto que el p-valor es < 0.05
#ahora analisisamos la siginificancia de los parametros individuales
#vemos que para el intercepto 11.1147 > qt(0.975 , df =nrow(poblacion)-2) es decir  
# se rechaza Ho, mientras e para la variable tasa.crimen -2.493< df =nrow(poblacion)-2) ,
# es decir que no se rechaza Ho.


# 2.10 analisis de los residuos


# graficos residuales
library(ggplot2)
(u_t <- reg1$residuals)
hist(u_t)

g <- ggplot(data = poblacion, aes(x=poblacion, y=u_t))
g + geom_point()



g <- ggplot(data = poblacion, aes(x=tasa.crimen, y=u_t))
g + geom_point()


#media de los residuos
mean(reg1$residuals) 

# graf normal
qqnorm(u_t)
qqline(u_t)

# Pronosticos vs residuos
(y_t <- reg1$fitted.values)
plot(u_t,y_t)

# puntos influyentes
# Cook's plot
# D > 2/k
cutoff <- 2/((nrow(poblacion)-length(reg1$coefficients))) 
plot(reg1, which=4, cook.levels=cutoff)
poblacion[40,]

# Puntos singulares
library(car)
outlierTest(reg1) # Bonferonni p-value for most extreme obs
qqPlot(reg1, main="QQ Plot") #qq plot for studentized residuos 







