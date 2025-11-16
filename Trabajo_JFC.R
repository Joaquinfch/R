# NombreApellido_Trabajo2.R Joaquín Fernández Cheetham
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
install.packages("vioplot")
library(ggplot2)
library(vioplot)
datos<- read.csv("datos_biomed.csv")
# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
head(datos)
summary(datos)
dim(datos)
str(datos)
ncol(datos)
#hay 5 variables
nrow(datos)
#hay 100 muestras y 3 tratamientos
# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
ggplot(datos, aes(x=Tratamiento, y=Glucosa))+
geom_boxplot()+
ggtitle("Boxplot de glucosa por tratamiento")
# 4. Realiza un violin plot (investiga qué es). (1 pt)

#Un violin plot es un gráfico que combina un diagrama de caja con un grafico de densidad de probabilidad saliendo para cada lado
ggplot(datos, aes(x=Tratamiento, y=Colesterol))+
geom_violin()+
ggtitle("Violin plot de colesterol por tratamiento")


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
ggplot(datos, aes(x=Glucosa, y=Presion, color=Tratamiento))+
geom_point()+
theme(legend.position="bottom")+
ggtitle("Dispersion de glucosa vs Presion") 

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)

#Un facet grid es una matriz de graficos creada a partir de un conjunto de datos. Los distintos paneles muestran diferentes versiones del mismo grafico
ggplot(datos, aes(x=Colesterol, y=Presion))+
geom_point()+
facet_grid(.~ Tratamiento)+
ggtitle("Colesterol vs Presión por tratamiento")

# 7. Realiza un histogramas para cada variable. (0.5 pts)
names(datos)
ggplot(datos,aes(x=Glucosa))+
geom_histogram(bins=10, fill="red", color="white")+
ggtitle("Histograma de Glucosa")

ggplot(datos, aes(x=Presion))+
geom_histogram(bins=10, fill="blue", color="white")+
ggtitle("Histograma de Presion")

ggplot(datos, aes(x=Colesterol))+
geom_histogram(bins=10, fill="yellow", color="white")+
ggtitle("Histograma de Colesterol")

# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
datos$Tratamiento <- as.factor(datos$Tratamiento)
class(datos$Tratamiento)
levels(datos$Tratamiento)

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data=datos, mean)
media_glucosa
sd_glucosa <- aggregate(Glucosa ~ Tratamiento, data=datos, sd)
sd_glucosa

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
placebo<- subset(datos, Tratamiento== "Placebo")
farmacoA<- subset(datos, Tratamiento== "FarmacoA")
farmacoB<- subset(datos, Tratamiento== "FarmacoB")
head(placebo)
head(farmacoA)
head(farmacoB)

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
shapiro.test(placebo$Glucosa)
shapiro.test(farmacoA$Glucosa)
shapiro.test(farmacoB$Glucosa)

t.test(placebo$Glucosa, farmacoA$Glucosa)
t.test(placebo$Glucosa, farmacoB$Glucosa)
t.test(farmacoA$Glucosa, farmacoB$Glucosa)

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
modelo_anova<-aov(Glucosa ~ Tratamiento, data=datos)
summary(modelo_anova)
TukeyHSD(modelo_anova)

