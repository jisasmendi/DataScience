#Instalar paquetes necesarios
install.packages("readxl")
install.packages("stringr")
install.packages('openxlsx')
install.packages("magrittr")
install.packages("dplyr")
install.packages("scales")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("sf")
install.packages("feasts")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("fable")
install.packages("fabletools")
install.packages("tsibbledata")
install.packages("urca")
install.packages("caret")

#Abrimos las librerías
library("readxl")
library(stringr)
library("openxlsx")
library(magrittr)
library(dplyr)
library(scales)
library(ggplot2)
library(lubridate)
library(sf)
library("feasts")
library(tsibble)
library("reshape2")
library("fable")
library(readxl)
library(tidyverse)
library(urca)
library(caret)

#Leemos los datos de cada año
datos2018=read_excel("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/dataset_boston_incidents/Datos_2018.xlsx")
datos2019=read_excel("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/dataset_boston_incidents/Datos_2019.xlsx")
datos2020=read_excel("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/dataset_boston_incidents/Datos_2020.xlsx")
datos2021=read_excel("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/dataset_boston_incidents/Datos_2021.xlsx")
datos2022=read_excel("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/dataset_boston_incidents/Datos_2022.xlsx")

#Quitamos columnas que no son de nuestro interés
datos2018x=datos2018[,c(-3,-4,-6,-9,-10,-11,-12,-13,-17)]
datos2019x=datos2019[,c(-3,-4,-6,-9,-10,-11,-12,-13,-17)]
datos2020x=datos2020[,c(-3,-4,-6,-9,-10,-11,-12,-13,-17)]
datos2021x=datos2021[,c(-3,-4,-6,-9,-10,-11,-12,-13,-17)]
datos2022x=datos2022[,c(-3,-4,-6,-9,-10,-11,-12,-13,-17)]

#Eliminamos aquellos outliers que no tienen número de incidente
datos2018y=datos2018x[!is.na(datos2018x$INCIDENT_NUMBER),]
datos2019y=datos2019x[!is.na(datos2019x$INCIDENT_NUMBER),]
datos2020y=datos2020x[!is.na(datos2020x$INCIDENT_NUMBER),]
datos2021y=datos2021x[!is.na(datos2021x$INCIDENT_NUMBER),]
datos2022y=datos2022x[!is.na(datos2022x$INCIDENT_NUMBER),]

#Juntamos los datos de los diferentes anos
DatosTotal = merge(datos2018y,datos2019y, all = TRUE)
DatosTotal = merge(DatosTotal,datos2020y, all = TRUE)
DatosTotal = merge(DatosTotal,datos2021y, all = TRUE)
DatosTotal = merge(DatosTotal,datos2022y, all = TRUE)

#Cambiamos nombre de los atributos al español
names(DatosTotal)<- c("Número Incidente","Código Delito","Distrito","Disparo","Fecha","Calle","Latitud","Longitud")

#Contamos datos vacíos por atributo
apply (DatosTotal, MARGIN = 2, function(x) sum (is.na(x)))


#Cambimos los códigos de distrito a sus nombres propios
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"A15","Charlestown")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"A1","Downtown")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"A7","East Boston")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"B2","Roxbury")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"B3","Mattapan")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"C6","South Boston")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"C11","Dorchester")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"D4","South End")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"D14","Brighton")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"E5","West Roxbury")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"E13","Jamaica Plain")
DatosTotal$Distrito<- str_replace(DatosTotal$Distrito,"E18","Hyde Park")

#Unificamos información respecto a los disparos. Si = Hubo disparos, No = No hubo disparos
DatosTotal$Disparo<- str_replace(DatosTotal$Disparo,"Y","Si")
DatosTotal$Disparo<- str_replace(DatosTotal$Disparo,"0","No")
DatosTotal$Disparo<- str_replace(DatosTotal$Disparo,"1","Si")
DatosTotal$Disparo<- str_replace_na(DatosTotal$Disparo,)
DatosTotal$Disparo<- str_replace(DatosTotal$Disparo,"NA","No")

#Corroboramos cambio en cantidad de datos vacíos por atributo
apply (DatosTotal, MARGIN = 2, function(x) sum (is.na(x)))


#Creamos archivo excel de la base datos
setwd("C:/Users/polic/Desktop/2023 - 1S/DATA SCIENCE")
write.xlsx(DatosTotal,"Datos Total2.xlsx")


#Agregamos a la base de datos la columna día de la semana
DatosTotal <- DatosTotal %>% mutate("Día de la semana" = weekdays(as.Date(DatosTotal$Fecha)))

#Creamos tabla de cantidad de incidentes por día de la semana
TablaIncidentesDia<- table(DatosTotal$`Día de la semana`)
TablaIncidentesDia


TablaIncidentesDia <- data.frame(TablaIncidentesDia)

#Cambiamos nombres de columnas a tabla
colnames(TablaIncidentesDia) <- c("Día de la Semana", "Número de Incidentes")

#Agregamos porcentajes por día, redondeando al segundo decimal
TablaIncidentesDia$porcentaje <- round(TablaIncidentesDia$`Número de Incidentes` / sum(TablaIncidentesDia$`Número de Incidentes`) * 100,2)
TablaIncidentesDia

#Promedio de incidentes por día
# Definir el rango de tiempo
fecha_inicio <- as.Date("2018-01-01")
fecha_fin <- as.Date("2022-12-30")

# Generar secuencia de fechas dentro del rango
fechas <- seq(fecha_inicio, fecha_fin, by = "day")

#Promedio incidentes
promediolunes = TablaIncidentesDia[3,2]/ sum(weekdays(fechas) == "lunes")
promediomartes = TablaIncidentesDia[4,2]/ sum(weekdays(fechas) == "martes")
promediomiercoles = TablaIncidentesDia[5,2]/ sum(weekdays(fechas) == "miércoles")
promediojueves = TablaIncidentesDia[2,2]/ sum(weekdays(fechas) == "jueves")
promedioviernes = TablaIncidentesDia[7,2]/ sum(weekdays(fechas) == "viernes")
promediosabado = TablaIncidentesDia[6,2]/ sum(weekdays(fechas) == "sábado")
promediodomingo = TablaIncidentesDia[1,2]/ sum(weekdays(fechas) == "domingo")
cantidad_dias <- c(promediolunes,promediomartes,promediomiercoles,promediojueves,promedioviernes,promediosabado,promediodomingo)
promediodiario <- data.frame(table(cantidad_dias))
promediodiario$dia <- c("Domingo","Sábado", "Martes", "Lunes", "Jueves", "Miércoles", "Viernes")
promediodiario<-promediodiario[,-2]
promediodiario$dia <- factor(promediodiario$dia, levels = c("Lunes","Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"))
promediodiario <- arrange(promediodiario, dia)
colnames(promediodiario) <- c("Promedio diario", "Día")
promediodiario


#Creamos gráfico de barras
grafico <- ggplot(TablaIncidentesDia, aes(x = TablaIncidentesDia$`Día de la Semana`, y = `Número de Incidentes`, fill = `Día de la Semana`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5) +
  geom_text(aes(label = TablaIncidentesDia$`Número de Incidentes`), position = position_stack(vjust = 0.5)) +
  labs(x = "Categoría", y = "Valor", title = "Incidentes por día de la semana 01/01/2018 al 30/12/2022")
grafico

#Creamos tabla de cantidad de incidentes por distrito
ofensasDistrito<-data.frame(table(DatosTotal$Distrito))
delitospordistrito <- arrange(ofensasDistrito, desc(Freq))
colnames(delitospordistrito) <- c("Distrito", "Cantidad delitos")
delitospordistrito

#Hacemos gráfico de torta de delitos por distrito
valores <- c(54201,48736,47575,40991,39051,28726,25807,21849,21287,17738,16336,7850,462)
labels <- c("Roxbury","Dorchester","South End","Mattapan","Downtown","South Boston","Brighton","Hyde Park","Jamaica Plain","West Roxbury","East Boston","Charlestown","External")

# Crea el gráfico de torta con ggplot2
df <- data.frame(labels, valores)
porcentaje <- round(valores / sum(valores) * 100,2) 
p <- ggplot(df, aes(x = "", y = valores, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void()

#Agregamos porcentaje y título a gráfico
p <- p + geom_text(aes(label = paste0(porcentaje, "%")), position = position_stack(vjust = 0.5))+labs(title = "Distribución de delitos por distrito (01/01/2018 al 30/12/2022)")+ labs(fill = "Distritos")
print(p)

#Creamos tabla de cantidad de incidentes por mes
ofensasMes<-data.frame(table(month(DatosTotal$Fecha)))
delitospormes <- arrange(ofensasMes,)
colnames(delitospormes) <- c("Mes", "Cantidad delitos")
delitospormes

#Gráfico de delitos por mes/año
DatosTotal$Fecha <- as.POSIXct(DatosTotal$Fecha)
nuevos_datos <- DatosTotal %>%
  mutate(año = lubridate::year(DatosTotal$Fecha),
         mes = lubridate::month(DatosTotal$Fecha)) %>%
  group_by(año, mes) %>%
  summarize(total_objetos = n())
ggplot(nuevos_datos, aes(x = mes, y = total_objetos, fill = factor(año))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Mes", y = "Total de objetos", title = "Delitos mensuales por año (01/01/2018 al 30/12/2022)", fill = "Año") +
  scale_fill_discrete(name = "Año") +
  scale_x_continuous(breaks = 1:12, labels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                               "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")) +
  theme_minimal()
nuevos_datos

#Gráfico de delitos respecto a disparos
barplot(table(DatosTotal$Disparo),horiz=TRUE,col = c("blue", "red"), xlab= "Existencia de disparo" , ylab="Cantidad de Incidentes", main= "Gráfico de Incidentes con disparos (01/01/2018 al 30/12/2022)")
mtext("No = 368.343", side = 3, line = 0.7, cex = 0.8)
mtext("Si = 3.672", side = 3, line = 0.01, cex = 0.8)
contardisparos = table(DatosTotal$Disparo)
contardisparos

#Gráfica frecuencia por código de delito
# Obtener los cinco datos mayores
tipoOrdenado <- sort(table(DatosTotal$`Código Delito`), decreasing = TRUE)
tipoOrdenado <- tipoOrdenado[1:5]
barplot(tipoOrdenado,horiz=FALSE,  col = "green", ylab= "Frecuencia" , xlab="Código de ofensa", main = "Cantidad de los 5 delitos más frecuentes (01/01/2018 al 30/12/2022)")

#Tabla ordenada de delitos por código de delito
ofensas<-data.frame(table(DatosTotal$`Código Delito`))
ofensas
datos_ordenados <- arrange(ofensas[1:5], desc(Freq))
datos_ordenados <- datos_ordenados[1:5,]
colnames(datos_ordenados) <- c("Código de Delito", "Frecuencia")
datos_ordenados


#leer datos shp de mapa
DistritosGeo <- st_read("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/Police_Districts.shp")

#Frecuencia de aparición de distritos en la base de datos
Frecuencia.distritos <- DatosTotal %>%
  count(Distrito)
colnames(Frecuencia.distritos)[2] <- "Frecuencia"
colnames(DatosTotal)

#cambiar nombres distritos
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"A15","Charlestown")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"A1","Downtown")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"A7","East Boston")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"B2","Roxbury")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"B3","Mattapan")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"C6","South Boston")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"C11","Dorchester")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"D4","South End")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"D14","Brighton")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"E5","West Roxbury")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"E13","Jamaica Plain")
DistritosGeo$DISTRICT<- str_replace(DistritosGeo$DISTRICT,"E18","Hyde Park")

#cambiar nombre columna DISTRICT por Distrito
#Acceder a los nombres de las columnas actuales
nombres_columnas <- colnames(DistritosGeo)

#Cambiar el nombre de la segunda columna
nombres_columnas[2] <- "Distrito"

#Asignar los nuevos nombres a las columnas de la tabla
colnames(DistritosGeo) <- nombres_columnas

#combinar datos geograficos y frecuencia
datos_combinados <- left_join(DistritosGeo, Frecuencia.distritos, by = "Distrito")

#armar grafico
ggplot() +
  geom_sf(data = datos_combinados, aes(fill = Frecuencia)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Frecuencia de Hechos Delictuales en Boston") +
  theme_minimal()

#Modelo series de tiempo


datosDistrito0=read_excel("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/Delitos_x_mes_x_distrito.xlsx")

datosDistrito <- select(datosDistrito0, Fecha, Downtown, Charlestown, Dorchester, Roxbury) %>% dplyr::mutate(Fecha=tsibble::yearmonth(Fecha))

datostibble <- as_tibble(datosDistrito,index ="Fecha")

mtibble <- reshape2::melt (datostibble,"Fecha")

mtibble <- as_tsibble(mtibble, index = "Fecha",key = "variable")

#Modelo para cada variable
#se usa modelador automático
modelo1 <- mtibble %>%
  model(auto.ets = ETS(value))
modelo1
report(modelo1)

#Pronóstico usando los modelos 
#se supone que lo hace para todas las variables a la vez
pronostico1<-modelo1 %>%
  forecast(h="1 year") #1 año es lo que queremos predecir 
pronostico1

pronostico1 %>%
  autoplot(mtibble, level=95, alpha=0.5) #level y alpha hace que sea más abierto, no tan justo


#Intervalos de confianza
pronostico1 %>%
  hilo(level=c(80,95))

accuracy(modelo1)

#Múltiples modelos, múltiples variables
modelos2<- mtibble %>%
  model(arima=ARIMA(value), ets=ETS(value), snaive=SNAIVE(value))

modelos2

#Capacidad predictiva
accuracy(modelos2) #devuelve los mismos valores para cada variable

#Pronósticos usando los 4 modelos
pronostico2<-modelos2 %>%
  forecast(h="1 year")
pronostico2

pronostico2 %>%
  autoplot(mtibble,alpha=0.5, level=95)

#Para seleccionar una variable (un distrito) y un modelo para ese distrito
modelos2 %>%
  filter(variable=="Charlestown") %>%
  select(snaive) %>% 
  report()

#Si tiene comportamiento estacional lo detecta automáticamente y utiliza sma 

#para finalizar modelo mixto
modelo3<-mtibble %>%
  model(arima=ARIMA(value),
        ets=ETS(value),
        snaive=SNAIVE(value))%>%
  mutate(mixto=(ets+arima+snaive)/3)

modelo3

pronostico3 <- modelo3 %>%
  forecast(h="1 year")
pronostico3


pronostico3 %>%
  autoplot(mtibble,alpha=0.5,level=NULL) #level null para que no se traslapen los resultados



#COMPROBACIÓN PRECISIÓN DEL MODELO

DatosDistrito2 = read_excel("D:/Archivos/2022PUCV/2023PUCV/DATASCIENCE/Delitos_x_mes_x_distrito.xlsx")

# Definir el rango de tiempo
fecha_inicio2 <- as.Date("2018-01-01")
fecha_fin2 <- as.Date("2021-12-30")

# Generar secuencia de fechas dentro del rango
DatosDistrito2 <- subset(DatosDistrito2, Fecha >=  fecha_inicio2 & Fecha <= fecha_fin2)

#Elegir distritos en esta fila
DatosDistrito2 <- select(DatosDistrito2, Fecha, "South End") %>% dplyr::mutate(Fecha=tsibble::yearmonth(Fecha))

datostibble2 <- as_tibble(DatosDistrito2,index ="Fecha")

mtibble2 <- reshape2::melt (datostibble2,"Fecha")

mtibble2 <- as_tsibble(mtibble2, index = "Fecha",key = "variable")

#Modelo para cada variable
#se usa modelador automático
modelo1.2 <- mtibble2 %>%
  model(auto.ets = ETS(value))
modelo1.2
report(modelo1.2)

#Pronóstico usando los modelos
#se supone que lo hace para todas las variables a la vez
pronostico1.2<-modelo1.2 %>%
  forecast(h="1 year") #1 año es lo que queremos predecir 
pronostico1.2

pronostico1.2 %>%
  autoplot(mtibble2, level=95, alpha=0.5) #level y alpha hace que sea mas abierto, no tan justo


#Intervalos de confianza
pronostico1.2 %>%
  hilo(level=c(80,95))

accuracy(modelo1.2)

#Múltiples modelos, múltiples variables
modelos2.2<- mtibble2 %>%
  model(arima=ARIMA(value), ets=ETS(value), snaive=SNAIVE(value))

modelos2.2

#Capacidad predictiva
accuracy(modelos2.2) #debe devolver los mismos valores para cada variable

#Pronósticos usando los 4 modelos
pronostico2.2<-modelos2.2 %>%
 forecast(h="1 year")
pronostico2.2

pronostico2.2 %>%
  autoplot(mtibble2,alpha=0.5, level=95)

#Para seleccionar una variable (un distrito) y un modelo para ese distrito
modelos2.2 %>%
  filter(variable=="Downtown") %>%
  select(ets) %>% 
  report()

#Si tiene comportamiento estacional lo detecta automáticamente y utiliza sma 

#Para finalizar modelo mixto
modelo3.2<-mtibble2 %>%
  model(arima=ARIMA(value),
        ets=ETS(value),
        snaive=SNAIVE(value))%>%
  mutate(mixto=(ets+arima+snaive)/3)

modelo3.2

pronostico3.2 <- modelo3.2 %>%
  forecast(h="1 year")
pronostico3.2


pronostico3.2 %>%
  autoplot(mtibble2,alpha=0.5,level=NULL) #level null para que no se traslapen los resultados

datosDistrito4 <- select(datosDistrito0, Fecha, Roxbury, Dorchester, "South End", Mattapan) %>% dplyr::mutate(Fecha=tsibble::yearmonth(Fecha))
datosDistrito5 <- select(datosDistrito0, Fecha, Downtown, "South Boston", Brighton, "Hyde Park") %>% dplyr::mutate(Fecha=tsibble::yearmonth(Fecha))
datosDistrito6 <- select(datosDistrito0, Fecha, "Jamaica Plain", "West Roxbury", "East Boston", "Charlestown") %>% dplyr::mutate(Fecha=tsibble::yearmonth(Fecha))

#Se puede generar gráfica para cualquier distrito
ggplot(datosDistrito4, aes(x = Fecha, y = datosDistrito0$`South End`)) +
  geom_line() +
  xlab("Periodo") +
  ylab("Frecuencia") +
  ggtitle("Delitos en South End")


# Evaluar el modelo y obtener la matriz de confusión
predicciones <- predict(modelo1.2, datos_nuevos)
matriz_confusion <- confusionMatrix(predicciones, DatosDistrito2$Fecha >= fecha_fin2)

# Obtener la precisión del modelo
precision <- matriz_confusion$overall['Accuracy']

# Imprimir la precisión
print(precision)