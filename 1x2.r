#https://jarroba.com/dataset-resultados-partidos-futbol-prediccion-machine-learning/

#- idPartido (4808): Un identificador ??nico de partido.
#- temporada (1977-78): Temporada en la que se jug?? el partido
#- division (1): Divisi??n en la que se jugo el partido (Primera '1', Segunda '2')
#- jornada (8): Jornada en la que se jug?? el partido
#- EquipoLocal (Rayo Vallecano): Nombre del Equipo Local
#- EquipoVisitante (Real Madrid): Nombre del Equipo Visitante
#- golesLocal (3): Goles que marc?? el equipo local
#- golesVisitante (2): Goles que marc?? el equipo visitante
#- fecha (30/10/1977): Fecha en la que se jug?? el partido
#- timestamp (247014000.0): Timestamp de la fecha en la que se jug?? el partido

#setwd("C:\\Users\\Gus\\iCloudDrive\\Desktop\\RPrograms\\1x2")
setwd("C:\\Users\\gustavo.palacios\\iCloudDrive\\Profesional\\Impronta\\RPrograms\\zzz_old\\1x2")

library(tidyverse)
library(lubridate)
library(rpart)
library(readxl)

# Funciones

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Invierno","Primavera","Verano","Oto?o","Invierno")
  return(cuts)
}

# Importamos el DataSet

df_1x2 <- read_excel("Dataset_1x2_Final.xlsx")
df_1x2$Division <- as.numeric(df_1x2$Division)
df_1x2$Jornada <- as.numeric(df_1x2$Jornada)
df_1x2$Goles_Local <- as.numeric(df_1x2$Goles_Local)
df_1x2$Goles_Visitante <- as.numeric(df_1x2$Goles_Visitante)

# Calculamos nuevos campos
df_1x2 <- df_1x2 %>%
  mutate(GolesFinales = Goles_Local - Goles_Visitante,
         #Fecha = dmy(Fecha),
         ResultadoQuiniela = ifelse(GolesFinales > 0, "1", ifelse(GolesFinales < 0, "2", "X")),
         Temporada = substr(Temporada, 1, 4),
         Partido = paste(Equipo_Local, " - ", Equipo_Visitante, sep=""),
         DiaSemana = weekdays(Fecha),
         Mes = months(Fecha),
         Estacion = getSeason(Fecha))



# Eliminar campos ID
df_1x2$Goles_Local <- NULL
df_1x2$Goles_Visitante <- NULL
df_1x2$Fecha <- NULL

# Seleccionamos las ultimas 10 temporadas
df_1x2 <- df_1x2 %>%
  filter(Temporada >= as.integer(substring(Sys.Date(),1,4))-10)

# Probar con campos tipo numericos, o campos tipo factorizados de forma ordinal
df_1x2$Partido <- as.factor(df_1x2$Partido)
df_1x2$DiaSemana <- as.factor(df_1x2$DiaSemana)
df_1x2$Estacion <- as.factor(df_1x2$Estacion)
df_1x2$Mes <- as.factor(df_1x2$Mes)

df_1x2$Equipo_Local <- as.factor(df_1x2$Equipo_Local)
df_1x2$Equipo_Visitante <- as.factor(df_1x2$Equipo_Visitante)

# Separamos el conjunto en Validacion y Test

set.seed(123)
sample <- 60
total <- nrow(df_1x2)
muestra <- as.integer(total*sample/100)
rows_train <- sort(sample(1:total,muestra))
aux <- 1:total
rows_validation <- aux[!(1:total %in% rows_train)]

train <- slice(df_1x2, rows_train)
validation <- slice(df_1x2, rows_validation)

# Comprobamos que no nos dejamos nada

nrow(train) + nrow(validation) == nrow(df_1x2)

# Generamos modelo con arbol de decision. Esto se lo copio a Daniel
modelo1 <- rpart(factor(ResultadoQuiniela) ~ 
                   Temporada + Division + Jornada + Equipo_Local + 
                   Equipo_Visitante + DiaSemana + Estacion + Mes,
                 data=train,method="class")

#modelo1 <- rpart(factor(ResultadoQuiniela) ~ 
#                   Temporada + Division + Jornada + Partido + 
#                   DiaSemana + Estacion + Mes,
#                 data=train,method="class")

#rpart.plot(modelo1)

prediccion1 <- predict(modelo1, validation, type="class")

table(validation$ResultadoQuiniela, prediccion1) %>% as_tibble() %>%
  rename(valor = Var1, prediccion = prediccion1) %>%
  mutate(`%` = sprintf("%03.2f", n / nrow(validation) * 100),
         acierto = as.integer(valor == prediccion)) %>%
  group_by(acierto) %>%
  mutate(`% acumulado` = cumsum(`%`)) %>%
  ungroup() %>%
  arrange(acierto) %>%
  select(-acierto)

# 83,72
