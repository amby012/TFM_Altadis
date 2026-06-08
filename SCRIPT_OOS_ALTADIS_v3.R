# ANÁLISIS DE ROTURAS DE STOCK (OoS) — ALTADIS 2015
# TFM - Diseño de Sistema Business Intelligence para la Optimización del Canal de Ventas: El Caso Altadis

install.packages(c("tidyverse", "lubridate", "forecast", "psych", "corrplot"))

library(tidyverse)
library(lubridate)
library(ggplot2)
library(forecast)
library(psych)
library(corrplot)


### 1. COMPROBAR DIRECTORIO Y CARGA DE DATOS ###

setwd("C:/Users/noeli/OneDrive/Desktop/TFM")
getwd()

SALES    <- read_delim("SalesDay (1).csv", delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
OOS      <- read_delim("OoSDay.csv",       delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
DELIVERY <- read_delim("DeliveryDay.csv",  delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Se fuerza formato fecha
SALES$Sales_DAY       <- ymd(SALES$Sales_DAY)
OOS$OoS_DAY           <- ymd(OOS$OoS_DAY)
DELIVERY$Delivery_DAY <- ymd(DELIVERY$Delivery_DAY)

sapply(SALES, class)
head(SALES)
str(SALES)


### 2. PREPARACIÓN DEL DATAFRAME DIARIO ###

# Agregamos cada fichero por día
ventas_dia <- SALES %>%
  group_by(Sales_DAY) %>%
  summarise(ventas = sum(Sales_Uds, na.rm = TRUE)) %>%
  rename(fecha = Sales_DAY)

oos_dia <- OOS %>%
  group_by(OoS_DAY) %>%
  summarise(n_oos = n()) %>%
  rename(fecha = OoS_DAY)

entregas_dia <- DELIVERY %>%
  group_by(Delivery_DAY) %>%
  summarise(entregas = sum(Delivery_Uds, na.rm = TRUE)) %>%
  rename(fecha = Delivery_DAY)

# Unimos los tres ficheros en un dataframe diario
DF <- ventas_dia %>%
  left_join(oos_dia,      by = "fecha") %>%
  left_join(entregas_dia, by = "fecha") %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  arrange(fecha)

# Añadimos el día de la semana como variable numérica y como etiqueta
DF$dow         <- wday(DF$fecha, week_start = 1)
DF$dia_semana  <- wday(DF$fecha, label = TRUE, abbr = FALSE, week_start = 1)

# Añadimos variable festivo y día antes del festivo
# Festivos nacionales España 2015 dentro del período analizado (fuente: BOE)
festivos <- as.Date(c("2015-03-19", "2015-04-02", "2015-04-03",
                      "2015-05-01", "2015-08-15"))

DF$festivo     <- as.integer(DF$fecha %in% festivos)
DF$pre_festivo <- as.integer((DF$fecha + 1) %in% festivos)

head(DF)
str(DF)


### 3. ANÁLISIS DESCRIPTIVO ###

summary(DF)
describe(DF)
# La media de OoS diario es de 1.331 incidencias
# El domingo tiene valores muy bajos porque apenas hay actividad
# El máximo se registra en días de lunes con alta demanda acumulada

# Evolución del OoS diario
ggplot(DF, aes(x = fecha, y = n_oos)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(title = "Evolución diaria de roturas de stock",
       x = "Fecha", y = "Número de OoS")
# Se aprecia un patrón semanal muy claro: pico los lunes, caída los domingos

# OoS medio por día de la semana
DF %>%
  group_by(dia_semana) %>%
  summarise(media_oos = mean(n_oos)) %>%
  ggplot(aes(x = dia_semana, y = media_oos, fill = dia_semana)) +
  geom_col(alpha = 0.8) +
  labs(title = "OoS medio por día de la semana",
       x = NULL, y = "OoS medio") +
  theme(legend.position = "none")
# El lunes concentra el mayor número de roturas: el fin de semana acumula demanda sin reposición

# OoS en festivos vs días normales
DF %>%
  mutate(tipo = case_when(
    festivo     == 1 ~ "Festivo",
    pre_festivo == 1 ~ "Día antes festivo",
    TRUE             ~ "Día normal"
  )) %>%
  group_by(tipo) %>%
  summarise(media_oos = mean(n_oos)) %>%
  ggplot(aes(x = tipo, y = media_oos, fill = tipo)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(media_oos, 0)), vjust = -0.4, size = 4) +
  labs(title = "OoS medio según tipo de día",
       x = NULL, y = "OoS medio") +
  theme(legend.position = "none")
# El día antes de un festivo tiene casi un 40% más de OoS que un día normal

# Boxplot del OoS por día de la semana
ggplot(DF, aes(x = dia_semana, y = n_oos, fill = dia_semana)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución del OoS por día de la semana",
       x = NULL, y = "Número de OoS") +
  theme(legend.position = "none")

# Matriz de correlación entre las variables principales
cor_matrix <- cor(select(DF, ventas, n_oos, entregas, dow, festivo, pre_festivo))
cor_matrix
corrplot(cor_matrix,
         method  = "square",
         type    = "upper",
         tl.col  = "black",
         tl.srt  = 45)
# ventas y n_oos tienen correlación positiva (r=0.39): a más ventas, más riesgo de rotura
# dow y n_oos tienen correlación negativa: conforme avanza la semana bajan las roturas


### 4. MODELO DE REGRESIÓN LINEAL ###

# VARIABLE DEPENDIENTE: n_oos
# VARIABLES INDEPENDIENTES: ventas, entregas, dow, festivo, pre_festivo

modelo_oos <- lm(n_oos ~ ventas + entregas + dow + festivo + pre_festivo,
                 data = DF)

summary(modelo_oos)
# R² indica qué porcentaje de la variación del OoS explican estas variables
# Los coeficientes con *** son los factores más determinantes

# Gráfico real vs predicción
DF$prediccion <- predict(modelo_oos)

ggplot(DF, aes(x = fecha)) +
  geom_line(aes(y = n_oos,      color = "Real"),       linewidth = 0.8) +
  geom_line(aes(y = prediccion, color = "Predicción"), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("Real" = "steelblue", "Predicción" = "tomato")) +
  labs(title = "OoS real vs predicción del modelo de regresión",
       x = "Fecha", y = "Número de OoS", color = NULL) +
  theme_minimal()

# Precisión del modelo
cat("R²  :", round(summary(modelo_oos)$r.squared, 3), "\n")
cat("RMSE:", round(sqrt(mean(modelo_oos$residuals^2)), 1), "\n")


### 5. PREDICCIÓN CON ARIMA ###

# Creamos la serie temporal con frecuencia 7 (patrón semanal)
ts_oos <- ts(DF$n_oos, frequency = 7)

# Ajustamos el modelo ARIMA automáticamente
modelo_arima <- auto.arima(ts_oos)
summary(modelo_arima)

# Predicción a 14 días
pred <- forecast(modelo_arima, h = 14)
pred

# Gráfico de predicción
autoplot(pred) +
  labs(title = "Predicción de roturas de stock — próximas 2 semanas",
       x = "Tiempo", y = "Número de OoS") +
  theme_minimal()
# La línea azul es la predicción y las bandas muestran el margen de incertidumbre
