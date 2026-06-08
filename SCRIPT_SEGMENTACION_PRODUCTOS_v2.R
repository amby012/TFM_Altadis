# SEGMENTACIÓN DE PRODUCTOS ALTADIS A PARTIR DE CÓDIGOS INTRÍNSECOS
# TFM - Diseño de Sistema Business Intelligence para la Optimización del Canal de Ventas: El Caso Altadis
#
# Al no disponer de descripciones ni precios, se segmentan los 60 productos
# a partir de la información contenida en el propio código: el prefijo
# identifica la familia de producto y el campo SIZE el tamaño de la presentación.

install.packages(c("tidyverse"))
library(tidyverse)


### 1. CARGA DE DATOS ###

setwd("C:/Users/noeli/OneDrive/Desktop/TFM")

PRODUCTS <- read_delim("Product.csv", delim = ";",
                       locale = locale(encoding = "UTF-8"),
                       show_col_types = FALSE)

head(PRODUCTS)
str(PRODUCTS)
summary(PRODUCTS)


### 2. SEGMENTACIÓN POR CÓDIGO ###

# Extraemos el prefijo del código (letras iniciales = familia de producto)
# Ejemplo: "Dome004" → familia "Dome" | "Brit090" → familia "Brit"
PRODUCTS$familia <- gsub("[0-9]", "", PRODUCTS$Product_Code)

# Clasificamos el tamaño en tres tramos según el campo SIZE
PRODUCTS$tamanio <- ifelse(PRODUCTS$SIZE <= 100, "Pequeño",
                    ifelse(PRODUCTS$SIZE <= 200, "Medio", "Grande"))

head(PRODUCTS)


### 3. ANÁLISIS DESCRIPTIVO ###

# Productos por familia
table(PRODUCTS$familia)
#Dome: 24 | Inte: 11 | Natu: 9 | Fren: 6 | Brit: 4 | Trad: 4 | Don: 2

# Productos por tamaño
table(PRODUCTS$tamanio)

# Cruce familia y formato
table(PRODUCTS$familia, PRODUCTS$Format)
# Brit y Trad son exclusivamente ASL (cajetilla convencional)
# El resto mezclan formatos en mayor o menor medida

# Gráfico: productos por familia
ggplot(PRODUCTS, aes(x = familia, fill = Format)) +
  geom_bar(alpha = 0.85) +
  labs(title = "Número de productos por familia y formato",
       x = "Familia", y = "Nº productos", fill = "Formato") +
  theme_minimal()

# Gráfico: distribución del tamaño por familia
ggplot(PRODUCTS, aes(x = familia, y = SIZE, fill = familia)) +
  geom_boxplot(alpha = 0.75) +
  labs(title = "Distribución del tamaño (SIZE) por familia de producto",
       x = "Familia", y = "SIZE") +
  theme_minimal() +
  theme(legend.position = "none")


### 4. EXPORTAR RESULTADOS ###

write_csv2(PRODUCTS, "segmentacion_productos.csv")

cat("Segmentación completada.\n")
cat(sprintf("Familias identificadas: %d\n", n_distinct(PRODUCTS$familia)))
