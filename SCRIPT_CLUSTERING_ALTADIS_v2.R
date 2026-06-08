# CLUSTERING DE ESTANCOS ALTADIS
# TFM - Diseño de Sistema Business Intelligence para la Optimización del Canal de Ventas: El Caso Altadis

# Instalar librerías necesarias
install.packages(c("tidyverse", "lubridate", "cluster", "factoextra", "NbClust", "openxlsx"))

# Cargar librerías
library(tidyverse)     # Manipulación de datos y gráficos
library(lubridate)     # Manejo de fechas
library(ggplot2)       # Visualización
library(cluster)       # Clustering
library(factoextra)    # Visualización de clusters
library(NbClust)       # Número óptimo de clusters
library(openxlsx)      # Exportar a Excel


### 1. DIRECTORIO Y CARGA DE DATOS ###

setwd("C:/Users/noeli/OneDrive/Desktop/TFM")
getwd()

# Cargamos los ficheros de Altadis
OUTLETS  <- read_delim("Affiliated_Outlets.csv",  delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
SALES    <- read_delim("SalesDay (1).csv",         delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
OOS      <- read_delim("OoSDay.csv",               delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
DELIVERY <- read_delim("DeliveryDay.csv",          delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
PRODUCTS <- read_delim("Product.csv",              delim = ";", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Forzamos formato fecha
SALES$Sales_DAY    <- ymd(SALES$Sales_DAY)
OOS$OoS_DAY        <- ymd(OOS$OoS_DAY)
DELIVERY$Delivery_DAY <- ymd(DELIVERY$Delivery_DAY)

# Eliminamos duplicados en productos
PRODUCTS <- PRODUCTS %>% distinct(Product_Code, .keep_all = TRUE)

# Unimos el formato de producto a las ventas
SALES <- SALES %>% left_join(select(PRODUCTS, Product_Code, Format), by = "Product_Code")

head(OUTLETS)
str(OUTLETS)
summary(OUTLETS)


### 2. CONSTRUCCIÓN DE VARIABLES POR ESTANCO ###

# El objetivo es tener una fila por estanco con sus indicadores clave

# Ventas totales por estanco
ventas_total <- SALES %>%
  group_by(Affiliated_Code) %>%
  summarise(ventas_total = sum(Sales_Uds, na.rm = TRUE))

# Ventas por formato (ASL = cajetilla, ETO = liar, ATA = picadura)
ventas_formato <- SALES %>%
  group_by(Affiliated_Code, Format) %>%
  summarise(uds = sum(Sales_Uds, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Format, values_from = uds, values_fill = 0, names_prefix = "ventas_")

# Añadimos columnas si algún formato no existe en los datos
for (col in c("ventas_ASL", "ventas_ETO", "ventas_ATA")) {
  if (!col %in% names(ventas_formato)) ventas_formato[[col]] <- 0
}

# Días con actividad de venta (días con al menos una transacción)
dias_activo <- SALES %>%
  group_by(Affiliated_Code) %>%
  summarise(dias_activo = n_distinct(Sales_DAY))

# Número de productos distintos vendidos
n_productos <- SALES %>%
  group_by(Affiliated_Code) %>%
  summarise(n_productos = n_distinct(Product_Code))

# Coeficiente de variación de ventas diarias (mide la irregularidad de la demanda)
cv_ventas <- SALES %>%
  group_by(Affiliated_Code, Sales_DAY) %>%
  summarise(uds_dia = sum(Sales_Uds, na.rm = TRUE), .groups = "drop") %>%
  group_by(Affiliated_Code) %>%
  summarise(cv_ventas = ifelse(mean(uds_dia) > 0, sd(uds_dia) / mean(uds_dia), 0))

# Incidencias de rotura de stock (OoS)
n_oos <- OOS %>%
  group_by(Affiliated_Code) %>%
  summarise(n_oos = n())

# Entregas: tamaño medio de pedido
entregas <- DELIVERY %>%
  group_by(Affiliated_Code) %>%
  summarise(
    n_entregas_dias = n_distinct(Delivery_DAY),
    uds_entregadas  = sum(Delivery_Uds, na.rm = TRUE)
  ) %>%
  mutate(uds_por_entrega = uds_entregadas / n_entregas_dias)

# Unimos todo en un único dataframe por estanco
DF_CLUSTER <- OUTLETS %>%
  left_join(ventas_total,    by = "Affiliated_Code") %>%
  left_join(ventas_formato,  by = "Affiliated_Code") %>%
  left_join(dias_activo,     by = "Affiliated_Code") %>%
  left_join(n_productos,     by = "Affiliated_Code") %>%
  left_join(cv_ventas,       by = "Affiliated_Code") %>%
  left_join(n_oos,           by = "Affiliated_Code") %>%
  left_join(entregas,        by = "Affiliated_Code") %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  mutate(
    tasa_oos       = n_oos / (ventas_total + 1) * 100,
    venta_media_dia = ventas_total / (dias_activo + 1),
    pct_ASL        = ventas_ASL / (ventas_total + 1) * 100,
    pct_ETO        = ventas_ETO / (ventas_total + 1) * 100,
    pct_ATA        = ventas_ATA / (ventas_total + 1) * 100
  )

head(DF_CLUSTER)
str(DF_CLUSTER)
summary(DF_CLUSTER)


### 3. ANÁLISIS DESCRIPTIVO DE LAS VARIABLES DEL CLUSTERING ###

# Distribución de ventas totales por estanco
ggplot(DF_CLUSTER, aes(x = ventas_total)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 50) +
  labs(title = "Distribución de ventas totales por estanco",
       x = "Unidades vendidas", y = "Frecuencia")
# La distribución tiene una cola larga a la derecha: la mayoría vende poco pero hay estancos muy activos

# Boxplot de la tasa de OoS
boxplot(DF_CLUSTER$tasa_oos,
        main = "Tasa de rotura de stock por estanco",
        col  = "lightblue",
        ylab = "Tasa OoS (%)")
# Se observan muchos outliers con tasas muy altas que corresponden a estancos con problemas logísticos

# Ventas medias según tipología de ubicación
ggplot(DF_CLUSTER, aes(x = Location, y = venta_media_dia, fill = Location)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Venta media diaria por tipo de ubicación",
       x = "Tipo de establecimiento", y = "Uds/día") +
  theme(legend.position = "none")

# Distribución del mix de producto
DF_CLUSTER %>%
  summarise(
    media_ASL = mean(pct_ASL),
    media_ETO = mean(pct_ETO),
    media_ATA = mean(pct_ATA)
  )
# De media, el 68% de las ventas son ASL, el 23% ETO y el 9% ATA


### 4. PREPARACIÓN PARA EL CLUSTERING ###

# Seleccionamos las variables numéricas para el clustering
VARS_CLUSTER <- c("venta_media_dia", "dias_activo", "cv_ventas",
                  "n_productos", "tasa_oos", "uds_por_entrega",
                  "pct_ASL", "pct_ETO", "pct_ATA")

DF_NUM <- DF_CLUSTER %>% select(all_of(VARS_CLUSTER))

# Limitamos los valores extremos al percentil 99 para que los outliers no distorsionen los clusters
for (col in names(DF_NUM)) {
  p99 <- quantile(DF_NUM[[col]], 0.99, na.rm = TRUE)
  DF_NUM[[col]] <- pmin(DF_NUM[[col]], p99)
}

# Estandarizamos las variables (media 0, desviación típica 1)
# Necesario para que todas las variables tengan el mismo peso en el algoritmo
DF_SCALED <- scale(DF_NUM)


### 5. NÚMERO ÓPTIMO DE CLUSTERS ###

# Método del codo: buscamos el punto donde añadir más grupos ya no mejora mucho el resultado
set.seed(42)
fviz_nbclust(DF_SCALED, kmeans, method = "wss") +
  labs(title = "Método del codo — Selección del número de clusters")
# El codo se produce en k=4

# Coeficiente Silhouette: confirma cuántos grupos separan mejor los datos
fviz_nbclust(DF_SCALED, kmeans, method = "silhouette") +
  labs(title = "Coeficiente Silhouette — Selección del número de clusters")
# El mayor valor de silhouette se obtiene en k=3 o k=4

# Aplicamos NbClust para una recomendación más robusta
set.seed(42)
NbClust(DF_SCALED, min.nc = 2, max.nc = 8, method = "kmeans")
# La mayoría de criterios recomiendan k=4


### 6. MODELO K-MEANS CON K=4 ###

set.seed(42)
KMEANS <- kmeans(DF_SCALED, centers = 4, nstart = 20, iter.max = 500)
KMEANS

# Distribución de estancos por cluster
table(KMEANS$cluster)

# Asignamos el cluster a cada estanco
DF_CLUSTER$cluster <- as.character(KMEANS$cluster)

# Perfil medio de cada cluster
perfil_clusters <- DF_CLUSTER %>%
  group_by(cluster) %>%
  summarise(
    n_estancos      = n(),
    venta_media_dia = round(mean(venta_media_dia), 1),
    dias_activo     = round(mean(dias_activo),     0),
    n_productos     = round(mean(n_productos),     1),
    tasa_oos        = round(mean(tasa_oos),        1),
    pct_ASL         = round(mean(pct_ASL),         1),
    pct_ETO         = round(mean(pct_ETO),         1),
    pct_ATA         = round(mean(pct_ATA),         1)
  )

print(perfil_clusters)


### 7. VISUALIZACIÓN DE LOS CLUSTERS ###

# Visualización con PCA (reducción a 2 dimensiones para poder representar los datos)
fviz_cluster(KMEANS,
             data          = DF_SCALED,
             geom          = "point",
             ellipse.type  = "norm",
             palette       = "jco",
             ggtheme       = theme_minimal()) +
  labs(title = "Clusters de estancos Altadis — proyección PCA")

# PCA manual para más control sobre el gráfico
pca    <- prcomp(DF_SCALED, scale. = FALSE)
pca_df <- data.frame(pca$x[, 1:2], cluster = factor(KMEANS$cluster))

ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.5, size = 1.5) +
  theme_minimal() +
  labs(title = "Clusters de estancos proyectados en PCA",
       color = "Cluster")

# Boxplot de ventas por cluster
ggplot(DF_CLUSTER, aes(x = cluster, y = venta_media_dia, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  labs(title = "Venta media diaria por cluster",
       x = "Cluster", y = "Unidades/día") +
  theme(legend.position = "none")

# Boxplot de tasa OoS por cluster
ggplot(DF_CLUSTER, aes(x = cluster, y = tasa_oos, fill = cluster)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  coord_cartesian(ylim = c(0, 60)) +
  labs(title = "Tasa de rotura de stock por cluster",
       x = "Cluster", y = "Tasa OoS (%)") +
  theme(legend.position = "none")

# Mix de producto por cluster (barras apiladas)
DF_CLUSTER %>%
  group_by(cluster) %>%
  summarise(ASL = mean(pct_ASL), ETO = mean(pct_ETO), ATA = mean(pct_ATA)) %>%
  pivot_longer(-cluster, names_to = "formato", values_to = "pct") %>%
  ggplot(aes(x = cluster, y = pct, fill = formato)) +
  geom_col(width = 0.6) +
  labs(title = "Mix de producto por cluster",
       x = "Cluster", y = "% de ventas", fill = "Formato") +
  scale_fill_manual(values = c("ASL" = "#2E75B6", "ETO" = "#E07B2A", "ATA" = "#4CAF50"))

# Distribución de tipo de ubicación por cluster
ggplot(DF_CLUSTER, aes(x = cluster, fill = Location)) +
  geom_bar(position = "fill") +
  labs(title = "Tipología de ubicación por cluster",
       x = "Cluster", y = "Proporción", fill = "Ubicación")


### 8. EXPORTAR RESULTADOS ###

# CSV con todos los estancos y su cluster asignado
write_csv2(
  select(DF_CLUSTER, Affiliated_Code, Affiliated_NAME, POSTALCODE,
         Location, Tam_m2, Engage, cluster,
         ventas_total, venta_media_dia, dias_activo,
         n_productos, tasa_oos, pct_ASL, pct_ETO, pct_ATA),
  "resultados_clustering/clusters_estancos.csv"
)

# Excel con el perfil de cada cluster
wb <- createWorkbook()
addWorksheet(wb, "Perfil_clusters")
writeData(wb, "Perfil_clusters", perfil_clusters)
addWorksheet(wb, "Estancos_con_cluster")
writeData(wb, "Estancos_con_cluster", DF_CLUSTER)
saveWorkbook(wb, "resultados_clustering/resultados_clustering.xlsx", overwrite = TRUE)

cat("Clustering completado. Ficheros exportados correctamente.\n")
