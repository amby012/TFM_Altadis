# =============================================================================
# ANÁLISIS DE CARACTERIZACIÓN DEL CONSUMIDOR DE TABACO
# Cruce ENSE 2011-2012 (INE) × Datos Altadis 2015
# =============================================================================
# TFM - Diseño de Sistema Business Intelligence para la Optimización del Canal
# de Ventas: El Caso Altadis
#
# Fuentes:
#   - ENSE 2011-2012: Microdatos adulto anonimizado (INE / Ministerio de Sanidad)
#   - Datos Altadis:  SalesDay, OoSDay, DeliveryDay, Affiliated_Outlets, Product
#
# Requisitos:
#   install.packages(c("tidyverse","readr","lubridate","openxlsx",
#                      "scales","ggrepel","patchwork","viridis"))
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(openxlsx)
  library(ggrepel)
  library(patchwork)
  library(viridis)
})

# ─────────────────────────────────────────────────────────────────────────────
# 0. CONFIGURACIÓN Y RUTAS
# ─────────────────────────────────────────────────────────────────────────────

# ▶ Rutas configuradas para el entorno de Noelia
BASE <- "C:/Users/noeli/OneDrive/Desktop/TFM"

RUTA_ENSE_ADULTO <- file.path(BASE, "MICRODATO ADULTO ANONIMIZADO.txt")
RUTA_OUTLETS     <- file.path(BASE, "Affiliated_Outlets.csv")
RUTA_SALES       <- file.path(BASE, "SalesDay (1).csv")
RUTA_OOS         <- file.path(BASE, "OoSDay.csv")
RUTA_DELIVERY    <- file.path(BASE, "DeliveryDay.csv")
RUTA_PRODUCT     <- file.path(BASE, "Product.csv")
RUTA_SALIDA      <- file.path(BASE, "resultados_altadis_ense/")

dir.create(RUTA_SALIDA, showWarnings = FALSE, recursive = TRUE)

# Paleta corporativa
COLORES <- list(
  azul_oscuro = "#1F3864",
  azul_medio  = "#2E75B6",
  naranja     = "#E07B2A",
  gris        = "#7F7F7F",
  verde       = "#4CAF50",
  rojo        = "#E53935"
)

# Tema ggplot personalizado
tema_altadis <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, margin = margin(b = 8)),
    plot.subtitle    = element_text(size = 10, color = "grey50"),
    axis.title       = element_text(size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA)
  )

# Mapas de código
CCAA_NOMBRES <- c(
  "01" = "Andalucía",     "02" = "Aragón",         "03" = "Asturias",
  "04" = "Baleares",      "05" = "Canarias",        "06" = "Cantabria",
  "07" = "CastillaLeón",  "08" = "CastillaMancha",  "09" = "Cataluña",
  "10" = "C.Valenciana",  "11" = "Extremadura",     "12" = "Galicia",
  "13" = "Madrid",        "14" = "Murcia",           "15" = "Navarra",
  "16" = "PaísVasco",     "17" = "LaRioja",          "18" = "Ceuta",
  "19" = "Melilla"
)

CP2CCAA <- c(
  "04"="01","18"="01","21"="01","23"="01","29"="01","41"="01","11"="01","14"="01",
  "22"="02","44"="02","50"="02",
  "33"="03",
  "07"="04",
  "35"="05","38"="05",
  "39"="06",
  "05"="07","09"="07","24"="07","34"="07","37"="07","40"="07","42"="07","47"="07","49"="07",
  "02"="08","13"="08","16"="08","19"="08","45"="08",
  "08"="09","17"="09","25"="09","43"="09",
  "03"="10","12"="10","46"="10",
  "06"="11","10"="11",
  "15"="12","27"="12","32"="12","36"="12",
  "28"="13",
  "30"="14",
  "31"="15",
  "01"="16","20"="16","48"="16",
  "26"="17",
  "51"="18",
  "52"="19"
)

# ─────────────────────────────────────────────────────────────────────────────
# 1. CARGA Y LIMPIEZA DE MICRODATOS ENSE 2011-2012
# ─────────────────────────────────────────────────────────────────────────────
cat(strrep("=", 65), "\n")
cat("BLOQUE 1 — Carga de microdatos ENSE 2011-2012\n")
cat(strrep("=", 65), "\n")

# Diseño de registro (posiciones 1-based en R con substr)
# substr(x, start, stop) — posiciones 1-indexed
leer_ense <- function(ruta) {
  lineas <- readLines(ruta, encoding = "latin1")
  lineas <- lineas[nchar(lineas) >= 310]
  
  datos <- tibble(
    linea   = lineas,
    ccaa    = substr(linea,   1,  2),
    sexo    = substr(linea,  14, 14),
    edad    = substr(linea,  16, 17),
    estudios= substr(linea,  41, 41),
    clase   = substr(linea,  73, 73),
    tipo_tab= substr(linea, 294, 294),
    fuma    = substr(linea, 308, 308)
  ) |>
    select(-linea) |>
    mutate(
      across(everything(), str_trim),
      edad_num = suppressWarnings(as.integer(edad))
    ) |>
    filter(
      !is.na(edad_num),
      edad_num >= 15,
      sexo %in% c("1", "2")
    ) |>
    mutate(
      gedad = case_when(
        edad_num < 25 ~ "15-24",
        edad_num < 35 ~ "25-34",
        edad_num < 45 ~ "35-44",
        edad_num < 55 ~ "45-54",
        edad_num < 65 ~ "55-64",
        TRUE          ~ "65+"
      ),
      fumador    = if_else(fuma == "1", 1L, 0L),
      liar       = if_else(tipo_tab == "2", 1L, 0L),
      cigarrillo = if_else(tipo_tab == "1", 1L, 0L),
      puro       = if_else(tipo_tab == "4", 1L, 0L),
      sexo_label = if_else(sexo == "1", "Hombre", "Mujer"),
      ccaa_nombre= CCAA_NOMBRES[ccaa]
    )
}

ense <- leer_ense(RUTA_ENSE_ADULTO)

cat(sprintf("  Registros válidos cargados : %s\n",   format(nrow(ense), big.mark=".")))
cat(sprintf("  Prevalencia fumadores      : %.1f%%\n", mean(ense$fumador) * 100))
cat(sprintf("  %% uso picadura/liar        : %.1f%%\n",
            filter(ense, fumador == 1) |> pull(liar) |> mean() * 100))
cat(sprintf("  CCAAs cubiertas            : %d\n", n_distinct(ense$ccaa)))

# ─────────────────────────────────────────────────────────────────────────────
# 2. ANÁLISIS DE PREVALENCIA FUMADORA
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 65), "\n")
cat("BLOQUE 2 — Prevalencia fumadora por perfil sociodemográfico\n")
cat(strrep("=", 65), "\n")

ORDEN_EDAD <- c("15-24","25-34","35-44","45-54","55-64","65+")

# 2a. Por sexo × grupo de edad
prev_edad_sexo <- ense |>
  group_by(gedad, sexo_label) |>
  summarise(
    tasa_pct   = round(mean(fumador) * 100, 1),
    n_fumadores= sum(fumador),
    n_total    = n(),
    .groups    = "drop"
  ) |>
  mutate(gedad = factor(gedad, levels = ORDEN_EDAD))

cat("\nPrevalencia por sexo y grupo de edad:\n")
print(prev_edad_sexo)

# 2b. Por tipo de producto entre fumadores
fumadores_df <- filter(ense, fumador == 1)

prev_tipo <- fumadores_df |>
  group_by(gedad, sexo_label) |>
  summarise(
    liar       = round(mean(liar) * 100, 1),
    cigarrillo = round(mean(cigarrillo) * 100, 1),
    puro       = round(mean(puro) * 100, 1),
    .groups    = "drop"
  )

cat("\n% tipo de tabaco entre fumadores (por edad y sexo):\n")
print(prev_tipo)

# 2c. Por CCAA
prev_ccaa <- ense |>
  group_by(ccaa, ccaa_nombre) |>
  summarise(
    tasa_pct   = round(mean(fumador) * 100, 1),
    n_fumadores= sum(fumador),
    n_total    = n(),
    .groups    = "drop"
  ) |>
  arrange(desc(tasa_pct))

cat("\nPrevalencia por CCAA (ordenada de mayor a menor):\n")
print(prev_ccaa)

# ─────────────────────────────────────────────────────────────────────────────
# 3. CARGA Y ANÁLISIS DE DATOS ALTADIS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 65), "\n")
cat("BLOQUE 3 — Carga y análisis de datos Altadis\n")
cat(strrep("=", 65), "\n")

# Outlets
outlets <- read_delim(RUTA_OUTLETS, delim = ";", locale = locale(encoding = "UTF-8"),
                      show_col_types = FALSE) |>
  rename_with(str_trim) |>
  mutate(
    CP2  = str_pad(as.character(POSTALCODE), 5, pad = "0") |> str_sub(1, 2),
    CCAA = CP2CCAA[CP2]
  )

cat(sprintf("  Outlets: %s establecimientos, %s CPs únicos\n",
            format(nrow(outlets), big.mark="."),
            format(n_distinct(outlets$POSTALCODE), big.mark=".")))

# Products
products <- read_delim(RUTA_PRODUCT, delim = ";", locale = locale(encoding = "UTF-8"),
                       show_col_types = FALSE) |>
  rename_with(str_trim)

cat(sprintf("  Productos: %d referencias\n", nrow(products)))

# Sales
cat("  Leyendo SalesDay (puede tardar unos segundos)...\n")
sales <- read_delim(RUTA_SALES, delim = ";", locale = locale(encoding = "UTF-8"),
                    col_types = cols(
                      Sales_DAY      = col_character(),
                      Affiliated_Code= col_character(),
                      Product_Code   = col_character(),
                      Sales_Uds      = col_double()
                    ), show_col_types = FALSE) |>
  rename_with(str_trim) |>
  mutate(
    Sales_DAY = ymd(Sales_DAY),
    mes       = month(Sales_DAY),
    semana    = isoweek(Sales_DAY)
  ) |>
  left_join(select(products, Product_Code, Format), by = "Product_Code") |>
  left_join(select(outlets, Affiliated_Code, POSTALCODE, CP2, CCAA,
                   Location, Management_Cluster, Tam_m2),
            by = "Affiliated_Code")

cat(sprintf("  SalesDay: %s registros, %s uds totales\n",
            format(nrow(sales), big.mark="."),
            format(sum(sales$Sales_Uds, na.rm=TRUE), big.mark=".", nsmall=0)))
cat(sprintf("  Período : %s → %s\n",
            min(sales$Sales_DAY, na.rm=TRUE),
            max(sales$Sales_DAY, na.rm=TRUE)))

# OoS
oos <- read_delim(RUTA_OOS, delim = ";", locale = locale(encoding = "UTF-8"),
                  col_types = cols(OoS_DAY = col_character()),
                  show_col_types = FALSE) |>
  rename_with(str_trim) |>
  mutate(OoS_DAY = ymd(OoS_DAY)) |>
  left_join(select(outlets, Affiliated_Code, CCAA, Location, Management_Cluster),
            by = "Affiliated_Code")

cat(sprintf("  OoSDay  : %s incidencias de rotura de stock\n", format(nrow(oos), big.mark=".")))

# Delivery
delivery <- read_delim(RUTA_DELIVERY, delim = ";", locale = locale(encoding = "UTF-8"),
                       col_types = cols(Delivery_DAY = col_character()),
                       show_col_types = FALSE) |>
  rename_with(str_trim) |>
  mutate(Delivery_DAY = ymd(Delivery_DAY))

cat(sprintf("  DeliveryDay: %s registros de entrega\n", format(nrow(delivery), big.mark=".")))

# ─── Agregaciones clave ────────────────────────────────────────────

# Ventas por CCAA y formato
ventas_ccaa <- sales |>
  group_by(CCAA, Format) |>
  summarise(uds = sum(Sales_Uds, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Format, values_from = uds, values_fill = 0) |>
  mutate(
    TOTAL   = rowSums(across(any_of(c("ASL","ETO","ATA"))), na.rm = TRUE),
    pct_ASL = round(ASL / TOTAL * 100, 1),
    pct_ETO = round(ETO / TOTAL * 100, 1),
    pct_ATA = round(ATA / TOTAL * 100, 1),
    CCAA_nombre = CCAA_NOMBRES[CCAA]
  )

# Ventas por tipo de ubicación
ventas_loc <- sales |>
  group_by(Location) |>
  summarise(
    total        = sum(Sales_Uds, na.rm = TRUE),
    media_diaria = mean(Sales_Uds, na.rm = TRUE),
    n_registros  = n(),
    .groups = "drop"
  ) |>
  arrange(desc(total))

# OoS por CCAA
oos_ccaa <- oos |>
  count(CCAA, name = "n_oos") |>
  mutate(CCAA_nombre = CCAA_NOMBRES[CCAA])

# Ventas diarias (serie temporal)
ventas_dia <- sales |>
  group_by(Sales_DAY) |>
  summarise(uds = sum(Sales_Uds, na.rm = TRUE), .groups = "drop")

# Outlet performance
outlet_sales <- sales |>
  group_by(Affiliated_Code) |>
  summarise(ventas_total = sum(Sales_Uds, na.rm = TRUE), .groups = "drop")

outlet_oos <- oos |>
  count(Affiliated_Code, name = "n_oos")

outlet_del <- delivery |>
  group_by(Affiliated_Code) |>
  summarise(entregas_total = sum(Delivery_Uds, na.rm = TRUE), .groups = "drop")

outlet_perf <- outlets |>
  left_join(outlet_sales, by = "Affiliated_Code") |>
  left_join(outlet_oos,   by = "Affiliated_Code") |>
  left_join(outlet_del,   by = "Affiliated_Code") |>
  mutate(
    across(c(ventas_total, n_oos, entregas_total), ~replace_na(.x, 0)),
    tasa_oos = round(n_oos / (ventas_total + 1) * 100, 2)
  )

cat(sprintf("\n  Ventas medias por outlet : %.0f uds\n", mean(outlet_perf$ventas_total, na.rm=TRUE)))
cat(sprintf("  OoS media por outlet    : %.1f incidencias\n", mean(outlet_perf$n_oos, na.rm=TRUE)))

# ─────────────────────────────────────────────────────────────────────────────
# 4. CRUCE ENSE × ALTADIS POR CCAA
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 65), "\n")
cat("BLOQUE 4 — Cruce ENSE × Altadis: modelo de caracterización\n")
cat(strrep("=", 65), "\n")

outlets_por_ccaa <- outlets |>
  count(CCAA, name = "outlets_ccaa")

modelo <- prev_ccaa |>
  inner_join(select(ventas_ccaa, CCAA, TOTAL, pct_ASL, pct_ETO, pct_ATA),
             by = c("ccaa" = "CCAA")) |>
  left_join(select(oos_ccaa, CCAA, n_oos), by = c("ccaa" = "CCAA")) |>
  left_join(outlets_por_ccaa,               by = c("ccaa" = "CCAA")) |>
  mutate(
    ventas_por_fumador_ense = round(TOTAL / n_total / (tasa_pct / 100), 1),
    idx_oportunidad = round(
      (tasa_pct - mean(tasa_pct, na.rm=TRUE)) / sd(tasa_pct, na.rm=TRUE) -
        (ventas_por_fumador_ense - mean(ventas_por_fumador_ense, na.rm=TRUE)) /
        sd(ventas_por_fumador_ense, na.rm=TRUE),
      3
    )
  )

q_prev  <- median(modelo$tasa_pct,              na.rm = TRUE)
q_venta <- median(modelo$ventas_por_fumador_ense, na.rm = TRUE)

modelo <- modelo |>
  mutate(
    cuadrante = case_when(
      tasa_pct >= q_prev & ventas_por_fumador_ense >= q_venta ~ "Mercado maduro (defender)",
      tasa_pct >= q_prev & ventas_por_fumador_ense <  q_venta ~ "Zona oportunidad (crecer)",
      tasa_pct <  q_prev & ventas_por_fumador_ense >= q_venta ~ "Alta eficiencia (optimizar)",
      TRUE                                                     ~ "Bajo potencial (monitorizar)"
    )
  ) |>
  arrange(desc(idx_oportunidad))

cat("\nModelo de caracterización por CCAA (ordenado por índice oportunidad):\n")
print(select(modelo, ccaa_nombre, tasa_pct, TOTAL, ventas_por_fumador_ense,
             n_oos, outlets_ccaa, idx_oportunidad, cuadrante))

# Perfil del fumador por tipo de producto
cat("\n=== PERFIL PICADURA vs CIGARRILLO (ENSE) ===\n")
etiquetas_tipo <- c("1"="Cigarrillo (ASL)","2"="Picadura/Liar (ETO/ATA)",
                    "3"="No fuma","4"="Puro/Cigarro")

perfil_tipo <- fumadores_df |>
  group_by(tipo_tab) |>
  summarise(
    n          = n(),
    edad_media = round(mean(edad_num), 1),
    pct_hombre = round(mean(sexo == "1") * 100, 1),
    .groups    = "drop"
  ) |>
  mutate(tipo_tab = etiquetas_tipo[tipo_tab])

print(perfil_tipo)

# ─────────────────────────────────────────────────────────────────────────────
# 5. ANÁLISIS DE OPORTUNIDAD DE MERCADO
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 65), "\n")
cat("BLOQUE 5 — Análisis de oportunidad de mercado\n")
cat(strrep("=", 65), "\n")

top_outlets <- outlet_perf |>
  slice_max(ventas_total, n = 10) |>
  select(Affiliated_Code, Affiliated_NAME, POSTALCODE, Location,
         Management_Cluster, Tam_m2, ventas_total, n_oos, tasa_oos)

cat("\nTop 10 outlets por volumen de ventas:\n")
print(top_outlets)

riesgo_oos <- outlet_perf |>
  filter(ventas_total > quantile(ventas_total, 0.75, na.rm=TRUE)) |>
  slice_max(tasa_oos, n = 10) |>
  select(Affiliated_Code, Location, ventas_total, n_oos, tasa_oos, CCAA)

cat("\nTop 10 outlets con mayor tasa OoS (entre los más vendedores):\n")
print(riesgo_oos)

corr_test <- cor.test(modelo$tasa_pct, modelo$TOTAL)
cat(sprintf("\nCorrelación Pearson prevalencia ENSE vs ventas Altadis: r=%.3f, p=%.3f\n",
            corr_test$estimate, corr_test$p.value))

cat("\nVentas medias por tipo de ubicación:\n")
print(select(ventas_loc, Location, total, media_diaria))

# ─────────────────────────────────────────────────────────────────────────────
# 6. VISUALIZACIONES
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 65), "\n")
cat("BLOQUE 6 — Generando visualizaciones...\n")
cat(strrep("=", 65), "\n")

fig_list <- c()

guardar <- function(nombre, fig, w = 10, h = 5.5) {
  ruta <- file.path(RUTA_SALIDA, nombre)
  ggsave(ruta, plot = fig, width = w, height = h, dpi = 120, bg = "white")
  cat(sprintf("  ✓ %s\n", nombre))
  c(nombre)
}

# ── Fig 1: Prevalencia fumadores ENSE por sexo y grupo de edad ─────────────
fig1 <- prev_edad_sexo |>
  mutate(gedad = factor(gedad, levels = ORDEN_EDAD)) |>
  ggplot(aes(x = gedad, y = tasa_pct, fill = sexo_label)) +
  geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
  geom_text(aes(label = paste0(tasa_pct, "%")),
            position = position_dodge(0.75), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Hombre" = COLORES$azul_medio,
                               "Mujer"  = COLORES$naranja)) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 42)) +
  labs(
    title    = "Prevalencia de fumadores por sexo y grupo de edad",
    subtitle = "ENSE 2011-2012 (INE)",
    x        = "Grupo de edad",
    y        = "Prevalencia fumadores (%)",
    fill     = NULL
  ) +
  tema_altadis

fig_list <- c(fig_list, guardar("fig1_prevalencia_edad_sexo.png", fig1, 10, 5.5))

# ── Fig 2: Prevalencia por CCAA ────────────────────────────────────────────
umbral_alto <- quantile(prev_ccaa$tasa_pct, 0.7, na.rm=TRUE)
media_prev  <- mean(prev_ccaa$tasa_pct, na.rm=TRUE)

fig2 <- prev_ccaa |>
  mutate(
    ccaa_nombre = fct_reorder(ccaa_nombre, tasa_pct),
    color_bar   = if_else(tasa_pct >= umbral_alto, "alto", "normal")
  ) |>
  ggplot(aes(y = ccaa_nombre, x = tasa_pct, fill = color_bar)) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = media_prev, linetype = "dashed",
             color = COLORES$gris, linewidth = 0.8) +
  geom_text(aes(label = paste0(tasa_pct, "%")), hjust = -0.1, size = 3) +
  scale_fill_manual(values = c("alto"  = COLORES$rojo,
                               "normal"= COLORES$azul_medio),
                    guide = "none") +
  scale_x_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  annotate("text", x = media_prev + 0.3, y = 0.6,
           label = sprintf("Media: %.1f%%", media_prev),
           color = COLORES$gris, size = 3, hjust = 0) +
  labs(
    title    = "Prevalencia de fumadores por Comunidad Autónoma",
    subtitle = "ENSE 2011-2012 (INE)",
    x        = "Prevalencia fumadores (%)",
    y        = NULL
  ) +
  tema_altadis

fig_list <- c(fig_list, guardar("fig2_prevalencia_ccaa.png", fig2, 11, 7))

# ── Fig 3: Ventas Altadis por formato (gráfico de sectores) ────────────────
ventas_fmt_total <- sales |>
  group_by(Format) |>
  summarise(uds = sum(Sales_Uds, na.rm=TRUE), .groups="drop") |>
  mutate(
    pct   = uds / sum(uds),
    label = sprintf("%s\n%.1f%%", Format, pct * 100)
  )

fig3 <- ventas_fmt_total |>
  ggplot(aes(x = "", y = uds, fill = Format)) +
  geom_col(width = 1, color = "white", linewidth = 1.2) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c(
    "ASL" = COLORES$azul_medio,
    "ETO" = COLORES$naranja,
    "ATA" = COLORES$verde
  )) +
  labs(
    title    = "Distribución de ventas por formato de producto",
    subtitle = sprintf("Altadis 2015 — Total: %s unidades",
                       format(sum(ventas_fmt_total$uds), big.mark=".")),
    fill     = NULL
  ) +
  tema_altadis +
  theme(
    axis.text = element_blank(), axis.title = element_blank(),
    panel.grid = element_blank()
  )

fig_list <- c(fig_list, guardar("fig3_ventas_formato.png", fig3, 7, 5.5))

# ── Fig 4: Ventas diarias (serie temporal) ─────────────────────────────────
ventas_dia_roll <- ventas_dia |>
  arrange(Sales_DAY) |>
  mutate(media7 = zoo::rollmean(uds, k = 7, fill = NA, align = "right"))

fig4 <- ggplot(ventas_dia_roll, aes(x = Sales_DAY)) +
  geom_ribbon(aes(ymin = 0, ymax = uds), fill = COLORES$azul_medio, alpha = 0.25) +
  geom_line(aes(y = uds, color = "Diario"), linewidth = 0.6, alpha = 0.7) +
  geom_line(aes(y = media7, color = "Media 7 días"), linewidth = 1.8) +
  scale_color_manual(values = c("Diario" = COLORES$azul_medio,
                                "Media 7 días" = COLORES$azul_oscuro)) +
  scale_y_continuous(labels = label_comma()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(
    title    = "Evolución diaria de ventas — Altadis 2015",
    x        = "Fecha",
    y        = "Unidades vendidas",
    color    = NULL
  ) +
  tema_altadis +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

fig_list <- c(fig_list, guardar("fig4_serie_temporal_ventas.png", fig4, 14, 5.5))

# ── Fig 5: Mapa de cuadrantes estratégicos CCAA ────────────────────────────
colores_cuad <- c(
  "Mercado maduro (defender)"   = COLORES$rojo,
  "Zona oportunidad (crecer)"   = COLORES$verde,
  "Alta eficiencia (optimizar)" = COLORES$azul_medio,
  "Bajo potencial (monitorizar)"= COLORES$gris
)

fig5 <- modelo |>
  mutate(outlets_ccaa = replace_na(outlets_ccaa, 10)) |>
  ggplot(aes(x = tasa_pct, y = ventas_por_fumador_ense,
             color = cuadrante, size = outlets_ccaa)) +
  geom_vline(xintercept = q_prev,  linetype = "dashed", color = "grey70") +
  geom_hline(yintercept = q_venta, linetype = "dashed", color = "grey70") +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = ccaa_nombre), size = 3, show.legend = FALSE) +
  scale_color_manual(values = colores_cuad) +
  scale_size_continuous(range = c(3, 10), guide = "none") +
  labs(
    title    = "Mapa estratégico de CCAAs",
    subtitle = "Prevalencia ENSE vs Intensidad de ventas Altadis",
    x        = "Prevalencia fumadores ENSE 2011-12 (%)",
    y        = "Ventas Altadis por fumador estimado (uds)",
    color    = "Cuadrante"
  ) +
  tema_altadis +
  theme(legend.position = "right")

fig_list <- c(fig_list, guardar("fig5_cuadrantes_ccaa.png", fig5, 12, 8))

# ── Fig 6: OoS vs Ventas por outlet (scatter) ─────────────────────────────
set.seed(42)
sample_outlets <- outlet_perf |>
  filter(ventas_total > 0) |>
  slice_sample(n = min(2000, nrow(outlet_perf))) |>
  mutate(Engage = as.numeric(as.factor(Management_Cluster)))

fig6 <- sample_outlets |>
  ggplot(aes(x = ventas_total, y = n_oos, color = Engage)) +
  geom_point(alpha = 0.45, size = 1.5) +
  scale_color_viridis_c(option = "plasma", name = "Nivel Engage\n(proxy cluster)") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title    = "Relación Ventas vs OoS por establecimiento",
    subtitle = "Coloreado por cluster de gestión (Management_Cluster)",
    x        = "Ventas totales (unidades)",
    y        = "Incidencias Out-of-Stock"
  ) +
  tema_altadis

fig_list <- c(fig_list, guardar("fig6_ventas_oos_scatter.png", fig6, 10, 6))

# ── Fig 7: Ventas por tipo de ubicación ────────────────────────────────────
fig7 <- ventas_loc |>
  mutate(Location = fct_reorder(Location, total)) |>
  ggplot(aes(y = Location, x = total / 1e6)) +
  geom_col(fill = COLORES$azul_medio, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.2fM", total / 1e6)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = label_comma(suffix = "M"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Ventas totales por tipología de establecimiento",
    subtitle = "Altadis 2015",
    x        = "Ventas totales (millones de unidades)",
    y        = NULL
  ) +
  tema_altadis

fig_list <- c(fig_list, guardar("fig7_ventas_ubicacion.png", fig7, 10, 5.5))

# ── Fig 8: % picadura por grupo edad (ENSE) ─────────────────────────────────
liar_edad <- fumadores_df |>
  group_by(gedad, sexo_label) |>
  summarise(pct_liar = mean(liar) * 100, .groups = "drop") |>
  mutate(gedad = factor(gedad, levels = ORDEN_EDAD))

fig8 <- liar_edad |>
  ggplot(aes(x = gedad, y = pct_liar, fill = sexo_label)) +
  geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct_liar, 1), "%")),
            position = position_dodge(0.75), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Hombre" = COLORES$azul_medio,
                               "Mujer"  = COLORES$naranja)) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Consumo de picadura (tabaco de liar) por edad y sexo",
    subtitle = "ENSE 2011-2012 — relevante para productos ETO/ATA Altadis",
    x        = "Grupo de edad",
    y        = "% fumadores que usan picadura/liar",
    fill     = NULL
  ) +
  tema_altadis

fig_list <- c(fig_list, guardar("fig8_picadura_edad_sexo.png", fig8, 10, 5.5))

# ─────────────────────────────────────────────────────────────────────────────
# 7. EXPORTACIÓN DE RESULTADOS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 65), "\n")
cat("BLOQUE 7 — Exportando resultados a Excel...\n")
cat(strrep("=", 65), "\n")

wb <- createWorkbook()

add_hoja <- function(wb, df, nombre) {
  addWorksheet(wb, nombre)
  writeData(wb, nombre, df)
}

add_hoja(wb, prev_edad_sexo, "ENSE_Prevalencia_EdadSexo")
add_hoja(wb, prev_ccaa,      "ENSE_Prevalencia_CCAA")
add_hoja(wb, prev_tipo,      "ENSE_TipoTabaco")
add_hoja(wb, modelo,         "Modelo_ENSE_Altadis_CCAA")
add_hoja(wb, ventas_loc,     "Altadis_Ventas_Ubicacion")
add_hoja(wb, top_outlets,    "Altadis_Top10_Outlets")
add_hoja(wb, riesgo_oos,     "Altadis_RiesgoOoS")
add_hoja(wb, outlet_perf,    "Altadis_Outlet_Performance")

ruta_excel <- file.path(RUTA_SALIDA, "resultados_caracterizacion.xlsx")
saveWorkbook(wb, ruta_excel, overwrite = TRUE)
cat(sprintf("  ✓ resultados_caracterizacion.xlsx (%d hojas)\n", length(wb$worksheets)))

# ─────────────────────────────────────────────────────────────────────────────
# RESUMEN FINAL
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 65), "\n")
cat("RESUMEN DEL ANÁLISIS\n")
cat(strrep("=", 65), "\n")
cat(sprintf("  Encuesta ENSE analizada    : 2011-2012 (n=%s adultos)\n",
            format(nrow(ense), big.mark=".")))
cat(sprintf("  Prevalencia fumadores (ES) : %.1f%%\n", mean(ense$fumador)*100))
cat(sprintf("  %% fumadores usan picadura  : %.1f%%\n", mean(fumadores_df$liar)*100))
cat(sprintf("  Período Altadis analizado  : %s → %s\n",
            min(sales$Sales_DAY, na.rm=TRUE), max(sales$Sales_DAY, na.rm=TRUE)))
cat(sprintf("  Outlets Altadis            : %s\n", format(nrow(outlets), big.mark=".")))
cat(sprintf("  Unidades vendidas          : %s\n",
            format(sum(sales$Sales_Uds, na.rm=TRUE), big.mark=".", nsmall=0)))
cat(sprintf("  Incidencias OoS            : %s\n", format(nrow(oos), big.mark=".")))
cat(sprintf("  CCAAs en modelo cruzado    : %d\n", nrow(modelo)))

cat("\n  Zonas de OPORTUNIDAD (alta prevalencia, bajas ventas relativas):\n")
modelo |>
  filter(str_detect(cuadrante, "oportunidad")) |>
  with(mapply(function(n, t, i)
    cat(sprintf("    → %s (prevalencia=%.1f%%, idx=%.2f)\n", n, t, i)),
    ccaa_nombre, tasa_pct, idx_oportunidad))

cat(sprintf("\n  Ficheros generados en: %s\n", normalizePath(RUTA_SALIDA)))
cat(sprintf("  Gráficos: %s\n", paste(fig_list, collapse=", ")))
cat("  Excel   : resultados_caracterizacion.xlsx\n")
cat("\n", strrep("=", 65), "\n")
cat("ANÁLISIS COMPLETADO\n")
cat(strrep("=", 65), "\n")