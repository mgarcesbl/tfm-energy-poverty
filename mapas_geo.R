# ============================================================================
# MAPAS DE MADRID — Secciones censales
# ============================================================================

# Instalar si no tienes
if (!require(sf)) install.packages("sf")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(patchwork)) install.packages("patchwork") # ya lo tienes

library(sf)
library(ggplot2)
library(stringr)

# ----------------------------------------------------------------------------
# PASO 1: Cargar shapefile del INE 2021
# Ajusta la ruta a donde tengas guardada la carpeta
# ----------------------------------------------------------------------------
madrid_sf <- st_read("data/seccionado_2021/SECC_CE_20210101.shp") |>
  filter(str_detect(CUSEC, "^28079"))

cat(sprintf("Secciones cargadas: %d\n", nrow(madrid_sf)))
cat("Columnas del shapefile:\n")
print(names(madrid_sf))

# ----------------------------------------------------------------------------
# PASO 2: Join con tu dataset analítico
# ----------------------------------------------------------------------------
madrid_map <- madrid_sf |> 
  left_join(
    dataset |> select(CUSEC, EVI_synthetic, energy_poverty, group, mean_equiv_income),
    by = "CUSEC"
  ) |> 
  mutate(
    # Extraer código de distrito del CUSEC para superponer límites
    district = str_sub(CUSEC, 6, 7),
    
    # Etiquetas legibles para el grupo
    group_label = case_when(
      group == "Concordant"              ~ "Concordant",
      group == "Non_Vulnerable"          ~ "Non-vulnerable",
      group == "Spatial_False_Positives" ~ "Spatial False Positives",
      group == "Spatial_False_Negatives" ~ "Spatial False Negatives"
    ),
    group_label = factor(group_label, levels = c(
      "Concordant", "Spatial False Positives", 
      "Spatial False Negatives", "Non-vulnerable"
    ))
  )

# Límites de distrito (agregando secciones)
district_sf <- madrid_map |> 
  group_by(district) |> 
  summarise(geometry = st_union(geometry))

# ----------------------------------------------------------------------------
# MAPA 1: EVI sintético continuo (gradiente norte-sur)
# ----------------------------------------------------------------------------
map1 <- ggplot(madrid_map) +
  geom_sf(aes(fill = EVI_synthetic), color = NA) +
  geom_sf(data = district_sf, fill = NA, color = "white", linewidth = 0.4) +
  scale_fill_gradientn(
    colours = c("#2166AC", "#92C5DE", "#F7F7F7", "#F4A582", "#B2182B"),
    name = "Synthetic EVI (%)",
    na.value = "grey80"
  ) +
  labs(
    title = "Synthetic Energy Vulnerability Index",
    subtitle = "Continuous EVI by census section (n = 2,443) | Madrid 2021"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "right"
  )

map1

# ----------------------------------------------------------------------------
# MAPA 2: Clasificación binaria EVI > 15
# ----------------------------------------------------------------------------
map2 <- ggplot(madrid_map) +
  geom_sf(aes(fill = energy_poverty), color = NA) +
  geom_sf(data = district_sf, fill = NA, color = "white", linewidth = 0.4) +
  scale_fill_manual(
    values = c("TRUE" = "#B2182B", "FALSE" = "#2166AC"),
    labels = c("TRUE" = "Energy poor (EVI > 15)", "FALSE" = "Non-poor"),
    name = NULL,
    na.value = "grey80"
  ) +
  labs(
    title = "Binary Energy Poverty Classification",
    subtitle = "EVI > 15 threshold | 40.0% of sections classified as energy poor"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "bottom"
  )

map2

# ----------------------------------------------------------------------------
# MAPA 3: Grupos de discrepancia (el más original)
# ----------------------------------------------------------------------------
map3 <- ggplot(madrid_map) +
  geom_sf(aes(fill = group_label), color = NA) +
  geom_sf(data = district_sf, fill = NA, color = "white", linewidth = 0.4) +
  scale_fill_manual(
    values = c(
      "Concordant"               = "#E57373",
      "Spatial False Positives"  = "#FF8C00",
      "Spatial False Negatives"  = "#8E44AD",
      "Non-vulnerable"           = "#82B0D9"
    ),
    name = NULL,
    na.value = "grey80"
  ) +
  labs(
    title = "Discrepancy Analysis Groups",
    subtitle = "Spatial mismatch between satellite-based and survey-based classifications"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 9)
  )

map3


# ----------------------------------------------------------------------------
# EXPORTAR MAPAS EN ALTA RESOLUCIÓN PARA WORD
# ----------------------------------------------------------------------------

# Crear carpeta de output si no existe
dir.create("/maps", recursive = TRUE, showWarnings = FALSE)

ggsave("output/maps/map1_EVI_continuo.png",
       plot   = map1,
       width  = 18, height = 16, units = "cm",
       dpi    = 600, bg = "white")

ggsave("output/maps/map2_clasificacion_binaria.png",
       plot   = map2,
       width  = 18, height = 16, units = "cm",
       dpi    = 600, bg = "white")

ggsave("output/maps/map3_discrepancias.png",
       plot   = map3,
       width  = 18, height = 16, units = "cm",
       dpi    = 600, bg = "white")
