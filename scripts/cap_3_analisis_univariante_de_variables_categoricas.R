# ==============================================================================
# Análisis Exploratorio y Visualización de Datos con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Capítulo 3: Análisis Univariante de Variables Categóricas
# ==============================================================================

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(forcats)
library(janitor)
library(ggplot2)
library(knitr)
library(gt)
library(ggpubr)
library(tidyr)

# ==============================================================================
# 1. PREPARACIÓN DE LOS DATOS PARA EL ANÁLISIS
# ==============================================================================

data <- read_csv("data_covid.csv") %>%
  select(sexo, escolaridad, residencia, disnea, dolor_toracico, adinamia, astenia, fatiga) %>%
  mutate(
    sexo = fct_recode(
      factor(sexo),
      "Femenino" = "0",
      "Masculino" = "1"
    ),
    escolaridad = fct_recode(
      factor(escolaridad),
      "Analfabeta" = "0",
      "Primaria" = "1",
      "Secundaria" = "2",
      "Bachillerato" = "3",
      "Universidad" = "4"
    ),
    residencia = fct_recode(
      factor(residencia),
      "Ciudad de México" = "1",
      "Estado de México" = "2",
      "Guerrero" = "4",
      "Tabasco" = "9",
      "Puebla" = "10"
    ),
    disnea = fct_recode(
      factor(disnea),
      "No" = "0",
      "Sí" = "1"
    ),
    dolor_toracico = fct_recode(
      factor(dolor_toracico),
      "No" = "0",
      "Sí" = "1"
    ),
    adinamia = fct_recode(
      factor(adinamia),
      "No" = "0",
      "Sí" = "1"
    ),
    astenia = fct_recode(
      factor(astenia),
      "No" = "0",
      "Sí" = "1"
    ),
    fatiga = fct_recode(
      factor(fatiga),
      "No" = "0",
      "Sí" = "1"
    )
  )

# ==============================================================================
# 2. RESUMEN CON FRECUENCIAS Y PORCENTAJES
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 SEXO (Variable nominal dicotómica)
# ------------------------------------------------------------------------------

tabyl(data, sexo) %>%
  adorn_pct_formatting()

# Con totales
tabyl(data, sexo) %>%
  adorn_pct_formatting() %>%
  adorn_totals()

# ------------------------------------------------------------------------------
# 2.2 RESIDENCIA (Variable nominal politómica)
# ------------------------------------------------------------------------------

tabyl(data, residencia) %>%
  adorn_pct_formatting()

# ------------------------------------------------------------------------------
# 2.3 ESCOLARIDAD (Variable ordinal)
# ------------------------------------------------------------------------------

tabyl(data, escolaridad) %>%
  adorn_pct_formatting()

# Versión formateada con knitr
tabyl(data, escolaridad) %>%
  adorn_pct_formatting() %>%
  kable()

# Versión enriquecida con frecuencias y porcentajes acumulados
tabyl(data, escolaridad) %>%
  mutate(
    frecuencia_acumulada = cumsum(n),
    porcentaje_acumulado = cumsum(percent)
  ) %>%
  adorn_pct_formatting(columns = c(percent, porcentaje_acumulado))

# ------------------------------------------------------------------------------
# 2.4 RESIDENCIA (Tabla con summarise ordenada por frecuencia)
# ------------------------------------------------------------------------------

data %>%
  group_by(residencia) %>%
  summarise(
    frecuencia = n(),
    porcentaje = (n() / nrow(data)) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(frecuencia)) %>%
  bind_rows(
    tibble(
      residencia = "Total",
      frecuencia = nrow(data),
      porcentaje = 100
    )
  ) %>%
  kable()

# ------------------------------------------------------------------------------
# 2.5 ESCOLARIDAD (Variable ordinal con gt, sin acumulados)
# ------------------------------------------------------------------------------

tabyl(data, escolaridad) %>%
  adorn_pct_formatting() %>%
  gt() %>%
  tab_header(
    title = "Distribución de Escolaridad",
    subtitle = "Frecuencias y porcentajes"
  ) %>%
  cols_label(
    escolaridad = "Nivel de Escolaridad",
    n = "Frecuencia",
    percent = "Porcentaje"
  )

# ==============================================================================
# 3. GRÁFICAS DE BARRAS
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 SEXO (Gráfica de barras vertical)
# ------------------------------------------------------------------------------

ggplot(data, aes(x = sexo)) +
  geom_bar(fill = "#3498db", color = "white", linewidth = 0.5) +
  labs(
    title = "Distribución de Sexo",
    x = "Sexo",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.2 ESCOLARIDAD (Gráfica de barras vertical)
# ------------------------------------------------------------------------------

ggplot(data, aes(x = escolaridad)) +
  geom_bar(fill = "#9b59b6", color = "white", linewidth = 0.5) +
  labs(
    title = "Distribución de Escolaridad",
    x = "Nivel de Escolaridad",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ------------------------------------------------------------------------------
# 3.3 RESIDENCIA (Gráfica de barras horizontal)
# ------------------------------------------------------------------------------

ggplot(data, aes(y = residencia)) +
  geom_bar(fill = "#e74c3c", color = "white", linewidth = 0.5) +
  labs(
    title = "Distribución de Residencia",
    subtitle = "n = 153",
    x = "Frecuencia",
    y = "Residencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

# ------------------------------------------------------------------------------
# 3.4 RESIDENCIA (Gráfica de barras vertical ordenada por frecuencia con etiquetas)
# ------------------------------------------------------------------------------

residencia_freq <- data %>%
  count(residencia) %>%
  mutate(
    porcentaje = (n / sum(n)) * 100,
    residencia = fct_reorder(residencia, n, .desc = TRUE),
    etiqueta = paste0(n, " (", round(porcentaje, 1), "%)")
  )

ggplot(residencia_freq, aes(x = residencia, y = n, fill = residencia)) +
  geom_bar(stat = "identity", color = "white", linewidth = 0.7) +
  geom_text(aes(label = etiqueta), 
            vjust = -0.5, 
            fontface = "bold", 
            size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribución de Residencia",
    x = "Residencia",
    y = "Frecuencia"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# ==============================================================================
# 4. RESUMEN UNIVARIANTE DE > 1 VARIABLE
# ==============================================================================

data %>%
  select(disnea, dolor_toracico, adinamia, astenia, fatiga) %>%
  pivot_longer(cols = everything(), 
               names_to = "sintoma", 
               values_to = "respuesta") %>%
  count(sintoma, respuesta) %>%
  group_by(sintoma) %>%
  mutate(
    porcentaje = (n / sum(n)) * 100,
    sintoma = case_when(
      sintoma == "disnea" ~ "Disnea",
      sintoma == "dolor_toracico" ~ "Dolor torácico",
      sintoma == "adinamia" ~ "Adinamia",
      sintoma == "astenia" ~ "Astenia",
      sintoma == "fatiga" ~ "Fatiga"
    )
  ) %>%
  ungroup() %>%
  ggplot(aes(x = porcentaje, y = sintoma, fill = respuesta)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("No" = "#e74c3c", "Sí" = "#27ae60")) +
  labs(
    title = "Distribución de Síntomas de Alarma COVID-19",
    x = "Porcentaje (%)",
    y = "Síntoma",
    fill = "Respuesta"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )
