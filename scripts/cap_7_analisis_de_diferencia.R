# ==============================================================================
# Análisis Exploratorio y Visualización de Datos con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Capítulo 7: Análisis de Diferencia
# ==============================================================================
# Cargar librerías necesarias
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(knitr)
library(gt)
library(ggpubr)
library(tidyr)

# ==============================================================================
# 1. PREPARACIÓN DE LOS DATOS PARA EL ANÁLISIS
# ==============================================================================
data <- read_csv("data_covid.csv") %>%
  select(plaquetas, imc, edad, dias_estancia_hospitalaria, complicaciones, has, 
         clasificacion_sofa, curb65, sofa, ferritina) %>%
  mutate(
    plaquetas = as.numeric(plaquetas),
    imc = as.numeric(imc),
    edad = as.numeric(edad),
    dias_estancia_hospitalaria = as.numeric(dias_estancia_hospitalaria),
    ferritina = as.numeric(ferritina),
    complicaciones = fct_recode(
      factor(complicaciones),
      "No" = "0",
      "Sí" = "1"
    ),
    has = fct_recode(
      factor(has),
      "No" = "0",
      "Sí" = "1"
    ),
    clasificacion_sofa = fct_recode(
      factor(clasificacion_sofa),
      "Sin fallo orgánico" = "0",
      "Disfunción orgánica" = "1",
      "Fallo orgánico" = "2"
    ),
    curb65 = as.numeric(curb65),
    sofa = as.numeric(sofa)
  )

# ==============================================================================
# 2. COMPARACIÓN DE UNA VARIABLE NUMÉRICA ENTRE DOS GRUPOS
# ==============================================================================

# 2.1 Tabla de comparaciones de Ferritina según complicaciones
data %>%
  group_by(complicaciones) %>%
  summarize(
    n = n(),
    Mediana = median(ferritina, na.rm = TRUE),
    Q1 = quantile(ferritina, probs = 0.25, na.rm = TRUE),
    Q3 = quantile(ferritina, probs = 0.75, na.rm = TRUE),
    Media = mean(ferritina, na.rm = TRUE),
    DE = sd(ferritina, na.rm = TRUE)
  )

# 2.2 Boxplot: Ferritina según complicaciones
data %>%
  ggplot(aes(x = complicaciones, y = ferritina, fill = complicaciones)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("No" = "#3498db", "Sí" = "#e74c3c")) +
  labs(
    title = "Ferritina según Complicaciones",
    x = "Complicaciones",
    y = "Ferritina (ng/mL)"
  ) +
  theme_minimal()

# 2.3 Histograma: Ferritina según complicaciones
data %>%
  ggplot(aes(x = ferritina, fill = complicaciones)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~complicaciones) +
  scale_fill_manual(values = c("No" = "#3498db", "Sí" = "#e74c3c")) +
  labs(
    title = "Distribución de Ferritina según Complicaciones",
    x = "Ferritina (ng/mL)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# 2.4 Tabla de Edad según HAS con kable
data %>%
  group_by(has) %>%
  summarize(
    n = n(),
    Mediana = median(edad, na.rm = TRUE),
    Q1 = quantile(edad, probs = 0.25, na.rm = TRUE),
    Q3 = quantile(edad, probs = 0.75, na.rm = TRUE),
    Media = mean(edad, na.rm = TRUE),
    DE = sd(edad, na.rm = TRUE)
  ) %>%
  kable()

# 2.5 Tabla de Edad según HAS con paquete gt
data %>%
  group_by(has) %>%
  summarize(
    n = n(),
    Mediana = median(edad, na.rm = TRUE),
    Q1 = quantile(edad, probs = 0.25, na.rm = TRUE),
    Q3 = quantile(edad, probs = 0.75, na.rm = TRUE),
    Media = mean(edad, na.rm = TRUE),
    DE = sd(edad, na.rm = TRUE)
  ) %>%
  gt() %>%
  tab_header(
    title = "Edad según HAS",
    subtitle = "Estadística descriptiva"
  ) %>%
  fmt_number(columns = c(Mediana, Q1, Q3, Media, DE), decimals = 2)

# 2.6 Gráfica de Edad según HAS con ggpubr
data %>%
  ggplot(aes(x = has, y = edad, fill = has)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("No" = "#3498db", "Sí" = "#e74c3c")) +
  labs(
    title = "Edad según HAS",
    x = "HAS",
    y = "Edad (años)"
  ) +
  theme_pubr()

# ==============================================================================
# 3. COMPARACIÓN DE UNA VARIABLE NUMÉRICA ENTRE K GRUPOS
# ==============================================================================

# 3.1 Tabla de Días de Estancia Hospitalaria según Clasificación SOFA
data %>%
  group_by(clasificacion_sofa) %>%
  summarize(
    n = n(),
    Mediana = median(dias_estancia_hospitalaria, na.rm = TRUE),
    Q1 = quantile(dias_estancia_hospitalaria, probs = 0.25, na.rm = TRUE),
    Q3 = quantile(dias_estancia_hospitalaria, probs = 0.75, na.rm = TRUE),
    Media = mean(dias_estancia_hospitalaria, na.rm = TRUE),
    DE = sd(dias_estancia_hospitalaria, na.rm = TRUE)
  ) %>%
  kable()

# 3.2 Boxplot: Días de Estancia según Clasificación SOFA
data %>%
  ggplot(aes(x = clasificacion_sofa, y = dias_estancia_hospitalaria, fill = clasificacion_sofa)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  scale_fill_manual(values = c("Sin fallo orgánico" = "#3498db", "Disfunción orgánica" = "#f39c12", "Fallo orgánico" = "#e74c3c")) +
  labs(
    title = "Días de Estancia Hospitalaria según Clasificación SOFA",
    x = "Clasificación SOFA",
    y = "Días de Estancia"
  ) +
  theme_minimal()

# 3.3 Violin plot: Días de Estancia según Clasificación SOFA
data %>%
  ggplot(aes(x = clasificacion_sofa, y = dias_estancia_hospitalaria, fill = clasificacion_sofa)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = c("Sin fallo orgánico" = "#3498db", "Disfunción orgánica" = "#f39c12", "Fallo orgánico" = "#e74c3c")) +
  labs(
    title = "Distribución de Días de Estancia según Clasificación SOFA",
    x = "Clasificación SOFA",
    y = "Días de Estancia"
  ) +
  theme_pubr()

# ==============================================================================
# 4. BOXPLOT CON FACET WRAP
# ==============================================================================

# 4.1 Boxplot: Días de Estancia según Complicaciones con facet wrap por HAS
data %>%
  ggplot(aes(x = complicaciones, y = dias_estancia_hospitalaria, fill = complicaciones)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  facet_wrap(~has) +
  scale_fill_manual(values = c("No" = "#3498db", "Sí" = "#e74c3c")) +
  labs(
    title = "Días de Estancia Hospitalaria según Complicaciones y HAS",
    x = "Complicaciones",
    y = "Días de Estancia"
  ) +
  theme_pubr()

# ==============================================================================
