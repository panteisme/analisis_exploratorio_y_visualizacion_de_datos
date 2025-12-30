# ==============================================================================
# Análisis Exploratorio y Visualización de Datos con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Capítulo 4: Análisis Univariante de Variables Numéricas
# ==============================================================================

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(knitr)
library(gt)
library(moments)

# ==============================================================================
# 1. PREPARACIÓN DE LOS DATOS PARA EL ANÁLISIS
# ==============================================================================

data <- read_csv("data_covid.csv") %>%
  select(edad, imc, dimero_d, ferritina, procalcitonina, pcr, 
         lactato, dias_estancia_hospitalaria)

# ==============================================================================
# 2. RESUMEN DESCRIPTIVO DE VARIABLES NUMÉRICAS
# ==============================================================================

# Versión con knitr
data %>%
  summarise(
    across(
      everything(),
      list(
        Media = ~mean(., na.rm = TRUE),
        DE = ~sd(., na.rm = TRUE),
        Mediana = ~median(., na.rm = TRUE),
        Mínimo = ~min(., na.rm = TRUE),
        Máximo = ~max(., na.rm = TRUE),
        Q1 = ~quantile(., 0.25, na.rm = TRUE),
        Q3 = ~quantile(., 0.75, na.rm = TRUE),
        Asimetría = ~moments::skewness(., na.rm = TRUE),
        Curtosis = ~moments::kurtosis(., na.rm = TRUE)
      ),
      .names = "{.col}___{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "estadístico"),
    names_sep = "___"
  ) %>%
  pivot_wider(
    names_from = estadístico,
    values_from = value
  ) %>%
  kable(digits = 2)

# Versión con gt
summary_stats <- data %>%
  summarise(
    across(
      everything(),
      list(
        Media = ~mean(., na.rm = TRUE),
        DE = ~sd(., na.rm = TRUE),
        Mediana = ~median(., na.rm = TRUE),
        Mínimo = ~min(., na.rm = TRUE),
        Máximo = ~max(., na.rm = TRUE),
        Q1 = ~quantile(., 0.25, na.rm = TRUE),
        Q3 = ~quantile(., 0.75, na.rm = TRUE),
        Asimetría = ~moments::skewness(., na.rm = TRUE),
        Curtosis = ~moments::kurtosis(., na.rm = TRUE)
      ),
      .names = "{.col}___{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "estadístico"),
    names_sep = "___"
  ) %>%
  pivot_wider(
    names_from = estadístico,
    values_from = value
  ) %>%
  gt() %>%
  tab_header(
    title = "Resumen Descriptivo de Variables Numéricas"
  ) %>%
  cols_label(
    variable = "Variable",
    Media = "Media",
    DE = "Desv. Estándar",
    Mediana = "Mediana",
    Mínimo = "Mínimo",
    Máximo = "Máximo",
    Q1 = "Q1",
    Q3 = "Q3",
    Asimetría = "Asimetría",
    Curtosis = "Curtosis"
  ) %>%
  fmt_number(columns = 2:10, decimals = 2)

summary_stats

# ==============================================================================
# 3. GRÁFICAS UNIVARIANTES
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 EDAD: Histograma con densidad
# ------------------------------------------------------------------------------

ggplot(data, aes(x = edad)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#3498db", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#e74c3c", 
               linewidth = 1.2) +
  labs(
    title = "Distribución de Edad",
    x = "Edad (años)",
    y = "Densidad"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.2 EDAD: Boxplot
# ------------------------------------------------------------------------------

ggplot(data, aes(x = "", y = edad)) +
  geom_boxplot(fill = "#3498db", alpha = 0.7, 
               outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.5, 
              color = "#34495e", size = 2) +
  labs(
    title = "Distribución de Edad",
    x = "",
    y = "Edad (años)"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.3 EDAD: QQ Plot
# ------------------------------------------------------------------------------

ggplot(data, aes(sample = edad)) +
  stat_qq(color = "#3498db", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Edad",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.4 IMC: Histograma con densidad
# ------------------------------------------------------------------------------

ggplot(data, aes(x = imc)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#9b59b6", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#e74c3c", 
               linewidth = 1.2) +
  labs(
    title = "Distribución del IMC",
    x = "IMC (kg/m²)",
    y = "Densidad"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.5 IMC: Boxplot
# ------------------------------------------------------------------------------

ggplot(data, aes(x = "", y = imc)) +
  geom_boxplot(fill = "#9b59b6", alpha = 0.7, 
               outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.5, 
              color = "#34495e", size = 2) +
  labs(
    title = "Distribución del IMC",
    x = "",
    y = "IMC (kg/m²)"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.6 IMC: QQ Plot
# ------------------------------------------------------------------------------

ggplot(data, aes(sample = imc)) +
  stat_qq(color = "#9b59b6", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - IMC",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.7 DÍMERO D: Histograma con densidad
# ------------------------------------------------------------------------------

ggplot(data, aes(x = dimero_d)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#e74c3c", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#3498db", 
               linewidth = 1.2) +
  labs(
    title = "Distribución del Dímero D",
    x = "Dímero D",
    y = "Densidad"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.8 DÍMERO D: Boxplot
# ------------------------------------------------------------------------------

ggplot(data, aes(x = "", y = dimero_d)) +
  geom_boxplot(fill = "#e74c3c", alpha = 0.7, 
               outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.5, 
              color = "#34495e", size = 2) +
  labs(
    title = "Distribución del Dímero D",
    x = "",
    y = "Dímero D"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.9 DÍMERO D: QQ Plot
# ------------------------------------------------------------------------------

ggplot(data, aes(sample = dimero_d)) +
  stat_qq(color = "#e74c3c", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#3498db", linewidth = 1) +
  labs(
    title = "QQ Plot - Dímero D",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.10 FERRITINA: Histograma con densidad
# -----------------------------------------------------------------------

ggplot(data, aes(x = ferritina)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#f39c12", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#e74c3c", 
               linewidth = 1.2) +
  labs(
    title = "Distribución de Ferritina",
    x = "Ferritina",
    y = "Densidad"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.11 FERRITINA: Boxplot
# -----------------------------------------------------------------------

ggplot(data, aes(x = "", y = ferritina)) +
  geom_boxplot(fill = "#f39c12", alpha = 0.7, 
               outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.5, 
              color = "#34495e", size = 2) +
  labs(
    title = "Distribución de Ferritina",
    x = "",
    y = "Ferritina"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.12 FERRITINA: QQ Plot
# -----------------------------------------------------------------------

ggplot(data, aes(sample = ferritina)) +
  stat_qq(color = "#f39c12", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Ferritina",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.13 DÍAS ESTANCIA HOSPITALARIA: Histograma
# -----------------------------------------------------------------------

ggplot(data, aes(x = dias_estancia_hospitalaria)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 15, 
                 fill = "#27ae60", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#e74c3c", 
               linewidth = 1.2) +
  labs(
    title = "Distribución Días Estancia Hospitalaria",
    x = "Días",
    y = "Densidad"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.14 DÍAS ESTANCIA HOSPITALARIA: Boxplot
# -----------------------------------------------------------------------

ggplot(data, aes(x = "", y = dias_estancia_hospitalaria)) +
  geom_boxplot(fill = "#27ae60", alpha = 0.7, 
               outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.5, 
              color = "#34495e", size = 2) +
  labs(
    title = "Distribución Días Estancia Hospitalaria",
    x = "",
    y = "Días"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ------------------------------------------------------------------------------
# 3.15 DÍAS ESTANCIA HOSPITALARIA: QQ Plot
# -----------------------------------------------------------------------

ggplot(data, aes(sample = dias_estancia_hospitalaria)) +
  stat_qq(color = "#27ae60", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#e74c3c", linewidth = 1) +
  labs(
    title = "QQ Plot - Días Estancia Hospitalaria",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

# ==============================================================================
# 4. ESTANDARIZACIÓN DE VARIABLES Y COMPARACIÓN
# ==============================================================================

data_estandarizado <- data %>%
  mutate(
    dimero_d_std = scale(dimero_d)[, 1],
    ferritina_std = scale(ferritina)[, 1],
    procalcitonina_std = scale(procalcitonina)[, 1],
    pcr_std = scale(pcr)[, 1],
    lactato_std = scale(lactato)[, 1]
  ) %>%
  select(dimero_d_std, ferritina_std, procalcitonina_std, 
         pcr_std, lactato_std)

# Gráfico de histogramas de variables estandarizadas
data_estandarizado %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor_estandarizado"
  ) %>%
  mutate(
    variable = case_when(
      variable == "dimero_d_std" ~ "Dímero D",
      variable == "ferritina_std" ~ "Ferritina",
      variable == "procalcitonina_std" ~ "Procalcitonina",
      variable == "pcr_std" ~ "PCR",
      variable == "lactato_std" ~ "Lactato"
    )
  ) %>%
  ggplot(aes(x = valor_estandarizado, fill = variable)) +
  geom_histogram(bins = 15, color = "white", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(
    values = c(
      "Dímero D" = "#e74c3c",
      "Ferritina" = "#f39c12",
      "Procalcitonina" = "#3498db",
      "PCR" = "#9b59b6",
      "Lactato" = "#27ae60"
    )
  ) +
  labs(
    title = "Distribución de Variables Estandarizadas",
    subtitle = "Variables normalizadas (z-score)",
    x = "Valor Estandarizado",
    y = "Frecuencia",
    fill = "Variable"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "none"
  )

# Gráfico de densidad de variables estandarizadas
data_estandarizado %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor_estandarizado"
  ) %>%
  mutate(
    variable = case_when(
      variable == "dimero_d_std" ~ "Dímero D",
      variable == "ferritina_std" ~ "Ferritina",
      variable == "procalcitonina_std" ~ "Procalcitonina",
      variable == "pcr_std" ~ "PCR",
      variable == "lactato_std" ~ "Lactato"
    )
  ) %>%
  ggplot(aes(x = valor_estandarizado, fill = variable)) +
  geom_density(alpha = 0.6, color = "white", linewidth = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(
    values = c(
      "Dímero D" = "#e74c3c",
      "Ferritina" = "#f39c12",
      "Procalcitonina" = "#3498db",
      "PCR" = "#9b59b6",
      "Lactato" = "#27ae60"
    )
  ) +
  labs(
    title = "Densidad de Variables Estandarizadas",
    subtitle = "Variables normalizadas (z-score)",
    x = "Valor Estandarizado",
    y = "Densidad",
    fill = "Variable"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "none"
  )

# Gráfico de boxplots de variables estandarizadas
data_estandarizado %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor_estandarizado"
  ) %>%
  mutate(
    variable = case_when(
      variable == "dimero_d_std" ~ "Dímero D",
      variable == "ferritina_std" ~ "Ferritina",
      variable == "procalcitonina_std" ~ "Procalcitonina",
      variable == "pcr_std" ~ "PCR",
      variable == "lactato_std" ~ "Lactato"
    )
  ) %>%
  ggplot(aes(x = variable, y = valor_estandarizado, 
             fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, 
               outlier.size = 2) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  scale_fill_manual(
    values = c(
      "Dímero D" = "#e74c3c",
      "Ferritina" = "#f39c12",
      "Procalcitonina" = "#3498db",
      "PCR" = "#9b59b6",
      "Lactato" = "#27ae60"
    )
  ) +
  labs(
    title = "Boxplots de Variables Estandarizadas",
    subtitle = "Variables normalizadas (z-score)",
    x = "Variable",
    y = "Valor Estandarizado",
    fill = "Variable"
  ) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
