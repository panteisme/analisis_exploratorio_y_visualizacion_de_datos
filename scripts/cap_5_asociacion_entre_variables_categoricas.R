# ==============================================================================
# Análisis Exploratorio y Visualización de Datos con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Capítulo 5: Asociación entre Variables Categóricas
# ==============================================================================

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(forcats)
library(janitor)
library(ggplot2)
library(knitr)
library(gt)
library(summarytools)
library(ggpubr)
library(tidyr)
library(DescTools)
library(rstatix)

# ==============================================================================
# 1. PREPARACIÓN DE LOS DATOS PARA EL ANÁLISIS
# ==============================================================================

data <- read_csv("data_covid.csv") %>%
  select(has, clasificacion_sofa, sexo, estado_nutricional, fatiga) %>%
  mutate(
    sexo = fct_recode(
      factor(sexo),
      "Femenino" = "0",
      "Masculino" = "1"
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
    estado_nutricional = fct_recode(
      factor(estado_nutricional),
      "Normal" = "1",
      "Sobrepeso" = "2",
      "Obesidad I" = "3",
      "Obesidad II" = "4",
      "Obesidad III" = "5"
    ),
    fatiga = fct_recode(
      factor(fatiga),
      "No" = "0",
      "Sí" = "1"
    )
  )
# ==============================================================================
# 2. TABLAS DE 2 × 2: SEXO × HAS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Tabla cruzada con dplyr (frecuencias)
# ------------------------------------------------------------------------------

sexo_has_freq <- data %>%
  group_by(sexo, has) %>%
  summarise(frecuencia = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = has,
    values_from = frecuencia,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(-sexo)))

sexo_has_freq %>%
  kable()

# ------------------------------------------------------------------------------
# 2.2 Tabla cruzada con frecuencias y porcentajes de fila
# ------------------------------------------------------------------------------

sexo_has_tabla <- data %>%
  group_by(sexo, has) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(etiqueta = paste0(n, " (", pct, "%)")) %>%
  select(sexo, has, etiqueta) %>%
  pivot_wider(names_from = has, values_from = etiqueta)

sexo_has_tabla %>%
  kable(caption = "Tabla cruzada: Sexo × HAS (frecuencias y porcentajes de fila)")

# ------------------------------------------------------------------------------
# 2.3 Tabla cruzada con tabyl
# ------------------------------------------------------------------------------

t1 <- tabyl(data, sexo, has, show_na = FALSE)
t1

t1 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

# Versión con totales y porcentajes por columna
data %>%
  tabyl(sexo, has, show_na = FALSE) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

# ==============================================================================
# 2.4 Tabla con gt
# ==============================================================================

gt_tabla <- data %>%
  group_by(sexo, has) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sexo) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = has,
    values_from = c(n, pct),
    names_glue = "{has}_{.value}"
  ) %>%
  rename(
    Sexo = sexo,
    `No (n)` = No_n,
    `No (%)` = No_pct,
    `Sí (n)` = Sí_n,
    `Sí (%)` = Sí_pct
  )

gt(gt_tabla) %>%
  tab_header(
    title = "Tabla de Asociación: Sexo × HAS",
    subtitle = "Frecuencias y porcentajes por fila"
  ) %>%
  fmt_number(
    columns = c(`No (n)`, `Sí (n)`),
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(`No (%)`, `Sí (%)`),
    decimals = 1
  )

# ==============================================================================
# 2.5 Tabla con summarytools
# ==============================================================================

ctable(x = data$sexo, 
       y = data$has, 
       prop = "r")  

# ==============================================================================
# 3. ANÁLISIS GRÁFICO DE TABLAS DE 2 × 2
# ==============================================================================

tabla_asociacion <- data %>%
  count(sexo, has) %>%
  group_by(sexo) %>%
  mutate(
    total = sum(n),
    porcentaje = n / total * 100,
    etiqueta = paste0(n, " (", round(porcentaje, 1), "%)")
  )

ggplot(tabla_asociacion, aes(x = sexo, y = porcentaje, fill = has)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.5) +
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 3.5) +
  scale_fill_manual(
    values = c("No" = "#3498db", "Sí" = "#e74c3c"),
    name = "HAS"
  ) +
  labs(
    title = "Asociación entre Sexo e Hipertensión Arterial Sistémica",
    subtitle = "Distribución porcentual",
    x = "Sexo",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right"
  )

# Gráfica de asociación con assocplot
tabla_chi <- table(data$sexo, data$has)
assocplot(tabla_chi, main = "Gráfica de Asociación: Sexo × HAS")

# ==============================================================================
# 4. TABLAS R × C: CLASIFICACIÓN SOFA × FATIGA
# ==============================================================================

# Tabla cruzada: Clasificación SOFA × Fatiga
sofa_fatiga_tabla <- data %>%
  group_by(clasificacion_sofa, fatiga) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(clasificacion_sofa) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  mutate(etiqueta = paste0(n, " (", pct, "%)")) %>%
  select(clasificacion_sofa, fatiga, etiqueta) %>%
  pivot_wider(names_from = fatiga, values_from = etiqueta)

sofa_fatiga_tabla %>%
  kable(caption = "Tabla cruzada: Clasificación SOFA × Fatiga (frecuencias y porcentajes de fila)")

# ==============================================================================
# 5. GRÁFICA DE ASOCIACIÓN DE TABLAS R × C
# ==============================================================================

# Gráfica de asociación con ggplot2: Clasificación SOFA × Fatiga
tabla_asociacion_sofa_fatiga <- data %>%
  count(clasificacion_sofa, fatiga) %>%
  group_by(clasificacion_sofa) %>%
  mutate(
    total = sum(n),
    porcentaje = n / total * 100,
    etiqueta = paste0(n, " (", round(porcentaje, 1), "%)")
  )

ggplot(tabla_asociacion_sofa_fatiga, aes(x = clasificacion_sofa, y = porcentaje, fill = fatiga)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.5) +
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 3.5) +
  scale_fill_manual(
    values = c("No" = "#3498db", "Sí" = "#e74c3c"),
    name = "Fatiga"
  ) +
  labs(
    title = "Asociación entre Clasificación SOFA y Fatiga",
    subtitle = "Distribución porcentual",
    x = "Clasificación SOFA",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Gráfica con ggpubr
ggbarplot(tabla_asociacion_sofa_fatiga,
          x = "clasificacion_sofa",
          y = "porcentaje",
          fill = "fatiga",
          palette = "Set2") +
  labs(
    title = "Asociación entre Clasificación SOFA y Fatiga",
    subtitle = "Distribución porcentual (ggpubr)",
    x = "Clasificación SOFA",
    y = "Porcentaje (%)",
    fill = "Fatiga"
  )

# ==============================================================================
# 6. TABLA ESTRATIFICADA: CLASIFICACIÓN SOFA × FATIGA POR SEXO
# ==============================================================================

# Estratificado por Sexo = Femenino
tabyl(data %>% filter(sexo == "Femenino"), clasificacion_sofa, fatiga, show_na = FALSE) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>%
  kable(caption = "Tabla cruzada: Clasificación SOFA × Fatiga - Femenino")

# Estratificado por Sexo = Masculino
tabyl(data %>% filter(sexo == "Masculino"), clasificacion_sofa, fatiga, show_na = FALSE) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() %>%
  kable(caption = "Tabla cruzada: Clasificación SOFA × Fatiga - Masculino")

# Gráfica estratificada por sexo
tabla_sofa_fatiga_sexo <- data %>%
  count(clasificacion_sofa, fatiga, sexo) %>%
  group_by(clasificacion_sofa, sexo) %>%
  mutate(
    total = sum(n),
    porcentaje = n / total * 100,
    etiqueta = paste0(n, " (", round(porcentaje, 1), "%)")
  ) %>%
  ungroup()

ggplot(tabla_sofa_fatiga_sexo, aes(x = clasificacion_sofa, y = porcentaje, fill = fatiga)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 0.5) +
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 3) +
  scale_fill_manual(
    values = c("No" = "#3498db", "Sí" = "#e74c3c"),
    name = "Fatiga"
  ) +
  labs(
    title = "Asociación entre Clasificación SOFA y Fatiga estratificado por Sexo",
    subtitle = "Distribución porcentual",
    x = "Clasificación SOFA",
    y = "Porcentaje (%)"
  ) +
  facet_wrap(~sexo) +
  theme_pubr() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# ==============================================================================
# 7. MEDIDAS DE ASOCIACIÓN PARA TABLAS DE CONTINGENCIA
# ==============================================================================

# ------------------------------------------------------------------------------
# 7.1 Coeficiente Phi para tablas de 2*2
# ------------------------------------------------------------------------------

tabla1 <- data %>%
  tabyl(sexo, has)

DescTools::Phi(tabla1[, -1])

# ------------------------------------------------------------------------------
# 7.2 Coeficiente de Contingencia para tablas de 2*2
# ------------------------------------------------------------------------------

DescTools::ContCoef(tabla1[, -1])

# ------------------------------------------------------------------------------
# 7.3 V de Cramer para tablas de r*c
# ------------------------------------------------------------------------------

tabla2 <- data %>%
  tabyl(clasificacion_sofa, fatiga)

DescTools::CramerV(tabla2[, -1])

# Con rstatix
data %>%
  tabyl(clasificacion_sofa, fatiga) %>%
  select(-clasificacion_sofa) %>%
  cramer_v()

# ------------------------------------------------------------------------------
# 7.4 Coeficiente de Contingencia para tablas de r*c
# ------------------------------------------------------------------------------

# Con DescTools
DescTools::ContCoef(tabla2[, -1])

# ------------------------------------------------------------------------------
# 7.5 Gamma de Goodman-Kruskal para variables ordinales
# ------------------------------------------------------------------------------

tabla3 <- data %>%
  tabyl(estado_nutricional, clasificacion_sofa)

DescTools::GoodmanKruskalGamma(as.matrix(tabla3[, -1]))

# ------------------------------------------------------------------------------
# 7.6 Tau-b de Kendall para variables ordinales
# ------------------------------------------------------------------------------

DescTools::KendallTauB(as.matrix(tabla3[, -1]))
