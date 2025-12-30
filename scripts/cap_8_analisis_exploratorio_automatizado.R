# ==============================================================================
# Análisis Exploratorio y Visualización de Datos con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Capítulo 8: Análisis Exploratorio Automatizado
# ==============================================================================

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(forcats)
library(summarytools)
library(modelsummary)
library(gtsummary)
library(broom)
library(gt)

# ==============================================================================
# 1. PREPARACIÓN DE LOS DATOS PARA EL ANÁLISIS
# ==============================================================================

data <- read_csv("data_covid.csv") %>%
  select(sexo, edad, imc, ferritina, procalcitonina, dimero_d, 
         dias_estancia_hospitalaria, clasificacion_sofa, pcr, has, 
         complicaciones, letalidad, dolor_toracico, adinamia, anosmia, 
         astenia, fatiga) %>%
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
    complicaciones = fct_recode(
      factor(complicaciones),
      "No" = "0",
      "Sí" = "1"
    ),
    letalidad = fct_recode(
      factor(letalidad),
      "Vivo" = "0",
      "Fallecido" = "1"
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
    anosmia = fct_recode(
      factor(anosmia),
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
# 2. ANÁLISIS DESCRIPTIVO CON GTSUMMARY
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Resumen del dataframe
# ------------------------------------------------------------------------------

data %>%
  tbl_summary(
    missing = "ifany"
  )

# ------------------------------------------------------------------------------
# 2.2 Resumen de subconjunto de síntomas
# ------------------------------------------------------------------------------

data %>%
  select(dolor_toracico, adinamia, anosmia, astenia, fatiga) %>%
  tbl_summary(
    missing = "ifany"
  )

# ------------------------------------------------------------------------------
# 2.2 Comparación de Variables por Complicaciones
# ------------------------------------------------------------------------------

data %>%
  tbl_summary(
    by = complicaciones,
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_overall()


data %>%
  tbl_summary(
    by = complicaciones,
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_overall() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Por Complicaciones**") %>%
  modify_spanning_header(stat_0 ~ "**Todos**")


# ==============================================================================
# 3. ANÁLISIS DESCRIPTIVO CON MODELSUMMARY
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Resumen de todas las variables
# ------------------------------------------------------------------------------

datasummary_skim(data)

# ------------------------------------------------------------------------------
# 3.2 Solo las variables categóricas
# ------------------------------------------------------------------------------

datasummary_skim(data, type = "categorical")

# ------------------------------------------------------------------------------
# 3.3 Solo las variables numéricas
# ------------------------------------------------------------------------------

datasummary_skim(data, type = "numeric")

# ------------------------------------------------------------------------------
# 3.4 Tabla de resumen para artículos de revista
# ------------------------------------------------------------------------------

datasummary_balance(~ 1, data = data)

# ------------------------------------------------------------------------------
# 3.5 Comparación de variables por complicaciones
# ------------------------------------------------------------------------------

datasummary_balance(~ complicaciones, data = data, dinm = FALSE)

# ==============================================================================
# 3. ANÁLISIS DESCRIPTIVO CON SUMMARYTOOLS
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1 Tabla de resumen para artículos de revista
# ------------------------------------------------------------------------------

dfSummary(data)

# ------------------------------------------------------------------------------
# 3.2 Análisis descriptivo univariante de datos numéricos
# ------------------------------------------------------------------------------

descr(data)

# ------------------------------------------------------------------------------
# 3.3 Análisis descriptivo univariante de datos categóricos
# ------------------------------------------------------------------------------

freq(data, report.nas = FALSE)

# ------------------------------------------------------------------------------
# 3.4 Visualización en formato html
# ------------------------------------------------------------------------------

view(freq(data), 
     collapse = TRUE, 
     cumul = FALSE, 
     totals = FALSE,
     report.nas = FALSE)

# ------------------------------------------------------------------------------
# 3.5 Comparación de variables por complicaciones
# ------------------------------------------------------------------------------

stby(data      = data, 
     INDICES   = data$complicaciones, 
     FUN       = descr, 
     stats     = c("mean", "sd", "med", "Q1", "Q3"), 
     transpose = TRUE)

stby(data      = data, 
     INDICES   = data$complicaciones, 
     FUN       = dfSummary, 
     transpose = TRUE)
