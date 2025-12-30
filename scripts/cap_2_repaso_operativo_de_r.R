# ==============================================================================
# Análisis Exploratorio y Visualización de Datos con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Capítulo 2: Repaso Operativo de R
# ==============================================================================

# ==============================================================================
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# ==============================================================================

# Instalar paquetes (ejecutar solo una vez)
install.packages("readr")
install.packages("haven")
install.packages("readxl")
install.packages("jmvReadWrite")

# Instalar múltiples paquetes a la vez
install.packages(c("readr", "haven", "readxl", "jmvReadWrite"))

# Cargar paquetes (ejecutar cada vez que inicies R)
library(readr)
library(haven)
library(readxl)
library(jmvReadWrite)

# Verificar si los paquetes están instalados y cargarlos
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

# ==============================================================================
# 2. IMPORTAR Y VISUALIZAR DATOS IMPORTADOS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Archivos CSV
# ------------------------------------------------------------------------------

# Importar archivo CSV de forma directa
data <- read_csv("data_covid.csv")

# Importar archivo CSV usando readr::read_csv()
data <- readr::read_csv("data_covid.csv")

# Visualizar el dataframe completo
View(data)

# Ver primeras filas
head(data)
head(data, 10)

# Ver estructura del dataframe
str(data)

# Ver dimensiones (filas y columnas)
dim(data)
nrow(data)
ncol(data)

# Ver nombres de variables
names(data)
colnames(data)

# Ver información del dataframe con glimpse
dplyr::glimpse(data)

# ------------------------------------------------------------------------------
# 2.2 Archivos SPSS
# ------------------------------------------------------------------------------

# Abrir archivo SPSS
data_spss <- haven::read_sav("data_covid.sav")

# Visualizar
View(data_spss)

# ------------------------------------------------------------------------------
# 2.3 Archivos Excel
# ------------------------------------------------------------------------------

# Abrir archivo Excel
data_excel <- readxl::read_excel("data_covid.xlsx")

# Visualizar
View(data_excel)

# ------------------------------------------------------------------------------
# 2.4 Archivos Stata
# ------------------------------------------------------------------------------

# Abrir archivo Stata
data_stata <- haven::read_stata("data_covid.dta")

# Visualizar
View(data_stata)

# ------------------------------------------------------------------------------
# 2.5 Archivos Jamovi
# ------------------------------------------------------------------------------

# Abrir archivo Jamovi
data_jamovi <- jmvReadWrite::read_omv("data_covid.omv")

# Visualizar
View(data_jamovi)

# ==============================================================================
# 3. FUNCIONES BÁSICAS CON DPLYR
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# 3.1 filter() - Filtrar observaciones
# ------------------------------------------------------------------------------

# Filtrar pacientes mayores de 50 años
data %>%
  filter(edad > 50)

# Filtrar pacientes con HAS y mayores de 50 años y contar
data %>%
  filter(has == "1" & edad > 50) %>%
  count()

# ------------------------------------------------------------------------------
# 3.2 arrange() - Ordenar datos
# ------------------------------------------------------------------------------

# Ordenar por IMC > 30 y ordenar por sexo y luego por edad
data %>%
  filter(imc > 39.9) %>%
  arrange(sexo, age = edad) %>%
  View()

# Filtrar por complicaciones, ordenar edad ascendente y visualizar
data %>%
  filter(letalidad == 1) %>%
  arrange(edad) %>%
  View()

# ------------------------------------------------------------------------------
# 3.3 select() - Seleccionar variables
# ------------------------------------------------------------------------------

# Seleccionar edad y complicaciones, crear rangos de edad y cruzar
data %>%
  select(edad, complicaciones) %>%
  mutate(
    rango_edad = cut(edad, 
                     breaks = seq(0, 100, 10),
                     labels = paste0(seq(0, 90, 10), "-", seq(10, 100, 10)),
                     right = FALSE)
  ) %>%
  count(rango_edad, complicaciones)

# Seleccionar las variables procalcitonina, dímero D y ferritina; calcular media y desviación estándar
tibble::tibble(
  Variable = c("Procalcitonina", "Dímero D", "Ferritina"),
  Media = c(
    mean(data$procalcitonina, na.rm = TRUE),
    mean(data$dimero_d, na.rm = TRUE),
    mean(data$ferritina, na.rm = TRUE)
  ),
  "Desv. Est." = c(
    sd(data$procalcitonina, na.rm = TRUE),
    sd(data$dimero_d, na.rm = TRUE),
    sd(data$ferritina, na.rm = TRUE)
  )
)

# ==============================================================================
# 4. BORRAR MEMORIA Y CONSOLA
# ==============================================================================

# Borrar todos los objetos de la memoria
rm(list = ls())

# Borrar la consola
cat("\014") # O en Windows Ctrl + L
