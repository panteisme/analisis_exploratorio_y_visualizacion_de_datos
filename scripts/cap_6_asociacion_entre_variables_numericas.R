# ==============================================================================
# Análisis Exploratorio y Visualización de Datos con R
# Canal de Youtube: Asesoría Estadística y Tesis
# Autor: Profesor Andre Chocó-Cedillos
# Capítulo 6: Asociación entre Variables Numéricas - Correlación
# ==============================================================================

# Cargar librerías necesarias
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(ggpubr)
library(ggExtra)
library(GGally)
library(rstatix)

# ==============================================================================
# 1. PREPARACIÓN DE LOS DATOS PARA EL ANÁLISIS
# ==============================================================================

data <- read_csv("data_covid.csv") %>%
  select(edad, imc, dimero_d, ferritina, procalcitonina, pcr, sofa, 
         dias_estancia_hospitalaria, sexo) %>%
  mutate(
    sexo = fct_recode(
      factor(sexo),
      "Femenino" = "0",
      "Masculino" = "1"
    )
  )

# ==============================================================================
# 2. GRÁFICAS DE DISPERSIÓN SIMPLE
# ==============================================================================

# Gráfica 1: Días de estancia hospitalaria × PCR
data %>%
  ggplot(aes(dias_estancia_hospitalaria, pcr)) +
  geom_point() +
  labs(
    title = "Relación entre días de estancia hospitalaria y PCR",
    x = "Días de estancia hospitalaria",
    y = "PCR (mg/dL)"
  ) +
  theme_pubr()

# Gráfica 2: IMC × Días de estancia hospitalaria
data %>%
  ggplot(aes(imc, dias_estancia_hospitalaria)) +
  geom_point() +
  labs(
    title = "Relación entre IMC y Días de Estancia Hospitalaria",
    x = "IMC (kg/m²)",
    y = "Días de estancia"
  ) +
  theme_pubr()

# Gráfica 3: Edad × SOFA
data %>%
  ggplot(aes(edad, sofa)) +
  geom_point() +
  labs(
    title = "Relación entre Edad y SOFA",
    x = "Edad (años)",
    y = "SOFA"
  ) +
  theme_pubr()

# ==============================================================================
# 2.1 GRÁFICAS CON DISTRIBUCIONES MARGINALES
# ==============================================================================

# Gráfica 1: Días de estancia hospitalaria × PCR con densidades
p1 <- ggplot(data, aes(x = dias_estancia_hospitalaria, y = pcr)) +
  geom_point(color = "#3498db", size = 2) +
  labs(
    title = "Días de estancia hospitaria × PCR (con distribuciones marginales)",
    x = "Días de estancia hospitalaria",
    y = "PCR (mg/dL)"
  ) +
  theme_pubr()

ggMarginal(p1, type = "density", fill = "#3498db")

# Gráfica 2: IMC × Días de estancia con densidades
p2 <- ggplot(data, aes(x = imc, y = dias_estancia_hospitalaria)) +
  geom_point(color = "#e74c3c", size = 2) +
  labs(
    title = "IMC × Días de Estancia (con distribuciones marginales)",
    x = "IMC (kg/m²)",
    y = "Días de estancia"
  ) +
  theme_pubr()

ggMarginal(p2, type = "density", fill = "#e74c3c")

# Gráfica 3: Edad × SOFA
p3 <- ggplot(data, aes(x = edad, y = sofa)) +
  geom_point(color = "#2ecc71", size = 2) +
  labs(
    title = "Edad × SOFA (con distribuciones marginales)",
    x = "Edad (años)",
    y = "SOFA"
  ) +
  theme_pubr()

ggMarginal(p3, type = "density", fill = "#2ecc71")

# ==============================================================================
# 3. MATRIZ DE CORRELACIONES CON GGALLY
# ==============================================================================

# Matriz de dispersión simple
data %>%
  ggscatmat()

# Matriz de pares completa
data %>%
  ggpairs()

# ==============================================================================
# 4. COEFICIENTES DE CORRELACIÓN CON RSTATIX
# ==============================================================================

# Correlación de todas las variables
data %>%
  cor_test() %>%
  filter(var1 < var2)

# Ver solo pares únicos (sin repeticiones)
data %>%
  cor_test() %>%
  filter(var1 < var2) %>%
  select(var1, var2, cor) %>%
  print(n = Inf)

# Correlaciones de IMC contra todas las demás
data %>%
  cor_test(
    vars = dias_estancia_hospitalaria,
    vars2 = c(edad, imc, dimero_d, ferritina, procalcitonina, pcr, sofa),
    method = "spearman"
  )

# Correlaciones de PCR contra otras variables
data %>%
  cor_test(
    vars = pcr,
    vars2 = c(edad, imc, dimero_d, ferritina, procalcitonina, sofa,
              dias_estancia_hospitalaria),
    method = "spearman"
  )

# ==============================================================================
# 5. MAPAS DE CALOR (HEATMAPS) DE CORRELACIONES
# ==============================================================================

# Calcular matriz de correlaciones
matriz_cor <- data %>%
  select(everything()) %>%
  cor(use = "complete.obs")

# Convertir a formato largo para ggplot
matriz_cor_larga <- matriz_cor %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable1") %>%
  pivot_longer(
    cols = -Variable1,
    names_to = "Variable2",
    values_to = "Correlacion"
  )

# Mapa de calor básico
ggplot(matriz_cor_larga, aes(x = Variable1, y = Variable2, fill = Correlacion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "#e74c3c",
    mid = "white",
    high = "#3498db",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  geom_text(aes(label = round(Correlacion, 2)), 
            color = "black", 
            size = 3) +
  labs(
    title = "Matriz de Correlaciones de Pearson",
    x = "Variables",
    y = "Variables",
    fill = "Correlación"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  )

# Mapa de calor con reordenamiento jerárquico
# Calcular distancias
distancia <- as.dist(1 - abs(matriz_cor))
orden_jerarquico <- hclust(distancia)$order

# Reordenar matriz
variables_ordenadas <- colnames(matriz_cor)[orden_jerarquico]

matriz_cor_larga_ordenada <- matriz_cor_larga %>%
  mutate(
    Variable1 = factor(Variable1, levels = variables_ordenadas),
    Variable2 = factor(Variable2, levels = variables_ordenadas)
  )

ggplot(matriz_cor_larga_ordenada, aes(x = Variable1, y = Variable2, fill = Correlacion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient2(
    low = "#e74c3c",
    mid = "white",
    high = "#3498db",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  geom_text(aes(label = round(Correlacion, 2)), 
            color = "black", 
            size = 3) +
  labs(
    title = "Matriz de Correlaciones (Reordenada Jerárquicamente)",
    x = "Variables",
    y = "Variables",
    fill = "Correlación"
  ) +
  theme_pubr() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14)
  )

# ==============================================================================
# 6. GRÁFICOS DE DISPERSIÓN ESTRATIFICADOS
# ==============================================================================

ggplot(data, aes(x = imc, y = dias_estancia_hospitalaria)) +
  geom_point(color = "#e74c3c", size = 2) +
  labs(
    title = "IMC × Días de Estancia Hospitalaria por Sexo",
    x = "IMC (kg/m²)",
    y = "Días de estancia"
  ) +
  facet_wrap(~sexo) +
  theme_pubr()
