# Análisis Exploratorio y Visualización de Datos con R

Repositorio oficial con materiales y scripts del curso "Análisis Exploratorio y Visualización de Datos con R" del canal de YouTube **Asesoría Estadística y Tesis**. Aprende a explorar, visualizar e interpretar datos mediante R con enfoque práctico y reproducible.

## 📋 Descripción

Este curso está orientado al desarrollo de competencias para la exploración, visualización e interpretación de datos mediante R. Se enfatiza el uso de herramientas del ecosistema tidyverse para crear análisis reproducibles, scripts documentados y flujos de trabajo estructurados en RStudio, con ejemplos aplicados a investigaciones en ciencias de la salud.

## 🎯 Objetivos

**Objetivo General**

Desarrollar habilidades para explorar, visualizar e interpretar datos mediante R, utilizando estrategias gráficas y descriptivas adecuadas según el tipo de variable y el objetivo analítico.

**Objetivos Específicos**

- Identificar el tipo de variables presentes en un conjunto de datos
- Aplicar técnicas de exploración y visualización adecuadas para variables categóricas y cuantitativas
- Interpretar patrones, asociaciones y diferencias entre variables
- Utilizar R como herramienta para el análisis exploratorio aplicado

---

## 📚 Contenido del Curso

El curso consta de **8 capítulos** organizados progresivamente:

| # | Tema | Descripción |
|----|------|-------------|
| 1 | Introducción al AED | Conceptos fundamentales, tipos de variables, escalas de medición |
| 2 | Repaso de R y RStudio | Paquetes, importación de datos, funciones básicas de dplyr |
| 3 | Variables categóricas | Tablas de frecuencias, gráficas de barras, análisis univariante |
| 4 | Variables cuantitativas | Estadísticas descriptivas, distribuciones, normalidad |
| 5 | Análisis bivariado (categórico) | Tablas de contingencia, medidas de asociación |
| 6 | Análisis bivariado (correlación) | Scatter plots, matrices de correlación, mapas de calor |
| 7 | Análisis de diferencia | Comparación entre grupos, boxplots, violin plots |
| 8 | Análisis automatizado | Tablas automáticas, reportes con gtsummary y summarytools |

## 🎥 Videos en YouTube

Cada capítulo tiene un video tutorial de aproximadamente **40 minutos**:
- Scripts ejecutados paso a paso
- Resultados inmediatos y su interpretación
- Enfoque práctico

[Ver lista de reproducción en YouTube](https://www.youtube.com/playlist?list=PLgAxL-lI4rQs6_73mMYW7t8MzP1lmz4vp)

## 📁 Estructura del Repositorio

```
├── scripts/
│   ├── cap_01_introduccion.R
│   ├── cap_02_repaso_operativo.R
│   ├── cap_03_variables_categoricas.R
│   ├── cap_04_variables_numericas.R
│   ├── cap_05_asociacion_categoricas.R
│   ├── cap_06_correlacion.R
│   ├── cap_07_analisis_diferencia.R
│   └── cap_08_automatizado.R
├── dataframes/
│   ├── data_covid.xlsx
│   ├── data_covid.dta
│   ├── data_covid.sav
│   ├── data_covid.csv
│   ├── data_covid.omv
│   ├── diccionario_variables_covid.html
│   └── diccionario_variables_covid.xlsx
├── programa/
│   └── Programa.pdf
├── materiales_complementarios/
│   └── (recursos complementarios)
└── README.md
```

## 💻 Requisitos

**Software**
- **R** versión 4.0 o superior: [Descargar](https://www.r-project.org/)
- **RStudio** (recomendado): [Descargar](https://posit.co/download/rstudio-desktop/)

**Paquetes Principales**

```r
# Instalación de paquetes necesarios
install.packages(c(
  # Ecosistema Tidyverse
  "readr", "dplyr", "tidyr", "forcats", "ggplot2", "here",
  
  # Visualización
  "ggpubr", "ggExtra", "GGally",
  
  # Tablas y reportes
  "knitr", "gt", "janitor", "gtsummary", "modelsummary", "summarytools",
  
  # Análisis estadístico
  "moments", "rstatix", "DescTools"
))
```

**Conocimientos previos**
- Conocimientos básicos de R y estadística descriptiva

## 📊 Dataset

El curso utiliza **data_covid** disponible en múltiples formatos para compatibilidad:
- **data_covid.csv** - Formato CSV (texto)
- **data_covid.xlsx** - Formato Excel
- **data_covid.sav** - Formato SPSS
- **data_covid.dta** - Formato Stata
- **data_covid.omv** - Formato Jamovi

Incluye:
- 153-200 observaciones de pacientes COVID-19
- 45+ variables clínicas
- Variables de múltiples tipos (nominal, ordinal, continua)

Se proporciona también **diccionario de variables** en formatos HTML y Excel para referencia rápida.

## 📖 Referencias

**Textos principales:**
- Wickham, H., & Grolemund, G. (2023). *R for Data Science* (2nd ed.). O'Reilly Media.
- Wilke, C. O. (2019). *Fundamentals of Data Visualization*. O'Reilly Media.
- Chang, W. (2018). *R Graphics Cookbook* (2nd ed.). O'Reilly Media.
- Schwabish, J. (2021). *Better Data Visualizations*. Columbia University Press.

**Recursos en línea:**
- [R Graph Gallery](https://r-graph-gallery.com/)
- [ggplot2 Documentation](https://ggplot2.tidyverse.org/)
- [tidyverse](https://www.tidyverse.org/)

## 🚀 Cómo Usar Este Repositorio

**1. Clonar o descargar el repositorio:**

```bash
git clone https://github.com/panteisme/analisis_exploratorio_y_visualizacion_de_datos.git
```

**2. Instalar paquetes necesarios**

Ejecuta el código de instalación mostrado en la sección Requisitos.

**3. Establecer directorio de trabajo en RStudio**

```r
setwd("ruta/a/analisis_exploratorio_y_visualizacion_de_datos")
```

**4. Explorar los scripts**

Abre los scripts en orden numérico (cap_01, cap_02, ..., cap_08) y sigue las demostraciones.

**5. Ver los videos en YouTube**

Sigue la [lista de reproducción oficial](https://www.youtube.com/playlist?list=PLgAxL-lI4rQs6_73mMYW7t8MzP1lmz4vp) mientras ejecutas el código.

**6. Experimentar y adaptar**

Modifica el código y adáptalo a tus propios datos para consolidar el aprendizaje.

## 💡 Metodología

- **Enfoque práctico:** 100% basado en código funcional y comentado
- **Interpretación:** Énfasis en entender resultados, no solo ejecutar código
- **Reproducible:** Todo el código es completamente reproducible

## 🤝 Contribuciones

Las contribuciones son bienvenidas. Por favor:

1. Haz un fork del proyecto
2. Crea una rama para tu característica (`git checkout -b feature/nueva-caracteristica`)
3. Commit tus cambios (`git commit -m 'Añadir nueva característica'`)
4. Push a la rama (`git push origin feature/nueva-caracteristica`)
5. Abre un Pull Request

## ⭐ Agradecimientos

Si este material te resulta útil, considera:

- ⭐ Dar una estrella al repositorio
- 📺 Suscribirte al canal de YouTube
- 📢 Compartir con colegas y estudiantes

## 📞 Contacto y Soporte

Para consultas sobre el curso:

- 💬 Comentarios en los videos de YouTube
- 📧 Email: panteisme@yahoo.com
- 🐛 Issues en este repositorio para reportar errores o sugerencias

## 📝 Licencia

MIT License - Ver archivo LICENSE para más detalles

## 👨‍🏫 Autor

**Prof. Andre Chocó-Cedillos**  
Universidad de San Carlos de Guatemala  
📺 YouTube: [Asesoría Estadística y Tesis](https://www.youtube.com/@asesoriaestadisticaytesis)  
📧 Email: panteisme@yahoo.com

---

**Versión:** 1.0 | **Última actualización:** Diciembre 2025
