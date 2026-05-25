# Segmentación No Supervisada de Puntos de Venta (K-Means)
# Objetivo: Agrupar la red logística en clústeres operativos basados en su 
# comportamiento transaccional y extraer reglas de negocio mediante árboles de decisión.

# Carga de librerías necesarias para el modelado no supervisado, visualización y reglas lógicas
library(readr)
library(dplyr)
library(lubridate)
library(cluster)
library(factoextra)
library(rpart)
library(rpart.plot)

# 1. Ingesta de datos y configuración del entorno de trabajo
# Definición del directorio de trabajo donde se alojan los datasets extraídos
setwd("D:/Escritorio Windows BUENO/Escritorio/UNIR/TFM/Datasets internos Red Proyectum - Altadis/Data set Red Proyectum - Altadis")

# Carga de la tabla de hechos transaccionales y las dimensiones de negocio
fact_operations <- read_csv("Fact_Operations.csv")
dim_estancos    <- read_csv("Dim_Affiliated_Outlets.csv") 
dim_clima       <- read_csv("Dim_Weather.csv")                
dim_producto    <- read_csv("Dim_Product.csv")            

# 2. Transformación de datos y replicación de la lógica de negocio de Power BI
# Integración de la dimensión geográfica mediante el código de afiliado
datos_preparados <- fact_operations %>%
  left_join(dim_estancos %>% select(Affiliated_Code, Provincia), 
            by = "Affiliated_Code")

# Normalización de nomenclaturas territoriales para asegurar la coherencia 
# con la limpieza ejecutada previamente mediante DAX en Power BI
datos_preparados <- datos_preparados %>%
  mutate(Provincia_normalizada = case_when(
    Provincia == "Vizcaya"      ~ "Bizkaia",
    Provincia == "Guipúzcoa"    ~ "Gipuzkoa",
    Provincia == "Álava"        ~ "Araba/Álava",
    Provincia == "Baleares"     ~ "Illes Balears",
    Provincia == "La Coruña"    ~ "A Coruña",
    Provincia == "Gerona"       ~ "Girona",
    TRUE                        ~ Provincia  
  )) %>%
  mutate(Clave_Provincia_Fecha = paste0(Provincia_normalizada, "_", as.character(Date)))

# Generación de la clave primaria compuesta en la dimensión climática para el cruce
dim_clima <- dim_clima %>%
  mutate(Clave_Provincia_Fecha = paste0(provincia, "_", as.character(Date)))

# Integración de la dimensión de producto para extraer la categoría de formato
datos_preparados <- datos_preparados %>%
  left_join(dim_producto %>% select(Product_Code, Format), 
            by = "Product_Code")

# 3. Consolidación de la matriz analítica
# Cruce final incorporando las variables meteorológicas (Temperatura y Precipitación)
datos_completos <- datos_preparados %>%
  left_join(dim_clima %>% select(Clave_Provincia_Fecha, Temp_Media, Precipitacion), 
            by = "Clave_Provincia_Fecha")

# Eliminación de registros con valores nulos para garantizar la estabilidad del algoritmo
datos_completos <- na.omit(datos_completos)

# 4. Extracción de características y construcción de perfiles operativos
# Transición de datos diarios transaccionales a un perfil agregado único por estanco
perfiles_estancos <- datos_completos %>%
  group_by(Affiliated_Code) %>%
  summarise(
    Tamaño_Local = sum(Sales_Uds, na.rm = TRUE),
    Frecuencia_Pedidos = sum(Delivery_Uds > 0, na.rm = TRUE),
    Sensibilidad_Clima = sd(Sales_Uds[Temp_Media > 25 | Temp_Media < 10], na.rm = TRUE),
    Ventas_Festivos = mean(Sales_Uds[National_holiday == 1], na.rm = TRUE),
    Tasa_Roturas = mean(as.numeric(OoS_Flag), na.rm = TRUE)
  ) %>%
  mutate(
    Sensibilidad_Clima = ifelse(is.na(Sensibilidad_Clima), 0, Sensibilidad_Clima),
    Ventas_Festivos = ifelse(is.na(Ventas_Festivos), 0, Ventas_Festivos)
  ) %>%
  # Filtrado de seguridad para excluir puntos de venta sin actividad registrada
  filter(Tamaño_Local > 0)

# 5. Preprocesamiento matemático y escalado algorítmico
# Aislamiento de variables numéricas y estandarización (Z-score) para evitar 
# que variables de gran magnitud (como ventas) eclipsen a las de menor magnitud (como roturas)
datos_segmentacion <- perfiles_estancos %>% select(-Affiliated_Code)
datos_escalados <- scale(datos_segmentacion)

# 6. Diagnóstico del número óptimo de clústeres
# Generación de la curva de varianza intra-clúster (Método del Codo)
grafico_codo <- fviz_nbclust(datos_escalados, kmeans, method = "wss") +
  labs(title = "Análisis de Varianza Intra-Clúster (Método del Codo)",
       x = "Número de Segmentos (k)",
       y = "Suma de Cuadrados (WSS)") +
  theme_minimal()

print(grafico_codo)

# 7. Entrenamiento del modelo no supervisado (K-Means)
# Fijación de semilla para reproducibilidad y partición matemática en 4 segmentos
set.seed(123) 
modelo_kmeans <- kmeans(datos_escalados, centers = 4, nstart = 25)

# 8. Asignación de segmentos y reducción de dimensionalidad visual
# Integración de la etiqueta de clúster al perfil original del estanco
perfiles_estancos$Cluster <- as.factor(modelo_kmeans$cluster)

# Proyección multivariante mediante Análisis de Componentes Principales (PCA)
grafico_clusters <- fviz_cluster(modelo_kmeans, data = datos_escalados,
                                 geom = "point",
                                 ellipse.type = "convex", 
                                 ggtheme = theme_minimal(),
                                 main = "Proyección Multivariante de Clústeres de Distribución")

print(grafico_clusters)

# 9. Interpretabilidad del modelo (Técnica avanzada de reglas de negocio)
# A. Análisis descriptivo de los centroides (Medias operativas reales de cada grupo)
analisis_centroides <- perfiles_estancos %>%
  group_by(Cluster) %>%
  summarise(
    Volumen_Medio = mean(Tamaño_Local),
    Dias_Entrega_Medios = mean(Frecuencia_Pedidos),
    Tasa_Roturas_Media = mean(Tasa_Roturas),
    Estancos_En_Grupo = n()
  )

cat("\nPERFIL MEDIO DE CADA CLÚSTER LOGÍSTICO\n")
print(analisis_centroides)

# B. Generación del Árbol de Decisión para extraer reglas lógicas de segmentación
# Esta técnica traduce las distancias geométricas del K-Means en reglas interpretables por negocio
arbol_reglas <- rpart(Cluster ~ Tamaño_Local + Frecuencia_Pedidos + Sensibilidad_Clima + Ventas_Festivos + Tasa_Roturas,
                      data = perfiles_estancos,
                      method = "class")

# Visualización del árbol de decisión con proporciones y umbrales de corte
rpart.plot(arbol_reglas, 
           main = "Árbol de Decisión: Reglas de Asignación de Clústeres",
           extra = 104, 
           box.palette = "Blues")

# 10. Exportación de resultados para integración en Inteligencia de Negocio
# Generación del archivo dimensional que actuará como maestro de estancos en Power BI
write_csv(perfiles_estancos, "Resultados_Segmentacion_KMeans.csv")
