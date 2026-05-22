# Cargar librerías necesarias
library(readr)
library(dplyr)
library(lubridate)
library(cluster)
library(factoextra)
library(rpart)
library(rpart.plot)

# 1. INGESTA DE DATOS Y CONFIGURACIÓN DEL ENTORNO
setwd("D:/Escritorio Windows BUENO/Escritorio/UNIR/TFM/Datasets internos Red Proyectum - Altadis/Data set Red Proyectum - Altadis")

fact_operations <- read_csv("Fact_Operations.csv")
dim_estancos <- read_csv("Dim_Affiliated_Outlets.csv") 
dim_clima <- read_csv("Dim_Weather.csv")                
dim_producto <- read_csv("Dim_Product.csv")            

# 2. TRANSFORMACIÓN DE DATOS Y REPLICACIÓN DE LA LÓGICA DE NEGOCIO
datos_preparados <- fact_operations %>%
  left_join(dim_estancos %>% select(Affiliated_Code, Provincia), 
            by = "Affiliated_Code")

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

dim_clima <- dim_clima %>%
  mutate(Clave_Provincia_Fecha = paste0(provincia, "_", as.character(Date)))

datos_preparados <- datos_preparados %>%
  left_join(dim_producto %>% select(Product_Code, Format), 
            by = "Product_Code")

# CONSOLIDACIÓN DE LA MATRIZ ANALÍTICA
datos_completos <- datos_preparados %>%
  left_join(dim_clima %>% select(Clave_Provincia_Fecha, Temp_Media, Precipitacion), 
            by = "Clave_Provincia_Fecha")

datos_completos <- na.omit(datos_completos)

# EXTRACCIÓN DE CARACTERÍSTICAS Y CONSTRUCCIÓN DE PERFILES OPERATIVOS
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
  filter(Tamaño_Local > 0)

# 5. PREPROCESAMIENTO MATEMÁTICO Y ESCALADO ALGORÍTMICO
datos_segmentacion <- perfiles_estancos %>% select(-Affiliated_Code)
datos_escalados <- scale(datos_segmentacion)

# 6. DIAGNÓSTICO DEL NÚMERO ÓPTIMO DE CLÚSTERES
grafico_codo <- fviz_nbclust(datos_escalados, kmeans, method = "wss") +
  labs(title = "Análisis de Varianza Intra-Clúster (Método del Codo)",
       x = "Número de Segmentos (k)",
       y = "Suma de Cuadrados (WSS)") +
  theme_minimal()

print(grafico_codo)

# 7. ENTRENAMIENTO DEL MODELO NO SUPERVISADO (K-MEANS)
set.seed(123) 
modelo_kmeans <- kmeans(datos_escalados, centers = 4, nstart = 25)

# 8. ASIGNACIÓN DE SEGMENTOS Y REDUCCIÓN DE DIMENSIONALIDAD VISUAL
perfiles_estancos$Cluster <- as.factor(modelo_kmeans$cluster)

grafico_clusters <- fviz_cluster(modelo_kmeans, data = datos_escalados,
                                 geom = "point",
                                 ellipse.type = "convex", 
                                 ggtheme = theme_minimal(),
                                 main = "Proyección Multivariante de Clústeres de Distribución")

print(grafico_clusters)

# 9. INTERPRETABILIDAD DEL MODELO (TÉCNICA AVANZADA DE REGLAS DE NEGOCIO)
# A. Análisis descriptivo de los centroides (Medias reales de cada grupo)
analisis_centroides <- perfiles_estancos %>%
  group_by(Cluster) %>%
  summarise(
    Volumen_Medio = mean(Tamaño_Local),
    Dias_Entrega_Medios = mean(Frecuencia_Pedidos),
    Tasa_Roturas_Media = mean(Tasa_Roturas),
    Estancos_En_Grupo = n()
  )
print("--- PERFIL MEDIO DE CADA CLÚSTER ---")
print(analisis_centroides)

# B. Generación del Árbol de Decisión para extraer reglas lógicas
arbol_reglas <- rpart(Cluster ~ Tamaño_Local + Frecuencia_Pedidos + Sensibilidad_Clima + Ventas_Festivos + Tasa_Roturas,
                      data = perfiles_estancos,
                      method = "class")

# Visualización del árbol de decisión
rpart.plot(arbol_reglas, 
           main = "Árbol de Decisión: Reglas de Asignación de Clústeres",
           extra = 104, # Muestra porcentajes y proporciones en los nodos
           box.palette = "Blues")

# 10. EXPORTACIÓN DE RESULTADOS PARA INTEGRACIÓN EN BUSINESS INTELLIGENCE
write_csv(perfiles_estancos, "Resultados_Segmentacion_KMeans.csv")
