# Predicción de Demanda Logística (Random Forest y Regresión Lineal)
# Objetivo: Estimar el volumen de ventas diario a nivel de punto de venta 
# utilizando modelos de regresión múltiple y algoritmos basados en árboles de decisión.

# Carga de librerías necesarias para la manipulación de datos y modelado predictivo
library(readr)
library(dplyr)
library(lubridate)
library(randomForest)
library(caret)
library(ranger)

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

# 4. Preparación de variables predictoras y tipificación de datos
# Tipificado de variables categóricas y temporales requeridas para el modelado algorítmico
datos_demanda <- datos_completos %>%
  mutate(
    Date = ymd(Date),
    Mes = as.numeric(month(Date)),      
    Dia_Semana = as.factor(wday(Date)),
    OoS_Flag = as.factor(OoS_Flag),
    Route_Flag = as.factor(Route_Flag),
    National_holiday = as.factor(National_holiday),
    Format = as.factor(Format)
  )

# Selección del vector de variables dependientes e independientes para la regresión
datos_demanda <- datos_demanda %>% select(
  Sales_Uds, Mes, Dia_Semana, Delivery_Uds, OoS_Flag, Route_Flag, 
  National_holiday, Temp_Media, Precipitacion, Format
)

# 5. Partición temporal del conjunto de datos (Entrenamiento y Validación)
# División estricta para evitar fuga de información y simular la operativa real del negocio
# Entrenamiento: meses de primavera y verano
train <- datos_demanda %>% filter(Mes %in% c(3, 4, 5, 6, 7, 8)) %>% select(-Mes)
# Validación (Test): meses de otoño
test  <- datos_demanda %>% filter(Mes %in% c(9, 10)) %>% select(-Mes)

# 6. Entrenamiento de modelos predictivos
# Definición del modelo base o "Baseline" (Regresión Lineal Múltiple)
modelo_lm <- lm(Sales_Uds ~ ., data = train)
pred_lm <- predict(modelo_lm, newdata = test)

# Liberación de memoria (Garbage Collection) para optimizar el rendimiento computacional
gc() 

# Entrenamiento del modelo avanzado (Random Forest) a través de la librería optimizada ranger
modelo_rf <- ranger(
  formula = Sales_Uds ~ ., 
  data = train, 
  num.trees = 50, 
  importance = 'impurity' 
)

# Extracción de las predicciones generadas por el modelo Random Forest
pred_rf <- predict(modelo_rf, data = test)$predictions

# 7. Evaluación de rendimiento y extracción de métricas predictivas
# Cálculo de errores absolutos (MAE) y cuadráticos (RMSE, R2) para ambos modelos
metricas_lm <- postResample(pred = pred_lm, obs = test$Sales_Uds)
metricas_rf <- postResample(pred = pred_rf, obs = test$Sales_Uds)

# Impresión en consola de las métricas de evaluación
print(metricas_lm)
print(metricas_rf)

# Visualización de la reducción de impureza y jerarquía predictiva (Feature Importance)
importancia <- modelo_rf$variable.importance
barplot(sort(importancia, decreasing = TRUE), 
        main = "Importancia Predictiva de las Variables", 
        las = 2, 
        col = "steelblue", 
        cex.names = 0.65)

# 8. Exportación de resultados para su integración en Inteligencia de Negocio
# Reconstrucción de la tabla de hechos transaccionales recuperando las claves logísticas
claves_test <- datos_completos %>% 
  filter(month(ymd(Date)) %in% c(9, 10)) %>% 
  select(Date, Affiliated_Code, Product_Code, Sales_Uds)

# Integración de las predicciones de ambos algoritmos en el dataset final
tabla_output_powerbi <- claves_test %>%
  mutate(
    Prediccion_Regresion = pred_lm,
    Prediccion_RandomForest = pred_rf
  )

# Exportación a formato CSV para su consumo automatizado en el modelo de Power BI
write_csv(tabla_output_powerbi, "Resultados_Prediccion_Demanda.csv")
