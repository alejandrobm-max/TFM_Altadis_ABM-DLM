# Cargar librerías necesarias
library(readr)
library(dplyr)
library(lubridate)
library(randomForest)
library(caret)
library(ranger)

# 1. INGESTA DE DATOS
# Configuración del directorio de trabajo y carga de la tabla de hechos y dimensiones
setwd("D:/Escritorio Windows BUENO/Escritorio/UNIR/TFM/Datasets internos Red Proyectum - Altadis/Data set Red Proyectum - Altadis")

fact_operations <- read_csv("Fact_Operations.csv")
dim_estancos <- read_csv("Dim_Affiliated_Outlets.csv") 
dim_clima <- read_csv("Dim_Weather.csv")                
dim_producto <- read_csv("Dim_Product.csv")            

# TRANSFORMACIÓN DE DATOS Y REPLICACIÓN DE LA LÓGICA DE NEGOCIO
# Integración de la dimensión geográfica y normalización de nomenclaturas territoriales
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

# Generación de clave primaria compuesta en la dimensión climática
dim_clima <- dim_clima %>%
  mutate(Clave_Provincia_Fecha = paste0(provincia, "_", as.character(Date)))

# Integración de la dimensión de producto
datos_preparados <- datos_preparados %>%
  left_join(dim_producto %>% select(Product_Code, Format), 
            by = "Product_Code")

# CONSOLIDACIÓN DE LA MATRIZ ANALÍTICA
# Cruce final con variables meteorológicas y tratamiento de valores nulos
datos_completos <- datos_preparados %>%
  left_join(dim_clima %>% select(Clave_Provincia_Fecha, Temp_Media, Precipitacion), 
            by = "Clave_Provincia_Fecha")

datos_completos <- na.omit(datos_completos)

# 4. PREPARACIÓN DE VARIABLES Y SELECCIÓN DE PREDICTORES
# Tipificado de variables categóricas y temporales para el modelado algorítmico
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

datos_demanda <- datos_demanda %>% select(
  Sales_Uds, Mes, Dia_Semana, Delivery_Uds, OoS_Flag, Route_Flag, 
  National_holiday, Temp_Media, Precipitacion, Format
)

# 5. PARTICIÓN TEMPORAL DEL CONJUNTO DE DATOS
# Entrenamiento (primavera-verano) y validación (otoño)
train <- datos_demanda %>% filter(Mes %in% c(3, 4, 5, 6, 7, 8)) %>% select(-Mes)
test  <- datos_demanda %>% filter(Mes %in% c(9, 10)) %>% select(-Mes)

# 6. ENTRENAMIENTO DE MODELOS PREDICTIVOS
# Definición del modelo base (Regresión lineal múltiple)
modelo_lm <- lm(Sales_Uds ~ ., data = train)
pred_lm <- predict(modelo_lm, newdata = test)

# Liberación de memoria RAM
gc() 

# Entrenamiento del modelo principal (Random Forest vía librería optimizada ranger)
modelo_rf <- ranger(
  formula = Sales_Uds ~ ., 
  data = train, 
  num.trees = 50, 
  importance = 'impurity' 
)

pred_rf <- predict(modelo_rf, data = test)$predictions

# 7. EVALUACIÓN DE RENDIMIENTO Y EXTRACCIÓN DE MÉTRICAS
# Cálculo de errores absolutos y cuadráticos (MAE, RMSE, R2)
metricas_lm <- postResample(pred = pred_lm, obs = test$Sales_Uds)
metricas_rf <- postResample(pred = pred_rf, obs = test$Sales_Uds)

print(metricas_lm)
print(metricas_rf)

# Visualización de la relevancia de cada variable en la predicción
importancia <- modelo_rf$variable.importance
barplot(sort(importancia, decreasing = TRUE), 
        main = "Importancia Predictiva de las Variables", 
        las = 2, 
        col = "steelblue", 
        cex.names = 0.65)

# 8. EXPORTACIÓN DE RESULTADOS PARA INTEGRACIÓN EN BUSINESS INTELLIGENCE
# Reconstrucción de la trazabilidad logística y exportación de las predicciones
claves_test <- datos_completos %>% 
  filter(month(ymd(Date)) %in% c(9, 10)) %>% 
  select(Date, Affiliated_Code, Product_Code, Sales_Uds)

tabla_output_powerbi <- claves_test %>%
  mutate(
    Prediccion_Regresion = pred_lm,
    Prediccion_RandomForest = pred_rf
  )

write_csv(tabla_output_powerbi, "Resultados_Prediccion_Demanda.csv")
