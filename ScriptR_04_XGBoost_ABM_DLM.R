# Clasificación Predictiva de Riesgo de Rotura (XGBoost)
# Objetivo: Predecir la probabilidad diaria de desabastecimiento en cada estanco 
# aplicando un modelo Extreme Gradient Boosting optimizado para clases desbalanceadas.

# Carga de librerías necesarias para la manipulación de datos y modelado
library(readr)
library(dplyr)
library(lubridate)
library(xgboost)
library(caret)
library(pROC)

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

# Integración de la dimensión de producto para extraer la categoría de formato (ej. ATA, FMC)
datos_preparados <- datos_preparados %>%
  left_join(dim_producto %>% select(Product_Code, Format), 
            by = "Product_Code")

# Consolidación final de la matriz analítica incorporando variables meteorológicas
datos_completos <- datos_preparados %>%
  left_join(dim_clima %>% select(Clave_Provincia_Fecha, Temp_Media, Precipitacion), 
            by = "Clave_Provincia_Fecha")

# Eliminación de registros con valores nulos para garantizar la estabilidad del algoritmo
datos_completos <- na.omit(datos_completos)

# 3. Preparación de variables operativas para la clasificación binaria
# Transformación de los tipos de datos y creación de la variable objetivo (Target)
datos_clasificacion <- datos_completos %>%
  mutate(
    Date = ymd(Date),
    Mes = as.numeric(month(Date)),      
    Dia_Semana = as.factor(wday(Date)),
    Route_Flag = as.factor(Route_Flag),
    National_holiday = as.factor(National_holiday),
    Format = as.factor(Format),
    # Binarización de la rotura de stock: 1 representa el evento anómalo (rotura), 0 operación normal
    Target_Rotura = ifelse(as.character(OoS_Flag) == "1" | as.character(OoS_Flag) == "Yes", 1, 0)
  )

# Selección del vector de predictores finales conservando las claves para la exportación posterior
datos_clasificacion <- datos_clasificacion %>% select(
  Target_Rotura, Mes, Dia_Semana, Delivery_Uds, Route_Flag, 
  National_holiday, Temp_Media, Precipitacion, Format,
  Affiliated_Code, Date
)

# 4. Partición temporal del conjunto de datos (Entrenamiento y Validación)
# División estricta por meses para evitar fuga de información, replicando la estructura del Random Forest
# Entrenamiento: meses de primavera y verano
train_data <- datos_clasificacion %>% filter(Mes %in% c(3, 4, 5, 6, 7, 8)) %>% select(-Mes)
# Validación (Test): meses de otoño
test_data  <- datos_clasificacion %>% filter(Mes %in% c(9, 10)) %>% select(-Mes)

# Separación de los identificadores (claves) de las variables predictoras para evitar sesgos en el aprendizaje
train_id <- train_data %>% select(Affiliated_Code, Date, Target_Rotura)
train_x  <- train_data %>% select(-Affiliated_Code, -Date, -Target_Rotura)

test_id  <- test_data %>% select(Affiliated_Code, Date, Target_Rotura)
test_x   <- test_data %>% select(-Affiliated_Code, -Date, -Target_Rotura)

# 5. Transformación matricial mediante One-Hot Encoding
# XGBoost requiere matrices numéricas puras. Se dummifican las variables categóricas
dummies <- dummyVars(" ~ .", data = train_x, fullRank = TRUE)
X_train_mat <- predict(dummies, newdata = train_x)
X_test_mat  <- predict(dummies, newdata = test_x)

# Aislamiento del vector objetivo (Target)
y_train_vec <- train_id$Target_Rotura
y_test_vec  <- test_id$Target_Rotura

#Entrenamiento del modelo XGBoost y compensación del desbalanceo
# Cálculo del ratio de desbalanceo para penalizar severamente los falsos negativos de la clase minoritaria
ratio_desbalanceo <- sum(y_train_vec == 0) / sum(y_train_vec == 1)

# Configuración de los hiperparámetros del algoritmo secuencial
parametros_xgb <- list(
  objective = "binary:logistic",  # Configuración para clasificación binaria
  eval_metric = "auc",            # Optimización basada en el Área Bajo la Curva ROC
  scale_pos_weight = ratio_desbalanceo, # Aplicación del peso correctivo
  max_depth = 6,                  # Límite de profundidad para prevenir el sobreajuste
  eta = 0.1                       # Tasa de aprendizaje
)

# Ejecución del entrenamiento del algoritmo
modelo_xgb <- xgboost(
  data = X_train_mat,
  label = y_train_vec,
  params = parametros_xgb,
  nrounds = 100,      
  verbose = 0         
)

# Evaluación de rendimiento y optimización de umbrales
# Predicción de probabilidades continuas de riesgo sobre el conjunto de test
probabilidades_riesgo <- predict(modelo_xgb, newdata = X_test_mat)

# Cálculo del Índice de Youden para determinar el umbral óptimo de decisión matemática
# Esto sustituye el umbral por defecto del 50% por el punto de corte ideal frente al desbalanceo
curva_roc <- roc(y_test_vec, probabilidades_riesgo)
umbral_optimo <- coords(curva_roc, "best", ret="threshold", best.method="youden")[[1]]

# Selección del primer valor en caso de empate algorítmico
umbral_optimo <- umbral_optimo[1] 

# Binarización de las predicciones aplicando el umbral optimizado
predicciones_binarias <- ifelse(probabilidades_riesgo > umbral_optimo, 1, 0)

# Generación de la Matriz de Confusión exigiendo explícitamente la clase 1 como evento de éxito
matriz_confusion <- confusionMatrix(
  as.factor(predicciones_binarias), 
  as.factor(y_test_vec),
  positive = "1"
)

# Impresión en consola de las métricas de negocio y evaluación
cat("\nMÉTRICAS DE RENDIMIENTO OPTIMIZADAS XGBOOST\n")
cat("Umbral Matemático Óptimo Aplicado (Youden):", round(umbral_optimo, 4), "\n\n")
print(matriz_confusion$table)
cat("\nÁrea Bajo la Curva (AUC):", round(auc(curva_roc), 4), "\n")
cat("Sensibilidad Real (Recall clase 1):", round(matriz_confusion$byClass["Sensitivity"], 4), "\n")

# Visualización de la jerarquía de importancia predictiva de las variables (Feature Importance)
importancia_variables <- xgb.importance(feature_names = colnames(X_train_mat), model = modelo_xgb)
xgb.plot.importance(importancia_variables[1:10, ], 
                    main = "Importancia Predictiva de Variables (Riesgo Rotura)",
                    col = "firebrick")

# 8. Exportación de resultados para integración en Inteligencia de Negocio
# Reconstrucción de la tabla de hechos cruzando las claves logísticas con las probabilidades predictivas
tabla_output_powerbi_xgb <- data.frame(
  Date = test_id$Date,
  Affiliated_Code = test_id$Affiliated_Code,
  Rotura_Real = test_id$Target_Rotura,
  Probabilidad_Riesgo_XGBoost = probabilidades_riesgo
)

# Exportación a formato CSV para su consumo automatizado en el modelo en estrella de Power BI
write_csv(tabla_output_powerbi_xgb, "Resultados_Prediccion_Clasificacion_XGBoost.csv")

