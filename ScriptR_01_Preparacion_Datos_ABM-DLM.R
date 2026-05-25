# Extracción, Transformación y Carga (ETL) y Generación de Datasets
# Objetivo: Depurar los datos transaccionales crudos de Altadis, integrar fuentes 
# exógenas (clima y festivos) y generar el modelo relacional base (Esquema en Estrella).

# Carga de librerías para la manipulación estructurada de datos y consumo de APIs
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
# Instalación y carga del paquete para conexión con la Agencia Estatal de Meteorología
# install.packages("climaemet") # Descomentar si es la primera ejecución
library(climaemet)

# 1. Configuración del entorno de trabajo
# Definición del directorio donde se alojan los datasets extraídos de los sistemas de la empresa
setwd("D:/Escritorio Windows BUENO/Escritorio/UNIR/TFM/Datasets internos Red Proyectum - Altadis/Data set Red Proyectum - Altadis")

# 2. Ingesta de datos corporativos (Altadis) y fuentes exógenas
# Carga de dimensiones maestras y tablas de hechos logísticos
df_Affiliated_Outlets <- read_delim("Affiliated_Outlets_enriquecido.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_DeliveryDay <- read_delim("DeliveryDay.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_OoSDay <- read_delim("OoSDay.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_Product <- read_delim("Product.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_RouteDay <- read_delim("RouteDay.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_SalesDay <- read_delim("SalesDay (1).csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Carga del calendario de festivos nacionales (Fuente externa)
df_2015_Holiday <- read_delim("2015_National_Holiday_Calendar.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Extracción automatizada de datos meteorológicos a través de la API de AEMET
# Configuración del token de acceso de desarrollador
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJkbG0uMjQwNTAzQGdtYWlsL
              mNvbSIsImp0aSI6ImI5M2Q4ODc2LWE0MWItNDg4My05NGYyLWU0YzFkYWU4YTNkZSIsImlzcyI6Ik
              FFTUVUIiwiaWF0IjoxNzc5MTMwNzc4LCJ1c2VySWQiOiJiOTNkODg3Ni1hNDFiLTQ4ODMtOTRmMi1lNGMx
              ZGFlOGEzZGUiLCJyb2xlIjoiIn0.2EyWWjZ-A8rakmiAOv1p_GbaMpm2IGVH1XEUD6pa6hY", install = TRUE)

# Descarga de la volumetría climática para la ventana temporal estricta del proyecto
df_clima_bruto <- aemet_daily_clim(start = "2015-03-09", end = "2015-10-04")

# 3. Proceso de depuración, transformación e integración (Pipeline ETL)

# 3.1. Normalización de tablas dimensionales (Maestros) y tratamiento de valores nulos
df_Outlets_Clean <- df_Affiliated_Outlets %>%
  mutate(
    # Normalización del formato de códigos postales a 5 dígitos (padding izquierdo)
    POSTALCODE = str_pad(as.character(POSTALCODE), width = 5, side = "left", pad = "0"),
    # Conversión de strings sin datos ("N.D.") a valores nulos computables (NA)
    Tam_m2 = na_if(Tam_m2, "N.D.") 
  ) %>%
  # Eliminación de duplicados para garantizar la integridad referencial de la dimensión
  distinct(Affiliated_Code, .keep_all = TRUE)

df_Product_Clean <- df_Product %>%
  distinct(Product_Code, .keep_all = TRUE)

# 3.2. Estandarización de formatos temporales
# Casting de vectores de caracteres a formato fecha unificado (YYYY-MM-DD)
df_SalesDay <- df_SalesDay %>% mutate(Sales_DAY = ymd(Sales_DAY))
df_DeliveryDay <- df_DeliveryDay %>% mutate(Delivery_DAY = ymd(Delivery_DAY))
df_OoSDay <- df_OoSDay %>% mutate(OoS_DAY = ymd(OoS_DAY))
df_RouteDay <- df_RouteDay %>% mutate(Route_DAY = ymd(Route_DAY))
df_2015_Holiday <- df_2015_Holiday %>% mutate(Date = ymd(Date))

# 3.3. Consolidación transaccional de hechos y corrección de anomalías numéricas
# Agrupación de ventas diarias y tratamiento de valores negativos (errores de caja)
df_Sales_Clean <- df_SalesDay %>%
  group_by(Sales_DAY, Affiliated_Code, Product_Code) %>%
  summarise(Sales_Uds = sum(Sales_Uds, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sales_Uds = ifelse(Sales_Uds < 0, 0, Sales_Uds))

# Agrupación de entregas logísticas
df_Delivery_Clean <- df_DeliveryDay %>%
  group_by(Delivery_DAY, Affiliated_Code, Product_Code) %>%
  summarise(Delivery_Uds = sum(Delivery_Uds, na.rm = TRUE), .groups = "drop") %>%
  mutate(Delivery_Uds = ifelse(Delivery_Uds < 0, 0, Delivery_Uds))

# Creación de variables binarias (Flags) para registrar incidencias y rutas
df_OoS_Clean <- df_OoSDay %>%
  distinct(OoS_DAY, Affiliated_Code, Product_Code) %>%
  mutate(OoS_Flag = 1)

df_Route_Clean <- df_RouteDay %>%
  distinct(Route_DAY, Affiliated_Code) %>%
  mutate(Route_Flag = 1)

# 3.4. Transformación y agregación de la dimensión meteorológica exógena
Dim_Weather <- df_clima_bruto %>%
  select(fecha, provincia, tmed, prec) %>%
  mutate(
    # Tratamiento de trazas de precipitación ("Ip") e internacionalización de decimales
    prec = ifelse(prec == "Ip", "0", prec),
    prec = as.numeric(str_replace(prec, ",", ".")),
    tmed = as.numeric(str_replace(tmed, ",", ".")),
    Date = ymd(fecha)
  ) %>%
  # Agregación para obtener la media climática diaria a nivel provincial
  group_by(Date, provincia) %>%
  summarise(
    Temp_Media = round(mean(tmed, na.rm = TRUE), 1),
    Precipitacion = round(mean(prec, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  # Normalización sintáctica: Capitalización estandarizada de provincias
  mutate(provincia = str_to_title(provincia))

# 3.5. Alineamiento temporal y generación de la Matriz Central de Hechos
# Definición del horizonte temporal del estudio
fecha_inicio <- ymd("2015-03-09")
fecha_fin <- ymd("2015-10-04")
calendario <- seq(fecha_inicio, fecha_fin, by = "days")

# Identificación de nodos activos en la red logística
combinaciones_activas <- df_Sales_Clean %>%
  distinct(Affiliated_Code, Product_Code)

# Generación del andamiaje temporal (Zero-Inflated Matrix) para balancear el panel de datos
df_Base_Panel <- expand_grid(Date = calendario, combinaciones_activas)

# Integración secuencial (Left Joins) de todos los flujos de información en una única macro-tabla
df_Master_Fact <- df_Base_Panel %>%
  left_join(df_Sales_Clean, by = c("Date" = "Sales_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_Delivery_Clean, by = c("Date" = "Delivery_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_OoS_Clean, by = c("Date" = "OoS_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_Route_Clean, by = c("Date" = "Route_DAY", "Affiliated_Code")) %>%
  mutate(
    # Imputación de ceros en los días sin registro (necesario para el entrenamiento de Machine Learning)
    Sales_Uds = replace_na(Sales_Uds, 0),
    Delivery_Uds = replace_na(Delivery_Uds, 0),
    OoS_Flag = replace_na(OoS_Flag, 0),
    Route_Flag = replace_na(Route_Flag, 0)
  ) %>%
  # Incorporación del calendario laboral
  left_join(df_2015_Holiday, by = "Date") %>%
  mutate(National_holiday = replace_na(National_holiday, 0))

# 4. Exportación del Modelo Relacional para Business Intelligence
# Generación de los archivos limpios y estructurados para alimentar Power BI y los algoritmos predictivos
write_csv(df_Outlets_Clean, "Dim_Affiliated_Outlets.csv")
write_csv(df_Product_Clean, "Dim_Product.csv")
write_csv(Dim_Weather, "Dim_Weather.csv")
write_csv(df_Master_Fact, "Fact_Operations.csv")
