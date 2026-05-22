# =================================================================================
# 1. CARGA DE LIBRERÍAS Y CONFIGURACIÓN INICIAL
# =================================================================================
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
install.packages("climaemet") 
library(climaemet)

# Configuración del directorio de trabajo (Ajustar ruta según el entorno local)
setwd("D:/Escritorio Windows BUENO/Escritorio/UNIR/TFM/Datasets internos Red Proyectum - Altadis/Data set Red Proyectum - Altadis")

# =================================================================================
# 2. CARGA DE DATOS (INTERNOS DE ALTADIS Y EXTERNOS)
# =================================================================================

# 2.1. Datos internos (Asegúrate de que el nombre del archivo es exactamente este)
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

# 2.2. Datos externos (Festivos Nacionales)
df_2015_Holiday <- read_delim("2015_National_Holiday_Calendar.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Datos externos (Meteorología AEMET)
# Configurar la API Key de AEMET
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJkbG0uMjQwNTAzQGdtYWlsL
              mNvbSIsImp0aSI6ImI5M2Q4ODc2LWE0MWItNDg4My05NGYyLWU0YzFkYWU4YTNkZSIsImlzcyI6Ik
              FFTUVUIiwiaWF0IjoxNzc5MTMwNzc4LCJ1c2VySWQiOiJiOTNkODg3Ni1hNDFiLTQ4ODMtOTRmMi1lNGMx
              ZGFlOGEzZGUiLCJyb2xlIjoiIn0.2EyWWjZ-A8rakmiAOv1p_GbaMpm2IGVH1XEUD6pa6hY", install = TRUE)

# Descargar datos diarios para la ventana temporal exacta del proyecto
df_clima_bruto <- aemet_daily_clim(start = "2015-03-09", end = "2015-10-04")

# =================================================================================
# 3. PROCESO DE DEPURACIÓN, TRANSFORMACIÓN E INTEGRACIÓN (ETL)
# =================================================================================

# 3.1. Normalización de tablas dimensionales (Maestros) y tratamiento de nulos
df_Outlets_Clean <- df_Affiliated_Outlets %>%
  mutate(
    # Normalizar códigos postales a 5 dígitos
    POSTALCODE = str_pad(as.character(POSTALCODE), width = 5, side = "left", pad = "0"),
    # Convertir "N.D." en nulos reales
    Tam_m2 = na_if(Tam_m2, "N.D.") 
  ) %>%
  distinct(Affiliated_Code, .keep_all = TRUE)

df_Product_Clean <- df_Product %>%
  distinct(Product_Code, .keep_all = TRUE)

# 3.2. Estandarización de formatos temporales
df_SalesDay <- df_SalesDay %>% mutate(Sales_DAY = ymd(Sales_DAY))
df_DeliveryDay <- df_DeliveryDay %>% mutate(Delivery_DAY = ymd(Delivery_DAY))
df_OoSDay <- df_OoSDay %>% mutate(OoS_DAY = ymd(OoS_DAY))
df_RouteDay <- df_RouteDay %>% mutate(Route_DAY = ymd(Route_DAY))
df_2015_Holiday <- df_2015_Holiday %>% mutate(Date = ymd(Date))

# 3.3. Consolidación transaccional de hechos y tratamiento de valores negativos
df_Sales_Clean <- df_SalesDay %>%
  group_by(Sales_DAY, Affiliated_Code, Product_Code) %>%
  summarise(Sales_Uds = sum(Sales_Uds, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sales_Uds = ifelse(Sales_Uds < 0, 0, Sales_Uds))

df_Delivery_Clean <- df_DeliveryDay %>%
  group_by(Delivery_DAY, Affiliated_Code, Product_Code) %>%
  summarise(Delivery_Uds = sum(Delivery_Uds, na.rm = TRUE), .groups = "drop") %>%
  mutate(Delivery_Uds = ifelse(Delivery_Uds < 0, 0, Delivery_Uds))

df_OoS_Clean <- df_OoSDay %>%
  distinct(OoS_DAY, Affiliated_Code, Product_Code) %>%
  mutate(OoS_Flag = 1)

df_Route_Clean <- df_RouteDay %>%
  distinct(Route_DAY, Affiliated_Code) %>%
  mutate(Route_Flag = 1)

#Transformación y agregación de la dimensión meteorológica (AEMET)
Dim_Weather <- df_clima_bruto %>%
  select(fecha, provincia, tmed, prec) %>%
  mutate(
    # Limpiar ceros y convertir comas en puntos
    prec = ifelse(prec == "Ip", "0", prec),
    prec = as.numeric(str_replace(prec, ",", ".")),
    tmed = as.numeric(str_replace(tmed, ",", ".")),
    Date = ymd(fecha)
  ) %>%
  # Agrupar por provincia y día
  group_by(Date, provincia) %>%
  summarise(
    Temp_Media = round(mean(tmed, na.rm = TRUE), 1),
    Precipitacion = round(mean(prec, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  # Asegurar que el nombre de la provincia empieza por mayúscula
  mutate(provincia = str_to_title(provincia))

# 3.5. Alineamiento temporal continuo e integración de fuentes exógenas en macro-tabla
fecha_inicio <- ymd("2015-03-09")
fecha_fin <- ymd("2015-10-04")
calendario <- seq(fecha_inicio, fecha_fin, by = "days")

combinaciones_activas <- df_Sales_Clean %>%
  distinct(Affiliated_Code, Product_Code)

# Andamiaje temporal (Zero-Inflated Matrix)
df_Base_Panel <- expand_grid(Date = calendario, combinaciones_activas)

df_Master_Fact <- df_Base_Panel %>%
  left_join(df_Sales_Clean, by = c("Date" = "Sales_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_Delivery_Clean, by = c("Date" = "Delivery_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_OoS_Clean, by = c("Date" = "OoS_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_Route_Clean, by = c("Date" = "Route_DAY", "Affiliated_Code")) %>%
  mutate(
    # Imputar ceros donde no hubo actividad
    Sales_Uds = replace_na(Sales_Uds, 0),
    Delivery_Uds = replace_na(Delivery_Uds, 0),
    OoS_Flag = replace_na(OoS_Flag, 0),
    Route_Flag = replace_na(Route_Flag, 0)
  ) %>%
  # Añadir el flag de festivo nacional
  left_join(df_2015_Holiday, by = "Date") %>%
  mutate(National_holiday = replace_na(National_holiday, 0))


# =================================================================================
# 4. EXPORTACIÓN DEL MODELO RELACIONAL PARA POWER BI Y MACHINE LEARNING
# =================================================================================

write_csv(df_Outlets_Clean, "Dim_Affiliated_Outlets.csv")
write_csv(df_Product_Clean, "Dim_Product.csv")
write_csv(Dim_Weather, "Dim_Weather.csv")
write_csv(df_Master_Fact, "Fact_Operations.csv")
