## Inteligencia de negocio aplicada a Altadis: modelos analíticos para la predicción, ##
 ################ segmentación y optimización de la distribución. #####################

# =================================================================================
# 1. CARGA DE LIBRERÍAS
# =================================================================================
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# Configuración del directorio de trabajo (Ajustar ruta según el entorno local)
setwd("D:/Escritorio Windows BUENO/Escritorio/UNIR/TFM/Datasets internos Red Proyectum - Altadis/Data set Red Proyectum - Altadis")


# =================================================================================
# 2. CARGA DE DATOS DE ALTADIS
# =================================================================================

# 2.1. Datos internos
df_Affiliated_Outlets <- read_delim("Affiliated_Outlets_enriquezido.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_DeliveryDay <- read_delim("DeliveryDay.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_OoSDay <- read_delim("OoSDay.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

df_Product <- read_delim("Product.csv", delim = ";", 
                         escape_double = FALSE, trim_ws = TRUE)

df_RouteDay <- read_delim("RouteDay.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_SalesDay <- read_delim("SalesDay (1).csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

# 2.2. Datos externos
df_2015_Holiday <- read_delim("2015_National_Holiday_Calendar.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)


# =================================================================================
# 3. PROCESO DE DEPURACIÓN, TRANSFORMACIÓN E INTEGRACIÓN (ETL)
# =================================================================================

# 3.1. Normalización de tablas dimensionales (Maestros) y tratamiento de nulos
df_Outlets_Clean <- df_Affiliated_Outlets %>%
  mutate(
    POSTALCODE = str_pad(as.character(POSTALCODE), width = 5, side = "left", pad = "0"),
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


# 3.4. Alineamiento temporal continuo e integración de fuentes exógenas
fecha_inicio <- ymd("2015-03-09")
fecha_fin <- ymd("2015-10-04")
calendario <- seq(fecha_inicio, fecha_fin, by = "days")

combinaciones_activas <- df_Sales_Clean %>%
  distinct(Affiliated_Code, Product_Code)

df_Base_Panel <- expand_grid(Date = calendario, combinaciones_activas)

df_Master_Fact <- df_Base_Panel %>%
  left_join(df_Sales_Clean, by = c("Date" = "Sales_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_Delivery_Clean, by = c("Date" = "Delivery_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_OoS_Clean, by = c("Date" = "OoS_DAY", "Affiliated_Code", "Product_Code")) %>%
  left_join(df_Route_Clean, by = c("Date" = "Route_DAY", "Affiliated_Code")) %>%
  mutate(
    Sales_Uds = replace_na(Sales_Uds, 0),
    Delivery_Uds = replace_na(Delivery_Uds, 0),
    OoS_Flag = replace_na(OoS_Flag, 0),
    Route_Flag = replace_na(Route_Flag, 0)
  ) %>%
  left_join(df_2015_Holiday, by = "Date") %>%
  mutate(National_holiday = replace_na(National_holiday, 0))


# =================================================================================
# 4. EXPORTACIÓN DEL MODELO RELACIONAL PARA INTELIGENCIA DE NEGOCIO Y ML
# =================================================================================

write_csv(df_Outlets_Clean, "Dim_Affiliated_Outlets.csv")
write_csv(df_Product_Clean, "Dim_Product.csv")
write_csv(df_Master_Fact, "Fact_Operations.csv")
