## Inteligencia de negocio aplicada a Altadis: modelos analíticos para la predicción, 
############### segmentación y optimización de la distribución. ###################

# 1. Carga de librerías
library(readr)


# 2. Carga de datos de Altadis

# 2.1. Datos internos

# Affiliated_Outlets_enriquecido
df_Affiliated_Outlets <- read_delim("Affiliated_Outlets_enriquecido.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df_Affiliated_Outlets)

# DeliveryDay
df_DeliveryDay <- read_delim("DeliveryDay.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df_DeliveryDay)

# OoSDay
df_OoSDay <- read_delim("OoSDay.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)
View(df_OoSDay)

# Product
df_Product <- read_delim("Product.csv", delim = ";", 
                         escape_double = FALSE, trim_ws = TRUE)
View(df_Product)

# RouteDay
df_RouteDay <- read_delim("RouteDay.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df_RouteDay)

# SalesDay
df_SalesDay <- read_delim("SalesDay (1).csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df_SalesDay)

# 2.2. Datos externos

# 2015_National_Holiday_Calendar
df_2015_Holiday <- read_delim("2015_National_Holiday_Calendar.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df_2015_Holiday)

